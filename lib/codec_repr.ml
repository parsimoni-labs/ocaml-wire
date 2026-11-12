open Types
open Codec_leaf

let int_of_typ_value = Eval.int_of_exn

type slots = { ints : int array; int64s : int64 array }
type runtime = Types.eval_ctx

let alloc_slots n = { ints = Array.make n 0; int64s = Array.make n 0L }

(* A loop rather than [Array.fill]: a scratch holds a slot per field, and for
   that few the C call costs more than the stores. The [int64s] half is
   written only by a field with a full-width slot ([Types.has_int64_slot]), so
   a record without one has nothing there to clear. *)
let clear_ints s n =
  for i = 0 to n - 1 do
    s.ints.(i) <- 0
  done

let clear_slots ?(int64s = true) s n =
  clear_ints s n;
  if int64s then
    for i = 0 to n - 1 do
      s.int64s.(i) <- 0L
    done

(* Grow [table] until it has a cell for domain index [i]. The new array reuses
   the existing [Atomic.t] cells rather than their contents, so a domain writing
   its scratch through the array it read races with nobody: both arrays name the
   same cell. Losing the CAS means another domain grew it first, so retry and
   take theirs. *)
let rec slots_cells table i =
  let a = Atomic.get table in
  let len = Array.length a in
  if i < len then a
  else
    let b =
      Array.init
        (max (i + 1) (max 8 (2 * len)))
        (fun j -> if j < len then a.(j) else Atomic.make None)
    in
    if Atomic.compare_and_set table a b then b else slots_cells table i

(* A per-domain [slots] scratch. The validators and decoders below reuse one
   scratch across calls to stay allocation-free, but a codec value is shared
   across domains, so a single shared scratch lets two domains decoding at once
   overwrite each other's fields. A misparsed segment is then dropped, which in
   a TCP stack pinned one flow per core stalls that flow. Per-domain scratch
   keeps the reuse and drops the sharing: a decode performs no effects, so
   cooperative fibers never interleave mid-decode. Preemptive systhreads sharing
   a domain must still not decode one codec at once.

   The scratch hangs off the validator and is indexed by [Domain.self_index ()],
   rather than living in [Domain.DLS] under a key minted here. A DLS key is
   drawn from a global counter and a domain sizes its DLS array from that
   counter, so one key per compiled validator made the first DLS read on a fresh
   domain cost a word per validator the program ever built, charged even to
   domains that never decode. [self_index] is dense over live domains instead,
   so the array here is bounded by the domains actually running.

   An index is reused once its domain terminates, handing the new occupant the
   old scratch. That is sound because every caller clears the scratch on entry,
   and two live domains never share an index. *)
let domain_local_slots n =
  let table = Atomic.make [||] in
  fun () ->
    let i = Domain.self_index () in
    let cell = (slots_cells table i).(i) in
    match Atomic.get cell with
    | Some s -> s
    | None ->
        let s = alloc_slots n in
        Atomic.set cell (Some s);
        s

let set_int_slot s idx v = s.ints.(idx) <- v
let set_int64_slot s idx v = s.int64s.(idx) <- v

(* The absolute offset of a field that failed: the record's [base] plus the
   field's own offset inside it, or [base] alone when the layout makes that
   offset dynamic ([-1]) and there is nothing better to say. [Types.parse_error]
   promises [at] is "the absolute byte offset of the failing field", so a caller
   that locates the field from [at] reads or rewrites the wrong bytes for every
   field but the first if the record's base is reported instead. *)
let at_of base field_off = if field_off >= 0 then base + field_off else base

(* Raise a field-constraint failure, reporting the offending field's value so a
   demux can route on it (e.g. a version field that failed [self = N]).
   [field_idx] is the field's int slot; [at] is the field's byte offset. *)
let raise_field_constraint ~field_idx ~at arr =
  let value =
    if field_idx >= 0 && field_idx < Array.length arr.ints then
      Some (Int64.of_int arr.ints.(field_idx))
    else None
  in
  raise_constraint ~at ~which:Field ?value ()

(* Build a populate function that writes a field value into the validator
   array without allocating. Resolves the type at seal time so the hot
   path is a direct arr.(idx) <- reader buf base. *)
(* Like [Uint64] in [build_populate], on a narrow-int platform the int slot
   only mirrors values the int can hold; a 64-bit host keeps the direct
   store. *)
let populate_uint32 idx reader =
  if Sys.int_size > 32 then fun slots runtime input_end buf base ->
    set_int_slot slots idx (UInt32.to_int (reader runtime input_end buf base))
  else fun slots runtime input_end buf base ->
    Option.iter (set_int_slot slots idx)
      (Int32.unsigned_to_int
         (UInt32.to_int32 (reader runtime input_end buf base)))

(* Same shape as [populate_uint32] on the signed side: the slot is a native
   [int], so where that int is narrower than the field only the values it can
   hold are mirrored, and a constraint over the rest reads an unpopulated zero
   rather than a truncated number. *)
let populate_sint32 idx reader =
  if Sys.int_size > 32 then fun slots runtime input_end buf base ->
    set_int_slot slots idx (SInt32.to_int (reader runtime input_end buf base))
  else fun slots runtime input_end buf base ->
    Option.iter (set_int_slot slots idx)
      (SInt32.to_int_opt (reader runtime input_end buf base))

let populate_uint_var idx reader =
  if Sys.int_size >= 63 then fun slots runtime input_end buf base ->
    set_int_slot slots idx (UInt63.to_int (reader runtime input_end buf base))
  else fun slots runtime input_end buf base ->
    Option.iter (set_int_slot slots idx)
      (Int64.unsigned_to_int
         (Optint.Int63.to_int64 (reader runtime input_end buf base)))

let populate_int idx reader slots runtime input_end buf base =
  set_int_slot slots idx (reader runtime input_end buf base)

(* [SInt8.t], [SInt16.t], [UInt8.t] and [UInt16.t] are native [int]s behind a
   range, so the slot takes the decoded value unchanged and no platform has a
   value it cannot hold. *)
let populate_sint8 idx reader slots runtime input_end buf base =
  set_int_slot slots idx (SInt8.to_int (reader runtime input_end buf base))

let populate_sint16 idx reader slots runtime input_end buf base =
  set_int_slot slots idx (SInt16.to_int (reader runtime input_end buf base))

let populate_uint8 idx reader slots runtime input_end buf base =
  set_int_slot slots idx (UInt8.to_int (reader runtime input_end buf base))

let populate_uint16 idx reader slots runtime input_end buf base =
  set_int_slot slots idx (UInt16.to_int (reader runtime input_end buf base))

(* Floats store their IEEE 754 bit pattern in the int slot so [Ref name] in a
   constraint sees the value the 3D side sees; a float64 also fills the int64
   slot with the full pattern, which its predicates read (the int slot cannot
   hold bits 52-62 on a narrow-int platform). The user-facing reader still
   returns the [float], reinterpreted via [Int*.float_of_bits] elsewhere. *)
let populate_float32 idx read slots _runtime _input_end buf base =
  set_int_slot slots idx (Int32.to_int (read buf base))

let populate_float64 idx read slots _runtime _input_end buf base =
  let bits = read buf base in
  set_int64_slot slots idx bits;
  set_int_slot slots idx (Int64.to_int bits)

let rec build_populate : type a.
    a typ ->
    int ->
    (runtime -> Input_end.t -> bytes -> int -> a) ->
    slots ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    unit =
 fun typ idx reader ->
  match typ with
  | Uint8 -> populate_uint8 idx reader
  | Uint16 _ -> populate_uint16 idx reader
  | Int8 -> populate_sint8 idx reader
  | Int16 _ -> populate_sint16 idx reader
  | Int32 _ -> populate_sint32 idx reader
  | Bits _ -> populate_int idx reader
  | Uint_var _ -> populate_uint_var idx reader
  | Uint32 _ -> populate_uint32 idx reader
  (* [Uint64] / [Int64] populate both slots: the full value in [int64s] for
     full-width [Field.int64] refs, and the truncated value in [ints] so legacy
     int-kind [Field.ref] / [Field.int] constraints keep their pre-int64
     behavior instead of silently reading an unpopulated zero. ([has_int64_slot]
     gates the int64 ref API to exactly these types.) *)
  | Uint64 _ ->
      fun slots runtime input_end buf base ->
        let v = reader runtime input_end buf base in
        set_int64_slot slots idx (UInt64.to_int64 v);
        Option.iter (set_int_slot slots idx) (UInt64.to_int_opt v)
  | Int64 _ ->
      fun slots runtime input_end buf base ->
        let v = reader runtime input_end buf base in
        set_int64_slot slots idx v;
        set_int_slot slots idx (Int64.to_int v)
  | Float32 Little -> populate_float32 idx Bytes.get_int32_le
  | Float32 Big -> populate_float32 idx Bytes.get_int32_be
  | Float64 Little -> populate_float64 idx Bytes.get_int64_le
  | Float64 Big -> populate_float64 idx Bytes.get_int64_be
  | Where { inner; _ } -> build_populate inner idx reader
  | Enum { base; cases; closed; _ } ->
      let check =
        enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
      in
      build_populate base idx (fun runtime input_end buf b ->
          check ~at:b (reader runtime input_end buf b))
  | Map { inner; encode; _ } ->
      build_populate inner idx (fun runtime input_end buf base ->
          encode (reader runtime input_end buf base))
  | Optional_or { inner; _ } ->
      (* [Optional_or] returns the inner type directly (no [option] wrapper),
         so the reader is already of the inner's value type. *)
      build_populate inner idx reader
  | Optional { inner; _ } -> (
      (* The optional decodes as ['inner option]; populate from the inner
         value when present, leave the slot at 0 when absent. *)
      let inner_populate =
        build_populate inner idx (fun runtime input_end buf base ->
            match reader runtime input_end buf base with
            | Some v -> v
            | None -> assert false)
      in
      fun slots runtime input_end buf base ->
        match reader runtime input_end buf base with
        | Some _ -> inner_populate slots runtime input_end buf base
        | None -> ())
  | Byte_array _ | Byte_slice _ | All_bytes | Unit ->
      (* A plain byte span (or unit) carries no constrained data, so its reader
         only re-slices the buffer. Running it for effect would allocate a string
         or slice per validate with nothing to check; skip it so validating a
         span-only codec stays allocation-free on a hot read path. *)
      fun _slots _runtime _input_end _buf _base -> ()
  | _ ->
      (* No int slot to populate, but the reader performs a real decode-side
         check (an embedded codec, an array or repeat element's constraints), so
         run it for effect: [Codec.validate] then rejects exactly what [decode]
         does. The value is discarded. Span types whose reader scans the payload
         do not reach here: [span_populate] gives them the scan without the
         copy, since what a validator drops on the floor must not be sized by
         the sender. *)
      fun _slots runtime input_end buf base ->
        ignore (reader runtime input_end buf base)

(* Bitfield extraction descriptor: word reader + packed shift/mask.
   Packing shift and mask into a single int lets [extract] be a direct
   [@inline always] function instead of an indirect closure call. *)
(* The word travels as [Optint.t] so a 32-bit base word survives a narrow-int
   platform: an unboxed native [int] on a 64-bit host, a boxed [int32] where
   the int cannot hold the word. *)
type bf_info = {
  word_reader : bytes -> int -> Optint.t;
  bf_shift : int;
  bf_mask : Optint.t;
  bf_mask_int : int;
}

let no_runtime = Types.unbound_eval_ctx

(* The context of a read after the check pass: a sub-codec read through it
   builds its record and leaves the checks to the pass that ran them. *)
let checked_runtime = Types.checked_eval_ctx no_runtime

let runtime ?env () =
  match env with
  | None -> no_runtime
  | Some env ->
      let rec find name i =
        if i >= Array.length env.Types.names then -1
        else if env.names.(i) = name then i
        else find name (i + 1)
      in
      Types.eval_ctx
        ~set_param:(fun name value ->
          let i = find name 0 in
          if i >= 0 then env.slots.(i) <- value)
        (fun name ->
          let i = find name 0 in
          if i < 0 then 0 else env.slots.(i))

type field_access =
  | Fixed of int
  | Bitfield of {
      base : bitfield_base;
      byte_off : int;
      shift : int;
      width : int;
    }
  | Dynamic of (runtime -> Input_end.t -> bytes -> int -> int)
  | Variable of {
      off : int;
      size_fn : runtime -> Input_end.t -> bytes -> int -> int;
    }
  | Variable_dynamic of {
      off_fn : runtime -> Input_end.t -> bytes -> int -> int;
      size_fn : runtime -> Input_end.t -> bytes -> int -> int;
    }

(* Absolute offset of the bytes a field occupies, from the record's base. A
   staged reader that rejects the value it read reports it here, so that [at]
   names the field and not the record; a bitfield names the base word it is
   packed into, the smallest span its value can be located in. *)
let access_at : field_access -> runtime -> Input_end.t -> bytes -> int -> int =
  function
  | Fixed off | Variable { off; _ } ->
      fun _runtime _input_end _buf base -> base + off
  | Bitfield { byte_off; _ } ->
      fun _runtime _input_end _buf base -> base + byte_off
  | Dynamic off_fn | Variable_dynamic { off_fn; _ } ->
      fun runtime input_end buf base -> base + off_fn runtime input_end buf base

(* [access_at] for the environment-free readers, which never see an offset that
   depends on the buffer: [build_immediate_staged_reader] hands back [None] for
   a variable or dynamic access rather than a reader, so only the two static
   shapes reach a caller of this. *)
let immediate_access_off = function
  | Fixed off | Variable { off; _ } -> off
  | Bitfield { byte_off; _ } -> byte_off
  | Dynamic _ | Variable_dynamic _ -> 0

type ('a, 'r) field = {
  id : int;
  name : string;
  typ : 'a typ;
  constraint_ : bool expr option;
  action : action option;
  doc : string option;
  get : 'r -> 'a;
}

let combine_constraint a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some a, Some b -> Some Expr.(a && b)

(* Capture top-level names before field/struct_ are shadowed.
   Extract Where constraints so that Codec.to_struct produces correct 3D. *)
let struct_field : type a r. (a, r) field -> Types.field =
 fun fld ->
  match fld.typ with
  | Where { cond; inner } ->
      field fld.name
        ?constraint_:(combine_constraint fld.constraint_ (Some cond))
        ?action:fld.action ?doc:fld.doc inner
  | _ ->
      field fld.name ?constraint_:fld.constraint_ ?action:fld.action
        ?doc:fld.doc fld.typ

let struct' = struct_

(* GADT snoc-list of typed field readers, built in forward order by add_field.
   ('full, 'remaining) readers tracks:
   - 'full:      the original constructor type
   - 'remaining: what's left after consuming the readers in this list

   Snoc appends at the end, so readers are in field order.
   At seal time, pattern-matching reconstructs the full application
   without partial application closures (for up to 6 fields). *)
type (_, _) readers =
  | Nil : ('f, 'f) readers
  | Snoc :
      ('full, 'a -> 'rest) readers * 'a field_read
      -> ('full, 'rest) readers

(* How [decode] reads one field. A scalar at a fixed offset is data, read by
   [read_scalar]; any other field is a closure, which costs an indirect call
   through [caml_applyN]. The offsets are relative to the record and inside the
   span [decode] has already bounds-checked, so the reads skip their own check. *)
and _ field_read =
  | Data : 'a scalar -> 'a field_read
  | Mapped : 'w scalar * ('w -> 'a) -> 'a field_read
      (** A scalar under a total [map]: read, then decoded. *)
  | Span : int * int -> string field_read
      (** A fixed byte array: its offset and length. *)
  | Fn : (runtime -> Input_end.t -> bytes -> int -> 'a) -> 'a field_read

and _ scalar =
  | Byte : int -> UInt8.t scalar
  | U16le : int -> UInt16.t scalar
  | U16be : int -> UInt16.t scalar
  | U32le : int -> UInt32.t scalar
  | U32be : int -> UInt32.t scalar
  | Sbyte : int -> SInt8.t scalar
  | I16le : int -> SInt16.t scalar
  | I16be : int -> SInt16.t scalar
  | I32le : int -> SInt32.t scalar
  | I32be : int -> SInt32.t scalar
  | Bits8 : { off : int; shift : int; mask : int } -> int scalar
  | Bits16le : { off : int; shift : int; mask : int } -> int scalar
  | Bits16be : { off : int; shift : int; mask : int } -> int scalar

(* Calls nothing, so it runs without a stack frame. *)
let read_scalar : type a. a scalar -> bytes -> int -> a =
 fun r buf base ->
  match r with
  | Byte off -> UInt8.unsafe_get buf (base + off)
  | U16le off -> UInt16.unsafe_le buf (base + off)
  | U16be off -> UInt16.unsafe_be buf (base + off)
  | U32le off -> UInt32.unsafe_le buf (base + off)
  | U32be off -> UInt32.unsafe_be buf (base + off)
  | Sbyte off -> SInt8.unsafe_get buf (base + off)
  | I16le off -> SInt16.unsafe_le buf (base + off)
  | I16be off -> SInt16.unsafe_be buf (base + off)
  | I32le off -> SInt32.unsafe_le buf (base + off)
  | I32be off -> SInt32.unsafe_be buf (base + off)
  | Bits8 { off; shift; mask } ->
      (Unchecked.u8 buf (base + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      (Unchecked.u16le buf (base + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      (Unchecked.u16be buf (base + off) lsr shift) land mask

(* The int a scalar's validation slot holds, for the scalars whose value an
   int holds exactly. *)
let slot_scalar : type a. a scalar -> bool = function
  | U32le _ | U32be _ | I32le _ | I32be _ -> Sys.int_size > 32
  | _ -> true

(* Calls nothing, like [read_scalar]. *)
let read_slot : type a. a scalar -> bytes -> int -> int =
 fun r buf base ->
  match r with
  | Byte off -> UInt8.to_int (UInt8.unsafe_get buf (base + off))
  | U16le off -> UInt16.to_int (UInt16.unsafe_le buf (base + off))
  | U16be off -> UInt16.to_int (UInt16.unsafe_be buf (base + off))
  | U32le off -> UInt32.to_int (UInt32.unsafe_le buf (base + off))
  | U32be off -> UInt32.to_int (UInt32.unsafe_be buf (base + off))
  | Sbyte off -> SInt8.to_int (SInt8.unsafe_get buf (base + off))
  | I16le off -> SInt16.to_int (SInt16.unsafe_le buf (base + off))
  | I16be off -> SInt16.to_int (SInt16.unsafe_be buf (base + off))
  | I32le off -> SInt32.to_int (SInt32.unsafe_le buf (base + off))
  | I32be off -> SInt32.to_int (SInt32.unsafe_be buf (base + off))
  | Bits8 { off; shift; mask } ->
      (Unchecked.u8 buf (base + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      (Unchecked.u16le buf (base + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      (Unchecked.u16be buf (base + off) lsr shift) land mask

let[@inline] read_field : type a.
    a field_read -> runtime -> Input_end.t -> bytes -> int -> a =
 fun r runtime input_end buf base ->
  match r with
  | Data s -> read_scalar s buf base
  | Mapped (s, decode) -> decode (read_scalar s buf base)
  | Span (off, n) -> Bytes.sub_string buf (base + off) n
  | Fn f -> f runtime input_end buf base

(* How [encode] writes a field it has a descriptor for: the field's value, put
   at the descriptor's offset with no bounds check of its own. *)
type 'r put =
  | Put : { get : 'r -> 'a; scalar : 'a scalar } -> 'r put
  | Blit : { get : 'r -> string; at : int; len : int } -> 'r put
      (** A fixed byte array: copied in after a check of its length. *)

let bits_of_mask mask =
  let rec go n = if mask lsr n = 0 then n else go (n + 1) in
  go 0

(* A bitfield value wider than its field, off the hot path. *)
let bits_too_wide ~mask v =
  Types.check_unsigned_encode ~bits:(bits_of_mask mask) v

(* Calls nothing on the normal path. A bitfield is or-ed into its word, which
   the encoder has zeroed first, as the general writer does. *)
let write_scalar : type a. a scalar -> bytes -> int -> a -> unit =
 fun r buf base v ->
  match r with
  | Byte off -> Unchecked.set_u8 buf (base + off) (UInt8.to_int v)
  | U16le off -> Unchecked.set_u16le buf (base + off) (UInt16.to_int v)
  | U16be off -> Unchecked.set_u16be buf (base + off) (UInt16.to_int v)
  | U32le off ->
      Unchecked.set_i32le buf (base + off) (Int32.of_int (UInt32.to_int v))
  | U32be off ->
      Unchecked.set_i32be buf (base + off) (Int32.of_int (UInt32.to_int v))
  | Sbyte off -> Unchecked.set_u8 buf (base + off) (SInt8.to_int v)
  | I16le off -> Unchecked.set_u16le buf (base + off) (SInt16.to_int v)
  | I16be off -> Unchecked.set_u16be buf (base + off) (SInt16.to_int v)
  | I32le off ->
      Unchecked.set_i32le buf (base + off) (Int32.of_int (SInt32.to_int v))
  | I32be off ->
      Unchecked.set_i32be buf (base + off) (Int32.of_int (SInt32.to_int v))
  | Bits8 { off; shift; mask } ->
      if v < 0 || v > mask then bits_too_wide ~mask v;
      let p = base + off in
      Unchecked.set_u8 buf p (Unchecked.u8 buf p lor (v lsl shift))
  | Bits16le { off; shift; mask } ->
      if v < 0 || v > mask then bits_too_wide ~mask v;
      let p = base + off in
      Unchecked.set_u16le buf p (Unchecked.u16le buf p lor (v lsl shift))
  | Bits16be { off; shift; mask } ->
      if v < 0 || v > mask then bits_too_wide ~mask v;
      let p = base + off in
      Unchecked.set_u16be buf p (Unchecked.u16be buf p lor (v lsl shift))

(* The scalars whose value an int holds exactly, so a write converts it with no
   loss. *)
let int_scalar : type a. a scalar -> bool = function
  | U32le _ | U32be _ | I32le _ | I32be _ -> Sys.int_size > 32
  | _ -> true

(* The data form of a field read, when the field is a bare scalar at a fixed
   offset or packed in an 8- or 16-bit word. *)
let field_scalar : type a.
    min_wire_size:int -> field_access -> a typ -> a scalar option =
 fun ~min_wire_size access typ ->
  match (access, typ) with
  | (Fixed off | Bitfield { byte_off = off; _ }), _
    when off < 0
         ||
         match field_wire_size typ with
         | Some w -> off + w > min_wire_size
         | None -> true ->
      (* Unchecked only inside the span [decode] has checked. *)
      None
  | Fixed off, Uint8 -> Some (Byte off)
  | Fixed off, Uint16 Little -> Some (U16le off)
  | Fixed off, Uint16 Big -> Some (U16be off)
  | Fixed off, Uint32 Little -> Some (U32le off)
  | Fixed off, Uint32 Big -> Some (U32be off)
  | Fixed off, Int8 -> Some (Sbyte off)
  | Fixed off, Int16 Little -> Some (I16le off)
  | Fixed off, Int16 Big -> Some (I16be off)
  | Fixed off, Int32 Little -> Some (I32le off)
  | Fixed off, Int32 Big -> Some (I32be off)
  | Bitfield { base; byte_off = off; shift; width }, Bits _ -> (
      let mask = (1 lsl width) - 1 in
      match base with
      | U8 -> Some (Bits8 { off; shift; mask })
      | U16 Little -> Some (Bits16le { off; shift; mask })
      | U16 Big -> Some (Bits16be { off; shift; mask })
      | U32 _ -> None)
  | _ -> None

(* How [decode] reads a field through [where], [enum] and total [map]
   wrappers. Their checks run in the check pass before any read, so the read
   only takes the value off the wire and decodes it. *)
let rec read_view : type a.
    min_wire_size:int -> field_access -> a typ -> a field_read option =
 fun ~min_wire_size access typ ->
  match typ with
  | Where { inner; _ } -> read_view ~min_wire_size access inner
  | Enum { base; _ } -> read_view ~min_wire_size access base
  | Map { inner; decode; index_bound = None; _ } -> (
      match read_view ~min_wire_size access inner with
      | Some (Data s) -> Some (Mapped (s, decode))
      | Some (Mapped (s, d)) -> Some (Mapped (s, fun w -> decode (d w)))
      | Some (Span _ | Fn _) | None -> None)
  | Byte_array { size = Int n } -> (
      match access with
      | Fixed off when off >= 0 && n >= 0 && off + n <= min_wire_size ->
          Some (Span (off, n))
      | _ -> None)
  | _ -> Option.map (fun s -> Data s) (field_scalar ~min_wire_size access typ)

let field_read ~min_wire_size access typ fn ~plain =
  if not plain then Fn fn
  else
    match read_view ~min_wire_size access typ with Some r -> r | None -> Fn fn

(* Bitfield group state: tracks the current base word being packed. *)
type bf_codec_state = {
  base : bitfield_base;
  bit_order : bit_order;
  base_off : int; (* byte offset of base word within record *)
  bits_used : int; (* bits consumed so far in current group *)
  total_bits : int; (* 8, 16, or 32 *)
}

(* Track the byte offset for the next field: static (constant) until we hit
   a variable-size field, then dynamic (computed from the buffer). *)
type next_off =
  | Static of int
  | Dynamic of (runtime -> Input_end.t -> bytes -> int -> int)
(* [compute runtime input_end buf base] returns the absolute byte offset where
   the next field starts. [base] is the record's base offset in [buf]. *)

type field_reader =
  string option * (runtime -> Input_end.t -> bytes -> int -> int)
(* Name + context/buffer/base reader for a previously-declared int field. A
   shadowed field has no name: it keeps its place, which numbers its slot, and
   no reference can reach it. *)

(* Closure access layer: reads leaves out of the buffer, resolving parameters
   through the operation's immutable evaluation context. *)
let bytes_leaves
    ?(sizeof_this : runtime -> Input_end.t -> bytes -> int -> int =
      fun _ _ _ _ -> 0) (env : field_reader list) :
    (runtime, Input_end.t, bytes, int) Expr_compiler.leaves =
  {
    ref_ =
      (fun name ->
        match List.assoc_opt (Some name) env with
        | Some reader -> reader
        | None ->
            Fmt.invalid_arg "Codec: unbound field ref %S in size expression"
              name);
    i64 =
      (fun name _ _ _ _ ->
        Fmt.invalid_arg
          "Codec: full-width field ref %S is only available in field \
           constraints"
          name);
    param_ref =
      (fun (Expr_compiler.Pack_param p) rt _ _ _ -> Types.eval_param rt p.name);
    sizeof_typ =
      (fun (Pack_typ t) ->
        match field_wire_size t with
        | Some n -> fun _ _ _ _ -> n
        | None -> invalid_arg "Codec: sizeof on variable-size type");
    sizeof_this;
    field_pos =
      (fun _ _ _ _ ->
        invalid_arg "Codec: [field_pos] only valid inside an action");
  }

let eval_buffer_expr f runtime input_end buf base =
  try f runtime input_end buf base
  with Parse_error ({ kind = Value_out_of_range _ | Zero_divisor; _ } as e) ->
    raise (Parse_error { e with at = base })

let compile_expr ?sizeof_this env e =
  let f = Expr_compiler.compile_int (bytes_leaves ?sizeof_this env) e in
  fun runtime input_end buf base ->
    eval_buffer_expr f runtime input_end buf base

let compile_bool_expr ?sizeof_this env e =
  let f = Expr_compiler.compile_bool (bytes_leaves ?sizeof_this env) e in
  fun runtime input_end buf base ->
    eval_buffer_expr f runtime input_end buf base

(* Int-array access layer: zero-alloc per decode. Used by field
   constraints / where clauses where the validator has already populated
   the per-decode int_array. *)
type idx = string -> int

type compile_ctx = {
  idx : idx;
  sizeof_this : int ref;
  field_pos : int;
  param_slots : (string, int) Hashtbl.t;
      (* Per-codec map from param name to its slot in this codec's decode
         array, filled at seal. Resolution lives here, not on the shared
         handle, so one handle can serve a standalone codec and an embedding
         with different slots. *)
}

let mk_ctx ?(sizeof_this = Stdlib.ref 0) ?(field_pos = 0) ~param_slots idx =
  { idx; sizeof_this; field_pos; param_slots }

let array_leaves (cc : compile_ctx) :
    (slots, unit, unit, unit) Expr_compiler.leaves =
  {
    ref_ =
      (fun name ->
        let i = cc.idx name in
        fun a () () () -> a.ints.(i));
    i64 =
      (fun name ->
        let i = cc.idx name in
        fun a () () () -> a.int64s.(i));
    param_ref =
      (fun (Expr_compiler.Pack_param p) a () () () ->
        (* The per-codec slot map is filled at seal, after these leaves are
           built, so it must be consulted per call. *)
        match Hashtbl.find_opt cc.param_slots p.Types.name with
        | Some i -> a.ints.(i)
        | None -> Fmt.invalid_arg "Codec: unbound parameter %S" p.Types.name);
    sizeof_typ =
      (fun (Pack_typ t) ->
        let n = field_wire_size t |> Option.value ~default:0 in
        fun _ () () () -> n);
    sizeof_this = (fun _ () () () -> !(cc.sizeof_this));
    field_pos = (fun _ () () () -> cc.field_pos);
  }

(* The int-array layer needs only the array, so the remaining context arguments
   are immediate units -- threaded here so callers keep the [arr -> _] shape. *)
let compile_int_arr cc e =
  let f = Expr_compiler.compile_int (array_leaves cc) e in
  fun arr -> f arr () () ()

type cmp = Eq | Ne | Lt | Le | Gt | Ge

let flip = function
  | Lt -> Gt
  | Le -> Ge
  | Gt -> Lt
  | Ge -> Le
  | (Eq | Ne) as c -> c

let cmp_slot_const cmp i n : slots -> bool =
  match cmp with
  | Eq -> fun a -> a.ints.(i) = n
  | Ne -> fun a -> a.ints.(i) <> n
  | Lt -> fun a -> a.ints.(i) < n
  | Le -> fun a -> a.ints.(i) <= n
  | Gt -> fun a -> a.ints.(i) > n
  | Ge -> fun a -> a.ints.(i) >= n

let cmp_slots cmp i j : slots -> bool =
  match cmp with
  | Eq -> fun a -> a.ints.(i) = a.ints.(j)
  | Ne -> fun a -> a.ints.(i) <> a.ints.(j)
  | Lt -> fun a -> a.ints.(i) < a.ints.(j)
  | Le -> fun a -> a.ints.(i) <= a.ints.(j)
  | Gt -> fun a -> a.ints.(i) > a.ints.(j)
  | Ge -> fun a -> a.ints.(i) >= a.ints.(j)

(* [e] over the slots. A comparison between an int field and a literal or
   another int field is one closure, the shape almost every constraint takes;
   anything else goes through [Expr_compiler]. *)
let rec compile_bool_arr cc (e : bool expr) : slots -> bool =
  let general () =
    let f = Expr_compiler.compile_bool (array_leaves cc) e in
    fun arr -> f arr () () ()
  in
  let cmp : type a. cmp -> a expr -> a expr -> slots -> bool =
   fun op x y ->
    match (x, y) with
    | Ref (I, a), Int n -> cmp_slot_const op (cc.idx a) n
    | Int n, Ref (I, a) -> cmp_slot_const (flip op) (cc.idx a) n
    | Ref (I, a), Ref (I, b) -> cmp_slots op (cc.idx a) (cc.idx b)
    | _ -> general ()
  in
  match e with
  | And (x, y) ->
      let fx = compile_bool_arr cc x and fy = compile_bool_arr cc y in
      fun a -> fx a && fy a
  | Or (x, y) ->
      let fx = compile_bool_arr cc x and fy = compile_bool_arr cc y in
      fun a -> fx a || fy a
  | Not x ->
      let fx = compile_bool_arr cc x in
      fun a -> not (fx a)
  | Eq (x, y) -> cmp Eq x y
  | Ne (x, y) -> cmp Ne x y
  | Lt (x, y) -> cmp Lt x y
  | Le (x, y) -> cmp Le x y
  | Gt (x, y) -> cmp Gt x y
  | Ge (x, y) -> cmp Ge x y
  | _ -> general ()

let eval_arr_at ~at f arr =
  try f arr with Parse_error e -> raise (Parse_error { e with at })

(* Compile action statements to operate on an int array instead of Eval.ctx.
   Assign updates the mutable parameter's array slot.
   Var binds a local by extending the index -- but since we can't grow the
   array, local vars in actions use cell-style mutation or are inlined.

   Return true short-circuits remaining statements (the action succeeds).
   Return false and Abort raise Parse_error to abort the parse. *)
type compiled_action = slots -> unit

exception Return_true

let rec compile_stmt (cc : compile_ctx) (s : Types.action_stmt) :
    compiled_action =
  match s with
  | Assign (p, e) -> (
      let fe = compile_int_arr cc e in
      fun arr ->
        let v = fe arr in
        match Hashtbl.find_opt cc.param_slots p.Types.name with
        | Some slot -> set_int_slot arr slot v
        | None -> Fmt.invalid_arg "Codec: unbound parameter %S" p.Types.name)
  | Field_assign (_, _, _) | Extern_call (_, _) -> fun _ -> ()
  | Return e ->
      let fe = compile_bool_arr cc e in
      fun arr ->
        if fe arr then raise_notrace Return_true
        else raise_constraint ~at:0 ~which:Action ()
  | Types.Abort -> fun _ -> raise_constraint ~at:0 ~which:Action ()
  | If (cond, then_, else_) ->
      let fc = compile_bool_arr cc cond in
      let ft = compile_stmts cc then_ in
      let fe =
        match else_ with
        | Some stmts -> compile_stmts cc stmts
        | None -> fun _ -> ()
      in
      fun arr -> if fc arr then ft arr else fe arr
  | Var (name, e) ->
      (* [Action.var name e] writes [e]'s value to the int_array slot for
         [name]. The slot is auto-registered by [action_vars] before the
         index is built, so [cc.idx name] always resolves; it is left
         uncaught on purpose -- a raise here would mean a missing slot
         registration (a real bug), and we want it to surface rather than
         silently write to nowhere. *)
      let i = cc.idx name in
      let fe = compile_int_arr cc e in
      fun arr -> set_int_slot arr i (fe arr)

and compile_stmts (cc : compile_ctx) (stmts : Types.action_stmt list) :
    compiled_action =
  match stmts with
  | [] -> fun _ -> ()
  | [ s ] -> compile_stmt cc s
  | stmts ->
      let compiled = List.map (compile_stmt cc) stmts in
      fun arr -> List.iter (fun f -> f arr) compiled

let compile_action (cc : compile_ctx) (act : action option) :
    compiled_action option =
  match act with
  | None -> None
  | Some (Success stmts | Act stmts) ->
      let f = compile_stmts cc stmts in
      Some (fun arr -> try f arr with Return_true -> ())

(* One step of the check pass. A scalar inside the frame is read into its slot
   with no bounds check and its constraint tested in place; any other field
   runs its general validator. *)
type step =
  | Slot : { slot : int; scalar : 'a scalar } -> step
  | Check : {
      name : string;
      slot : int;
      voff : int;
      cond : slots -> bool;
    }
      -> step
  | Range : {
      name : string;
      slot : int;
      scalar : 'a scalar;
      voff : int;
      set : (int * int) array;
      fail : runtime -> Input_end.t -> bytes -> int -> unit;
      store : bool;
    }
      -> step
      (** A scalar whose checks all bound its wire value: read it, keep it in
          its slot when something else reads it, and test it against [set], a
          few disjoint intervals. [fail] is the field's general validator, which
          raises the error the field's checks give. *)
  | Full of (slots -> runtime -> Input_end.t -> bytes -> int -> unit)
  | Covered of (slots -> runtime -> Input_end.t -> bytes -> int -> unit)
      (** A plain byte span's bounds check: the general validator, which a
          layout pass that checks every span's bounds makes redundant. *)

type packed_scalar = Ps : 'a scalar -> packed_scalar

(* How a field adds to the record's extent, for the layout pass. *)
type layout = Lfix of int | Lspan of int expr | Lrest

type ('f, 'r) record =
  | Record : {
      name : string;
      make : 'full;
      readers : ('full, 'f) readers;
          (* For a decode that has run the check pass first. *)
      checked_readers : ('full, 'f) readers;
          (* For one that reads before it checks: a field whose type validates
             checks itself as it is read. *)
      writers_rev : ('r -> runtime -> bytes -> int -> int -> int) list;
      puts_rev : 'r put list option;
      layout_rev : layout list option;
          (* Each field's extent, while every field is fixed or a plain byte
             span. *)
      prefix : (string * packed_scalar) list;
      (* The fixed-offset fields a span's size can read, by name. *)
      (* The fields' descriptor writes, while every field has one. *)
      size_of_value_rev : (Types.eval_ctx -> 'r -> int) list;
          (* Per-field value-driven size functions (one per writer). At
             seal we sum them into [size_of_value]; encode uses that
             instead of the buffer-driven [wire_size.compute] when sizing
             scratch buffers, because the latter misreads variable tails
             (all_bytes / rest_bytes / all_zeros) as "remaining buffer". *)
      min_wire_size : int;
          (* sum of all fixed-size fields -- minimum buffer size *)
      next_off : next_off; (* where the next field starts *)
      fixed_prefix : int ref;
          (* 3D's [sizeof(this)]: the byte count before the first
             variable-size field. EverParse resolves it from the type's own
             size, which stops accumulating once a field's extent is dynamic
             ([sum_size] returns the accumulator unchanged from there), so
             fixed fields after a variable one do not add to it. *)
      used_slots : (int, unit) Hashtbl.t;
          (* The slots a constraint, an action or the [where] clause reads; the
             check pass fills only those. *)
      validators_rev : (int (* byte offset *) * step) list;
      checkers_rev :
        (int (* byte offset *)
        * (slots -> runtime -> Input_end.t -> bytes -> int -> unit))
        list;
      field_actions_rev : (string * compiled_action) list;
      n_fields : int; (* count of named fields (for field indexing) *)
      n_array_slots : int;
          (* fields + action-local vars (for array allocation) *)
      r_bf : bf_codec_state option;
      field_access_rev : (string * (int * field_access)) list;
      where : bool expr option;
      doc : string option;
      field_readers : field_reader list;
      param_slots : (string, int) Hashtbl.t;
          (* Per-codec param name to decode-array slot, filled at seal. *)
    }
      -> ('f, 'r) record

type wire_size_info =
  | Fixed of int
  | Variable of {
      min_size : int;
      compute : runtime -> Input_end.t -> bytes -> int -> int;
    }

let id_counter = Atomic.make 0

(* A fused interval test, for a pass made of nothing else. *)
type interval =
  | Interval : {
      scalar : 'a scalar;
      set : (int * int) array;
      fail : runtime -> Input_end.t -> bytes -> int -> unit;
      name : string;
      voff : int;
    }
      -> interval

(* The check pass of a codec without parameters, as data, so [validate] and
   [decode] run it with one direct call. *)
type pass =
  | No_checks
  | Intervals of {
      flat : int array;
      tables : string array;
      intervals : interval array;
    }
      (** Interval tests only, on slots nothing else reads: no scratch. [flat]
          holds six ints per test, read by {!scan_intervals}; [intervals] names
          the field of a failed one. *)
  | Steps of {
      steps : step array;
      where : (slots -> bool) option;
      scratch : (unit -> slots) option;
          (** [None] when no step reads or writes a slot. *)
      clear : slots -> unit;
    }

(* What [seal] builds from a field list: the layout, the readers and writers
   and the validators. [view] does not build it; the first operation that needs
   it does (see [compiled]). *)
type 'r compiled = {
  size_of_value : Types.eval_ctx -> 'r -> int;
  field_access : (string * (int * field_access)) list;
  field_readers : field_reader list;
  field_actions : (string * compiled_action) list;
  decode : runtime -> Input_end.t -> bytes -> int -> 'r;
  read : runtime -> Input_end.t -> bytes -> int -> 'r;
      (* [decode] without its bounds check: the read phase, for bytes whose
         frame has been checked. *)
  plain_decode : (bytes -> int -> 'r) option;
      (* [decode] for a codec with no parameters and nothing to check after the
         reads: [Codec.decode] has only the bounds to check. *)
  encode : 'r -> runtime -> bytes -> int -> int;
      (* Public encode entry. [encode v runtime buf base] writes the record at
         [base], threads each per-field writer's return as the next
         writer's [write_off], and returns the final offset. *)
  wire_size : wire_size_info;
  validate : ?env_slots:int array -> bytes -> int -> unit;
  plain_validate : bytes -> int -> unit;
  plain_encode : ('r -> bytes -> int -> unit) option;
      (* [encode] of a fixed-size codec without parameters whose fields all
         have a descriptor write. *)
  fixed_frame : int;
      (* The frame of a fixed-size codec, or -1: with [plain_pass], what
         [validate] of a codec without parameters runs with no closure call. *)
  plain_pass : pass; (* [validate] for a codec without parameters. *)
  validate_ctx : runtime -> Input_end.t -> bytes -> int -> unit;
      (* [validate] for a codec embedded in another: the same bounds check and
         the same field pass, with parameters resolved by name against the
         caller's context rather than a [Param.env]. This is what a use site of
         the codec runs, so a sub-codec's checks are enforced wherever it
         appears. *)
  validate_sized : runtime -> Input_end.t -> bytes -> int -> int -> unit;
      (* [validate_ctx] of a record whose end, the last argument, the caller
         has just measured over the same bytes: the frame checks that end
         rather than measuring it again. *)
  validate_arr : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  clear_scratch : slots -> unit;
      (* Zeroes the scratch before [validate_arr], when a slot can be read
         before the pass writes it. *)
  populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  n_array_slots : int;
  has_checks : bool;
      (* Whether [validate_arr] checks anything: a constraint, a [where]
         clause, an action, or a field type whose reader validates. *)
  decode_scratch : unit -> slots;
      (* Validation scratch, one per domain (see [domain_local_slots]) and
         zeroed per decode (see [decode_exn]), mirroring the [validate]
         closure, the struct validator, and [Codec.get]'s staged reader.
         Domain-local so concurrent decode of one codec value on separate
         domains does not share the scratch. *)
  param_base : int;
  paths : string list;
      (* The fast paths [seal] selected, by name: read by [Testing.paths]. *)
}

type param_use = Stable | Settled | Unsettled

type 'r t = {
  id : int;
  name : string;
  struct_fields : Types.field list;
  n_params : int;
  param_handles : Param.packed list;
  param_use : param_use;
  where : bool expr option;
  doc : string option;
      (** Free-text note (e.g. an RFC citation) projected as a [/*++ ... --*/]
          comment on the codec's 3D typedef. *)
  compiled : 'r compiled option Atomic.t;
      (* Set once, by the first [compiled]. An atomic rather than a [Lazy.t]
         because a codec is shared across domains, and forcing one lazy value
         from two domains at once raises [Lazy.Undefined]. *)
  compile : string -> 'r compiled;
      (* Compiles the codec under the given name, which its errors report. *)
}

let pp ppf (t : _ t) = Fmt.string ppf t.name

let record_start ?where ?doc name make =
  Record
    {
      name;
      make;
      doc;
      readers = Nil;
      checked_readers = Nil;
      writers_rev = [];
      puts_rev = Some [];
      layout_rev = Some [];
      prefix = [];
      size_of_value_rev = [];
      min_wire_size = 0;
      next_off = Static 0;
      fixed_prefix = Stdlib.ref 0;
      used_slots = Hashtbl.create 8;
      validators_rev = [];
      checkers_rev = [];
      field_actions_rev = [];
      n_fields = 0;
      n_array_slots = 0;
      r_bf = None;
      field_access_rev = [];
      field_readers = [];
      where;
      param_slots = Hashtbl.create 4;
    }

let bind (f : 'a Field.t) get =
  {
    id = Field.id f;
    name = Field.name f;
    typ = Field.typ f;
    constraint_ = Field.constraint_ f;
    action = Field.action f;
    doc = Field.doc f;
    get;
  }

let ( $ ) = bind
