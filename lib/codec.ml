open Types
module Slice = Bytesrw.Bytes.Slice
module Param = Param

(** Generic blit-with-padding into a fixed [n]-byte window. The caller supplies
    the source length and a copy callback that writes exactly [len] bytes
    starting at the given destination offset; this helper handles bounding by
    [n] and zeroing the tail. *)
let blit_padded n buf off ~src_len ~blit =
  let len = min n src_len in
  blit ~dst_off:off ~len;
  if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
  off + n

let blit_string_padded n buf off v =
  blit_padded n buf off ~src_len:(String.length v) ~blit:(fun ~dst_off ~len ->
      Bytes.blit_string v 0 buf dst_off len)

let blit_slice_padded n buf off src =
  blit_padded n buf off ~src_len:(Slice.length src) ~blit:(fun ~dst_off ~len ->
      Bytes.blit (Slice.bytes src) (Slice.first src) buf dst_off len)

(* Pack a fixed-width integer setter into a [bytes -> int -> int -> int]
   field encoder that returns the offset advance. Used by the scalar cases
   in [build_field_encoder] / [build_field_reader]. *)
let setter_off n set buf off v =
  set buf off v;
  off + n

let setter_off_int32 n set buf off v =
  set buf off (Int32.of_int v);
  off + n

(* Encode a value into a fixed [n]-byte region: write it with [enc], then
   zero-pad the remainder. Used for [nested] regions. *)
let single_elem_pad n enc buf off v =
  let inner_end = enc buf off v in
  if inner_end < off + n then
    Bytes.fill buf inner_end (off + n - inner_end) '\x00';
  off + n

(* Encode a lone bitfield occupying its base word: range-check, place the value
   at its bit offset, advance by the base width. *)
let bits_field_encoder ~width ~base ~bit_order buf off v =
  let total = Bitfield.total_bits base in
  let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
  let mask = (1 lsl width) - 1 in
  if v land lnot mask <> 0 then
    Fmt.invalid_arg "Codec.encode: value 0x%X exceeds %d-bit field width" v
      width;
  Bitfield.write_word base buf off ((v land mask) lsl shift);
  off + Bitfield.byte_size base

let rec build_casetype_encoder : type a k.
    k typ -> (a, k) case_branch list -> bytes -> int -> a -> int =
 fun tag cases ->
  let tag_enc = build_field_encoder tag in
  let rec find buf off v = function
    | [] -> failwith "build_field_encoder: casetype: no matching case"
    | Case_branch { cb_inner; cb_project; _ } :: rest -> (
        (* [cb_project] yields the tag to write (its fixed index for an explicit
           case, or the value-carried tag for the default) and the body. *)
        match cb_project v with
        | Some (t, body) ->
            let off' = tag_enc buf off t in
            build_field_encoder cb_inner buf off' body
        | None -> find buf off v rest)
  in
  fun buf off v -> find buf off v cases

and build_field_encoder : type a. a typ -> bytes -> int -> a -> int =
 fun typ ->
  match typ with
  | Uint8 ->
      fun buf off v ->
        Bytes.set_uint8 buf off v;
        off + 1
  | Uint16 Little ->
      fun buf off v ->
        Bytes.set_uint16_le buf off v;
        off + 2
  | Uint16 Big ->
      fun buf off v ->
        Bytes.set_uint16_be buf off v;
        off + 2
  | Uint32 Little ->
      fun buf off v ->
        UInt32.set_le buf off v;
        off + 4
  | Uint32 Big ->
      fun buf off v ->
        UInt32.set_be buf off v;
        off + 4
  | Uint63 Little ->
      fun buf off v ->
        UInt63.set_le buf off v;
        off + 8
  | Uint63 Big ->
      fun buf off v ->
        UInt63.set_be buf off v;
        off + 8
  | Uint64 Little ->
      fun buf off v ->
        Bytes.set_int64_le buf off v;
        off + 8
  | Uint64 Big ->
      fun buf off v ->
        Bytes.set_int64_be buf off v;
        off + 8
  | Int8 -> setter_off 1 Bytes.set_int8
  | Int16 Little -> setter_off 2 Bytes.set_int16_le
  | Int16 Big -> setter_off 2 Bytes.set_int16_be
  | Int32 Little -> setter_off_int32 4 Bytes.set_int32_le
  | Int32 Big -> setter_off_int32 4 Bytes.set_int32_be
  | Int64 Little -> setter_off 8 Bytes.set_int64_le
  | Int64 Big -> setter_off 8 Bytes.set_int64_be
  | Float32 Little ->
      fun buf off v ->
        Bytes.set_int32_le buf off (Int32.bits_of_float v);
        off + 4
  | Float32 Big ->
      fun buf off v ->
        Bytes.set_int32_be buf off (Int32.bits_of_float v);
        off + 4
  | Float64 Little ->
      fun buf off v ->
        Bytes.set_int64_le buf off (Int64.bits_of_float v);
        off + 8
  | Float64 Big ->
      fun buf off v ->
        Bytes.set_int64_be buf off (Int64.bits_of_float v);
        off + 8
  | Uint_var { size = Int n; endian } ->
      fun buf off v ->
        Uint_var.write endian buf off n v;
        off + n
  | Byte_array { size = Int n } -> blit_string_padded n
  | Byte_array_where { size = Int n; _ } -> blit_string_padded n
  | Byte_slice { size = Int n } ->
      fun buf off v ->
        let len = min n (Slice.length v) in
        Bytes.blit (Slice.bytes v) (Slice.first v) buf off len;
        if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
        off + n
  | Where { inner; _ } -> build_field_encoder inner
  | Enum { base; _ } -> build_field_encoder base
  | Map { inner; encode; _ } ->
      let enc = build_field_encoder inner in
      fun buf off v -> enc buf off (encode v)
  | Unit -> fun _buf off () -> off
  | Codec { codec_encode; _ } -> fun buf off v -> codec_encode v buf off
  | Casetype { tag; cases; _ } -> build_casetype_encoder tag cases
  | Array { len = Int _; elem; seq = Seq_map s } ->
      let enc_elem = build_field_encoder elem in
      fun buf start_off vs ->
        let cur = Stdlib.ref start_off in
        s.iter (fun v -> cur := enc_elem buf !cur v) vs;
        !cur
  (* NUL-terminated string element / casetype case body: the bytes then a NUL.
     [zeroterm_at_most] pads the rest of its fixed [n]-byte region with zeros. *)
  | Zeroterm ->
      fun buf off v ->
        let n = String.length v in
        if String.contains v '\000' then
          invalid_arg "Codec.encode: zeroterm string contains a NUL byte";
        Bytes.blit_string v 0 buf off n;
        Bytes.set_uint8 buf (off + n) 0;
        off + n + 1
  | Zeroterm_at_most { size = Int region } ->
      fun buf off v ->
        let n = String.length v in
        if String.contains v '\000' then
          invalid_arg "Codec.encode: zeroterm string contains a NUL byte";
        if n + 1 > region then
          Fmt.invalid_arg
            "Codec.encode: zeroterm string needs %d bytes but region is %d"
            (n + 1) region;
        Bytes.blit_string v 0 buf off n;
        Bytes.fill buf (off + n) (region - n) '\x00';
        off + region
  (* A lone bitfield (e.g. a casetype case body) occupies its whole base word;
     write the value at its bit offset into an otherwise-zero word. *)
  | Bits { width; base; bit_order } ->
      bits_field_encoder ~width ~base ~bit_order
  (* A nested region (e.g. a casetype case body): encode the inner at the
     region start, then zero-pad the rest of the fixed [n]-byte region. *)
  | Single_elem { size = Int n; elem; _ } ->
      single_elem_pad n (build_field_encoder elem)
  | _ ->
      (* Fallback for complex types - not specialized *)
      fun _buf _off _v -> failwith "build_field_encoder: unsupported type"

(* An [enum] decodes through its base integer type; validate that the value is
   one of the named cases, raising [Invalid_enum] otherwise, so the decoder
   rejects exactly the inputs the EverParse-generated validator (and the
   [parse_direct] path) does, instead of accepting any base value. [valid] is
   computed once so the returned check allocates nothing per decode. *)
let enum_checker cases =
  let valid = List.map snd cases in
  fun ~at v ->
    if List.mem v valid then v else raise_invalid_enum ~at ~value:v ~valid

(* An open enum ([closed = false]) accepts any base value: the names only
   document known members, so decode does not reject the rest. *)
let enum_check cases closed =
  if closed then enum_checker cases else fun ~at:_ v -> v

(** Build a direct field reader that reads at a fixed offset. No tuples, no refs
    \- just pure value read. Caller must ensure the buffer is large enough. *)
let rec build_field_reader : type a. a typ -> int -> bytes -> int -> a =
 fun typ field_off ->
  match typ with
  | Uint8 -> fun buf base -> Bytes.get_uint8 buf (base + field_off)
  | Uint16 Little -> fun buf base -> Bytes.get_uint16_le buf (base + field_off)
  | Uint16 Big -> fun buf base -> Bytes.get_uint16_be buf (base + field_off)
  | Uint32 Little -> fun buf base -> UInt32.le buf (base + field_off)
  | Uint32 Big -> fun buf base -> UInt32.be buf (base + field_off)
  | Uint63 Little -> fun buf base -> UInt63.le buf (base + field_off)
  | Uint63 Big -> fun buf base -> UInt63.be buf (base + field_off)
  | Uint64 Little -> fun buf base -> Bytes.get_int64_le buf (base + field_off)
  | Uint64 Big -> fun buf base -> Bytes.get_int64_be buf (base + field_off)
  | Int8 -> fun buf base -> Bytes.get_int8 buf (base + field_off)
  | Int16 Little -> fun buf base -> Bytes.get_int16_le buf (base + field_off)
  | Int16 Big -> fun buf base -> Bytes.get_int16_be buf (base + field_off)
  | Int32 Little ->
      fun buf base -> Int32.to_int (Bytes.get_int32_le buf (base + field_off))
  | Int32 Big ->
      fun buf base -> Int32.to_int (Bytes.get_int32_be buf (base + field_off))
  | Int64 Little -> fun buf base -> Bytes.get_int64_le buf (base + field_off)
  | Int64 Big -> fun buf base -> Bytes.get_int64_be buf (base + field_off)
  | Float32 Little ->
      fun buf base ->
        Int32.float_of_bits (Bytes.get_int32_le buf (base + field_off))
  | Float32 Big ->
      fun buf base ->
        Int32.float_of_bits (Bytes.get_int32_be buf (base + field_off))
  | Float64 Little ->
      fun buf base ->
        Int64.float_of_bits (Bytes.get_int64_le buf (base + field_off))
  | Float64 Big ->
      fun buf base ->
        Int64.float_of_bits (Bytes.get_int64_be buf (base + field_off))
  | Uint_var { size = Int n; endian } ->
      fun buf base -> Uint_var.read endian buf (base + field_off) n
  | Byte_array { size = Int n } ->
      fun buf base -> Bytes.sub_string buf (base + field_off) n
  | Byte_array_where { size = Int n; _ } ->
      fun buf base -> Bytes.sub_string buf (base + field_off) n
  | Byte_slice { size = Int n } ->
      fun buf base -> Slice.make_or_eod buf ~first:(base + field_off) ~length:n
  | Where { inner; _ } -> build_field_reader inner field_off
  | Enum { base; cases; closed; _ } ->
      let read = build_field_reader base field_off in
      let check = enum_check cases closed in
      fun buf base -> check ~at:(base + field_off) (read buf base)
  | Map { inner; decode; _ } ->
      let read = build_field_reader inner field_off in
      fun buf base -> decode (read buf base)
  | Unit -> fun _buf _base -> ()
  (* A fixed-size sub-record / sub-codec as an array or nested element: decode
     it at the element offset. The element must be fixed-width (the Array case
     already gates on [field_wire_size]), so the sub-codec is too. *)
  | Codec { codec_decode; _ } ->
      fun buf base -> codec_decode buf (base + field_off)
  | Array { len = Int n; elem; seq = Seq_map s } -> (
      match field_wire_size elem with
      | Some elem_sz ->
          let read_elem = build_field_reader elem 0 in
          fun buf base ->
            let acc = Stdlib.ref s.empty in
            for i = 0 to n - 1 do
              let v = read_elem buf (base + field_off + (i * elem_sz)) in
              acc := s.add !acc v
            done;
            s.finish !acc
      | None ->
          fun _buf _base -> failwith "build_field_reader: unsupported type")
  | _ -> fun _buf _base -> failwith "build_field_reader: unsupported type"

let int_of_typ_value = Eval.int_of_exn

type slots = { ints : int array; int64s : int64 array }

let alloc_slots n = { ints = Array.make n 0; int64s = Array.make n 0L }

let clear_slots s n =
  Array.fill s.ints 0 n 0;
  Array.fill s.int64s 0 n 0L

(* A per-domain [slots] scratch. The validators and decoders below reuse one
   scratch across calls to stay allocation-free, but a codec value is shared
   across domains, so a single shared scratch lets two domains decoding at once
   overwrite each other's fields. A misparsed segment is then dropped, which in
   a TCP stack pinned one flow per core stalls that flow. Domain-local storage
   gives each domain its own scratch, still reused within the domain: a decode
   performs no effects, so cooperative fibers never interleave mid-decode.
   Preemptive systhreads sharing a domain must still not decode one codec at
   once. *)
let domain_local_slots n =
  let key = Domain.DLS.new_key (fun () -> alloc_slots n) in
  fun () -> Domain.DLS.get key

let set_int_slot s idx v = s.ints.(idx) <- v
let set_int64_slot s idx v = s.int64s.(idx) <- v

(* Raise a field-constraint failure, reporting the offending field's value so a
   demux can route on it (e.g. a version field that failed [self = N]).
   [field_idx] is the field's int slot; [at] is the record's byte offset. *)
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
let rec build_populate : type a.
    a typ -> int -> (bytes -> int -> a) -> slots -> bytes -> int -> unit =
 fun typ idx reader ->
  match typ with
  | Uint8 -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  | Uint16 _ -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  | Uint_var _ -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  | Uint32 _ ->
      fun slots buf base ->
        set_int_slot slots idx (UInt32.to_int (reader buf base))
  | Uint63 _ ->
      fun slots buf base ->
        set_int_slot slots idx (UInt63.to_int (reader buf base))
  | Int8 -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  | Int16 _ -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  | Int32 _ -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  | Bits _ -> fun slots buf base -> set_int_slot slots idx (reader buf base)
  (* [Uint64] / [Int64] populate both slots: the full value in [int64s] for
     full-width [Field.int64] refs, and the truncated value in [ints] so legacy
     int-kind [Field.ref] / [Field.int] constraints keep their pre-int64
     behavior instead of silently reading an unpopulated zero. ([has_int64_slot]
     gates the int64 ref API to exactly these types.) *)
  | Uint64 _ ->
      fun slots buf base ->
        let v = reader buf base in
        set_int64_slot slots idx v;
        Option.iter (set_int_slot slots idx) (Int64.unsigned_to_int v)
  | Int64 _ ->
      fun slots buf base ->
        let v = reader buf base in
        set_int64_slot slots idx v;
        set_int_slot slots idx (Int64.to_int v)
  (* Floats store their IEEE 754 bit pattern in the int slot so [Ref name] in a
     constraint sees the value the 3D side sees. The user-facing reader still
     returns the [float], reinterpreted via [Int*.float_of_bits] elsewhere. *)
  | Float32 Little ->
      fun slots buf base ->
        set_int_slot slots idx (Bytes.get_int32_le buf base |> Int32.to_int)
  | Float32 Big ->
      fun slots buf base ->
        set_int_slot slots idx (Bytes.get_int32_be buf base |> Int32.to_int)
  | Float64 Little ->
      fun slots buf base ->
        set_int_slot slots idx (Bytes.get_int64_le buf base |> Int64.to_int)
  | Float64 Big ->
      fun slots buf base ->
        set_int_slot slots idx (Bytes.get_int64_be buf base |> Int64.to_int)
  | Where { inner; _ } -> build_populate inner idx reader
  | Enum { base; cases; closed; _ } ->
      let check = enum_check cases closed in
      build_populate base idx (fun buf b -> check ~at:b (reader buf b))
  | Map { inner; encode; _ } ->
      build_populate inner idx (fun buf base -> encode (reader buf base))
  | Optional_or { inner; _ } ->
      (* [Optional_or] returns the inner type directly (no [option] wrapper),
         so the reader is already of the inner's value type. *)
      build_populate inner idx reader
  | Optional { inner; _ } -> (
      (* The optional decodes as ['inner option]; populate from the inner
         value when present, leave the slot at 0 when absent. *)
      let inner_populate =
        build_populate inner idx (fun buf base ->
            match reader buf base with Some v -> v | None -> assert false)
      in
      fun slots buf base ->
        match reader buf base with
        | Some _ -> inner_populate slots buf base
        | None -> ())
  | Byte_array _ | Byte_slice _ | All_bytes | Unit ->
      (* A plain byte span (or unit) carries no constrained data, so its reader
         only re-slices the buffer. Running it for effect would allocate a string
         or slice per validate with nothing to check; skip it so validating a
         span-only codec stays allocation-free on a hot read path. *)
      fun _slots _buf _base -> ()
  | _ ->
      (* No int slot to populate, but the reader performs a real decode-side
         check (all-zeros span, NUL terminator, refined span, embedded codec /
         array element constraints), so run it for effect: [Codec.validate] then
         rejects exactly what [decode] does. The value is discarded. *)
      fun _slots buf base -> ignore (reader buf base)

(* Bitfield extraction descriptor: word reader + packed shift/mask.
   Packing shift and mask into a single int lets [extract] be a direct
   [@inline always] function instead of an indirect closure call. *)
type bf_info = {
  word_reader : bytes -> int -> int;
  packed : int; (* shift in bits 0-7, mask in bits 8+ *)
}

type field_access =
  | Fixed of int
  | Bitfield of {
      base : bitfield_base;
      byte_off : int;
      shift : int;
      width : int;
    }
  | Dynamic of (bytes -> int -> int)
  | Variable of { off : int; size_fn : bytes -> int -> int }
  | Variable_dynamic of {
      off_fn : bytes -> int -> int;
      size_fn : bytes -> int -> int;
    }

type ('a, 'r) field = {
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
      ('full, 'a -> 'rest) readers * (bytes -> int -> 'a)
      -> ('full, 'rest) readers

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
type next_off = Static of int | Dynamic of (bytes -> int -> int)
(* [compute buf base] returns the absolute byte offset where the next
         field starts. [base] is the record's base offset in [buf]. *)

type field_reader = string * (bytes -> int -> int)
(* Name + buffer-and-base reader for a previously-declared int field. *)

(* Single GADT walker for [a expr] used by both call surfaces (closure
   over bytes for variable-size sizing; int_array lookup for constraint
   evaluation). The two surfaces share every operator dispatch and only
   differ in how leaves ([Ref], [Param_ref], [Sizeof], [Sizeof_this],
   [Field_pos]) resolve. *)

type packed_param = Pack_param : ('a, 'k) param_handle -> packed_param

(* Leaves resolution strategy for a given access layer. The context is
   threaded as two curried arguments rather than a single packed value: the
   closure access layer passes [buf base] with no per-call tuple, and the
   int-array layer passes [arr ()] with an immediate unit. *)
type ('c1, 'c2) leaves = {
  ref_ : string -> 'c1 -> 'c2 -> int;
  i64 : string -> 'c1 -> 'c2 -> int64;
  param_ref : packed_param -> 'c1 -> 'c2 -> int;
  sizeof_typ : packed_typ -> 'c1 -> 'c2 -> int;
  sizeof_this : 'c1 -> 'c2 -> int;
  field_pos : 'c1 -> 'c2 -> int;
}

let compile_int64 : type c1 c2.
    (c1, c2) leaves -> int64 expr -> c1 -> c2 -> int64 =
 fun l e ->
  match e with Int64 n -> fun _ _ -> n | Ref (I64, name) -> l.i64 name

let try_int64 : type a c1 c2.
    (c1, c2) leaves -> a expr -> (c1 -> c2 -> int64) option =
 fun l e ->
  let go e = Some (compile_int64 l e) in
  match e with Int64 _ as e -> go e | Ref (I64, _) as e -> go e | _ -> None

let rec compile_int : type c1 c2. (c1, c2) leaves -> int expr -> c1 -> c2 -> int
    =
 fun l e ->
  let rec_ = compile_int l in
  match e with
  | Int n -> fun _ _ -> n
  | Ref (I, name) -> l.ref_ name
  | Param_ref p -> l.param_ref (Pack_param p)
  | Sizeof t -> l.sizeof_typ (Pack_typ t)
  | Sizeof_this -> l.sizeof_this
  | Field_pos -> l.field_pos
  | Add (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d + fb c d
  | Sub (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d - fb c d
  | Mul (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d * fb c d
  | Div (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d / fb c d
  | Mod (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d mod fb c d
  | Land (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d land fb c d
  | Lor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d lor fb c d
  | Lxor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d lxor fb c d
  | Lnot a ->
      let fa = rec_ a in
      fun c d -> lnot (fa c d)
  | Lsl (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d lsl fb c d
  | Lsr (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c d -> fa c d lsr fb c d
  | Cast (w, a) -> (
      let fa = rec_ a in
      match w with
      | `U8 -> fun c d -> fa c d land 0xFF
      | `U16 -> fun c d -> fa c d land 0xFFFF
      | `U32 -> fun c d -> fa c d land 0xFFFF_FFFF
      | `U64 -> fa)
  | If_then_else (c, t, e) ->
      let fc = compile_bool l c in
      let ft = rec_ t and fe = rec_ e in
      fun c' d' -> if fc c' d' then ft c' d' else fe c' d'

(* Typed-GADT projector: refines [a expr] to [int expr] / [bool expr]
   so [Eq] / [Ne] can dispatch to the right compiler at the right type. *)
and try_int : type a c1 c2.
    (c1, c2) leaves -> a expr -> (c1 -> c2 -> int) option =
 fun l e ->
  let go e = Some (compile_int l e) in
  match e with
  | Int _ as e -> go e
  | Ref (I, _) as e -> go e
  | Param_ref _ as e -> go e
  | Sizeof _ as e -> go e
  | Sizeof_this as e -> go e
  | Field_pos as e -> go e
  | Add _ as e -> go e
  | Sub _ as e -> go e
  | Mul _ as e -> go e
  | Div _ as e -> go e
  | Mod _ as e -> go e
  | Land _ as e -> go e
  | Lor _ as e -> go e
  | Lxor _ as e -> go e
  | Lnot _ as e -> go e
  | Lsl _ as e -> go e
  | Lsr _ as e -> go e
  | Cast _ as e -> go e
  | If_then_else _ as e -> go e
  | _ -> None

and try_bool : type a c1 c2.
    (c1, c2) leaves -> a expr -> (c1 -> c2 -> bool) option =
 fun l e ->
  let go e = Some (compile_bool l e) in
  match e with
  | Bool _ as e -> go e
  | Eq _ as e -> go e
  | Ne _ as e -> go e
  | Lt _ as e -> go e
  | Le _ as e -> go e
  | Gt _ as e -> go e
  | Ge _ as e -> go e
  | And _ as e -> go e
  | Or _ as e -> go e
  | Not _ as e -> go e
  | _ -> None

and compile_bool : type c1 c2. (c1, c2) leaves -> bool expr -> c1 -> c2 -> bool
    =
 fun l e ->
  let bool_rec = compile_bool l in
  match e with
  | Bool b -> fun _ _ -> b
  | Eq (a, b) -> (
      match
        ( try_int l a,
          try_int l b,
          try_int64 l a,
          try_int64 l b,
          try_bool l a,
          try_bool l b )
      with
      | Some fa, Some fb, _, _, _, _ -> fun c d -> fa c d = fb c d
      | _, _, Some fa, Some fb, _, _ -> fun c d -> fa c d = fb c d
      | _, _, _, _, Some fa, Some fb -> fun c d -> fa c d = fb c d
      | _ -> assert false)
  | Ne (a, b) -> (
      match
        ( try_int l a,
          try_int l b,
          try_int64 l a,
          try_int64 l b,
          try_bool l a,
          try_bool l b )
      with
      | Some fa, Some fb, _, _, _, _ -> fun c d -> fa c d <> fb c d
      | _, _, Some fa, Some fb, _, _ -> fun c d -> fa c d <> fb c d
      | _, _, _, _, Some fa, Some fb -> fun c d -> fa c d <> fb c d
      | _ -> assert false)
  | Lt (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c d -> fa c d < fb c d
      | _, _, Some fa, Some fb ->
          fun c d -> Int64.unsigned_compare (fa c d) (fb c d) < 0
      | _ -> assert false)
  | Le (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c d -> fa c d <= fb c d
      | _, _, Some fa, Some fb ->
          fun c d -> Int64.unsigned_compare (fa c d) (fb c d) <= 0
      | _ -> assert false)
  | Gt (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c d -> fa c d > fb c d
      | _, _, Some fa, Some fb ->
          fun c d -> Int64.unsigned_compare (fa c d) (fb c d) > 0
      | _ -> assert false)
  | Ge (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c d -> fa c d >= fb c d
      | _, _, Some fa, Some fb ->
          fun c d -> Int64.unsigned_compare (fa c d) (fb c d) >= 0
      | _ -> assert false)
  | And (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c d -> fa c d && fb c d
  | Or (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c d -> fa c d || fb c d
  | Not e ->
      let fe = bool_rec e in
      fun c d -> not (fe c d)
  | Ref _ -> .

(* Closure-based access layer: the context is [buf] and [base], passed as two
   curried arguments. The compiled offset/size functions are [bytes -> int ->
   int] directly, so evaluating a size expression allocates nothing per call. *)
let bytes_leaves ?(sizeof_this : bytes -> int -> int = fun _ _ -> 0)
    (env : field_reader list) : (bytes, int) leaves =
  {
    ref_ =
      (fun name ->
        match List.assoc_opt name env with
        | Some reader -> reader
        | None ->
            Fmt.invalid_arg "Codec: unbound field ref %S in size expression"
              name);
    i64 =
      (fun name _ _ ->
        Fmt.invalid_arg
          "Codec: full-width field ref %S is only available in field \
           constraints"
          name);
    param_ref = (fun (Pack_param p) _ _ -> !(p.cell ()));
    sizeof_typ =
      (fun (Pack_typ t) ->
        match field_wire_size t with
        | Some n -> fun _ _ -> n
        | None -> invalid_arg "Codec: sizeof on variable-size type");
    sizeof_this;
    field_pos =
      (fun _ _ -> invalid_arg "Codec: [field_pos] only valid inside an action");
  }

let compile_expr ?sizeof_this env e =
  compile_int (bytes_leaves ?sizeof_this env) e

let compile_bool_expr ?sizeof_this env e =
  compile_bool (bytes_leaves ?sizeof_this env) e

(* Int-array access layer: zero-alloc per decode. Used by field
   constraints / where clauses where the validator has already populated
   the per-decode int_array. *)
type idx = string -> int

type compile_ctx = {
  idx : idx;
  sizeof_this : int;
  field_pos : int;
  param_slots : (string, int) Hashtbl.t;
      (* Per-codec map from param name to its slot in this codec's decode
         array, filled at seal. Resolution lives here, not on the shared
         handle, so one handle can serve a standalone codec and an embedding
         with different slots. *)
}

let mk_ctx ?(sizeof_this = 0) ?(field_pos = 0) ~param_slots idx =
  { idx; sizeof_this; field_pos; param_slots }

let array_leaves (cc : compile_ctx) : (slots, unit) leaves =
  {
    ref_ =
      (fun name ->
        let i = cc.idx name in
        fun a () -> a.ints.(i));
    i64 =
      (fun name ->
        let i = cc.idx name in
        fun a () -> a.int64s.(i));
    param_ref =
      (fun (Pack_param p) a () ->
        (* The per-codec slot map is filled at seal, after these leaves are
           built, so it must be consulted per call. A param not in this
           codec's map (e.g. one only reachable via [cell] forwarding from
           an embedding) falls back to the shared cell. *)
        match Hashtbl.find_opt cc.param_slots p.Types.name with
        | Some i -> a.ints.(i)
        | None -> !(p.cell ()));
    sizeof_typ =
      (fun (Pack_typ t) ->
        let n = field_wire_size t |> Option.value ~default:0 in
        fun _ () -> n);
    sizeof_this = (fun _ () -> cc.sizeof_this);
    field_pos = (fun _ () -> cc.field_pos);
  }

(* The int-array layer needs only the array, so the second context argument is
   an immediate unit -- threaded here so callers keep the [arr -> _] shape. *)
let compile_int_arr cc e =
  let f = compile_int (array_leaves cc) e in
  fun arr -> f arr ()

let compile_bool_arr cc e =
  let f = compile_bool (array_leaves cc) e in
  fun arr -> f arr ()

(* Compile action statements to operate on an int array instead of Eval.ctx.
   Assign writes to cell (mutable param) and updates the array.
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
        | None -> p.Types.cell () := v)
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

type ('f, 'r) record =
  | Record : {
      name : string;
      make : 'full;
      readers : ('full, 'f) readers;
      writers_rev : ('r -> bytes -> int -> int -> int) list;
      size_of_value_rev : ('r -> int) list;
          (* Per-field value-driven size functions (one per writer). At
             seal we sum them into [size_of_value]; encode uses that
             instead of the buffer-driven [wire_size.compute] when sizing
             scratch buffers, because the latter misreads variable tails
             (all_bytes / rest_bytes / all_zeros) as "remaining buffer". *)
      min_wire_size : int;
          (* sum of all fixed-size fields -- minimum buffer size *)
      next_off : next_off; (* where the next field starts *)
      fields_rev : Types.field list;
      validators_rev :
        (int (* byte offset *) * (slots -> bytes -> int -> unit)) list;
      checkers_rev :
        (int (* byte offset *) * (slots -> bytes -> int -> unit)) list;
      field_actions_rev : (string * compiled_action) list;
      n_fields : int; (* count of named fields (for field indexing) *)
      n_array_slots : int;
          (* fields + action-local vars (for array allocation) *)
      r_bf : bf_codec_state option;
      field_access_rev : (string * field_access) list;
      where : bool expr option;
      doc : string option;
      field_readers : field_reader list;
      param_slots : (string, int) Hashtbl.t;
          (* Per-codec param name to decode-array slot, filled at seal. *)
    }
      -> ('f, 'r) record

type wire_size_info =
  | Fixed of int
  | Variable of { min_size : int; compute : bytes -> int -> int }

let id_counter = Atomic.make 0

type 'r t = {
  id : int;
  name : string;
  size_of_value : 'r -> int;
  field_access : (string * field_access) list;
  field_readers : field_reader list;
  field_actions : (string * compiled_action) list;
  decode : bytes -> int -> 'r;
  encode : 'r -> bytes -> int -> int;
      (* Public encode entry. [encode v buf base] writes the record at
         [base], threads each per-field writer's return as the next
         writer's [write_off], and returns the final offset. *)
  wire_size : wire_size_info;
  struct_fields : Types.field list;
  validate : ?env_slots:int array -> bytes -> int -> unit;
  validate_arr : slots -> bytes -> int -> unit;
  populate : slots -> bytes -> int -> unit;
  n_array_slots : int;
  decode_scratch : unit -> slots;
      (* Validation scratch, one per domain (see [domain_local_slots]) and
         zeroed per decode (see [decode_exn]), mirroring the [validate]
         closure, the struct validator, and [Codec.get]'s staged reader.
         Domain-local so concurrent decode of one codec value on separate
         domains does not share the scratch. *)
  param_base : int;
  n_params : int;
  param_handles : Param.packed list;
  where : bool expr option;
  doc : string option;
      (** Free-text note (e.g. an RFC citation) projected as a [/*++ ... --*/]
          comment on the codec's 3D typedef. *)
}

let record_start ?where ?doc name make =
  Record
    {
      name;
      make;
      doc;
      readers = Nil;
      writers_rev = [];
      size_of_value_rev = [];
      min_wire_size = 0;
      next_off = Static 0;
      fields_rev = [];
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
    name = Field.name f;
    typ = Field.typ f;
    constraint_ = Field.constraint_ f;
    action = Field.action f;
    doc = Field.doc f;
    get;
  }

let ( $ ) = bind

(* Bitfield helpers -- shared module for base operations, specialized closures
   for performance-critical read/write dispatched at codec construction time. *)

let bf_base_byte_size = Bitfield.byte_size
let bf_base_total_bits = Bitfield.total_bits
let bf_base_equal = Bitfield.equal
let bf_write_base = Bitfield.write_word

let equal_bit_order (a : Types.bit_order) (b : Types.bit_order) =
  match (a, b) with
  | Msb_first, Msb_first | Lsb_first, Lsb_first -> true
  | Msb_first, Lsb_first | Lsb_first, Msb_first -> false

(* Build-time dispatch: pattern match on base happens once at codec
   construction, not on every read/write call. *)
let build_bf_reader base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | U8 ->
      fun buf off -> (Bytes.get_uint8 buf (off + byte_off) lsr shift) land mask
  | U16 Little ->
      fun buf off ->
        (Bytes.get_uint16_le buf (off + byte_off) lsr shift) land mask
  | U16 Big ->
      fun buf off ->
        (Bytes.get_uint16_be buf (off + byte_off) lsr shift) land mask
  | U32 Little ->
      fun buf off -> (UInt32.le buf (off + byte_off) lsr shift) land mask
  | U32 Big ->
      fun buf off -> (UInt32.be buf (off + byte_off) lsr shift) land mask

let err_bf_overflow width value =
  Fmt.invalid_arg "Codec.encode: value 0x%X exceeds %d-bit field width" value
    width

let[@inline] check_bf_overflow width value =
  if value lsr width <> 0 then err_bf_overflow width value

let build_bf_writer base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | U8 ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U16 Little ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U16 Big ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U32 Little ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = UInt32.le buf (off + byte_off) in
        UInt32.set_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U32 Big ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = UInt32.be buf (off + byte_off) in
        UInt32.set_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))

let build_bf_accessor_writer base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  let clear_mask = lnot (mask lsl shift) in
  match base with
  | U8 ->
      fun buf off value ->
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U16 Little ->
      fun buf off value ->
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U16 Big ->
      fun buf off value ->
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U32 Little ->
      fun buf off value ->
        let cur = UInt32.le buf (off + byte_off) in
        UInt32.set_le buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U32 Big ->
      fun buf off value ->
        let cur = UInt32.be buf (off + byte_off) in
        UInt32.set_be buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))

let build_bf_clear base byte_off =
 fun buf off -> bf_write_base base buf (off + byte_off) 0

let build_idx readers =
  let rev_readers = List.rev readers in
  fun name ->
    let rec find i = function
      | [] -> failwith ("unbound field: " ^ name)
      | (n, _) :: _ when n = name -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 rev_readers

(* Collect local variable names from action statements *)
let rec action_vars acc = function
  | Types.Var (name, _) -> name :: acc
  | Types.If (_, then_, else_) -> (
      let acc = List.fold_left action_vars acc then_ in
      match else_ with
      | Some stmts -> List.fold_left action_vars acc stmts
      | None -> acc)
  | _ -> acc

let rec iter_param_refs : type a. (Param.packed -> unit) -> a expr -> unit =
 fun f -> function
  | Param_ref p -> f (Param.Pack p)
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Land (a, b)
  | Lor (a, b)
  | Lxor (a, b)
  | Lsl (a, b)
  | Lsr (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Lt (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Le (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Gt (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Ge (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Eq (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Ne (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | And (a, b) | Or (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Not a -> iter_param_refs f a
  | Lnot a -> iter_param_refs f a
  | Cast (_, a) -> iter_param_refs f a
  | If_then_else (c, t, e) ->
      iter_param_refs f c;
      iter_param_refs f t;
      iter_param_refs f e
  | Int _ | Int64 _ | Bool _ | Ref _ | Sizeof _ | Sizeof_this | Field_pos -> ()

let rec iter_param_refs_stmt f = function
  | Types.Assign (p, e) ->
      f (Param.Pack p);
      iter_param_refs f e
  | Types.Field_assign (_, _, e) -> iter_param_refs f e
  | Types.Extern_call (_, _) -> ()
  | Types.Return e -> iter_param_refs f e
  | Types.Abort -> ()
  | Types.If (c, t, e) ->
      iter_param_refs f c;
      List.iter (iter_param_refs_stmt f) t;
      Option.iter (List.iter (iter_param_refs_stmt f)) e
  | Types.Var (_, e) -> iter_param_refs f e

let iter_param_refs_action f = function
  | Types.Success stmts | Types.Act stmts ->
      List.iter (iter_param_refs_stmt f) stmts

let rec iter_param_refs_typ : type a. (Param.packed -> unit) -> a typ -> unit =
 fun f typ ->
  match typ with
  | Byte_array { size } | Byte_slice { size } -> iter_param_refs f size
  | Byte_array_where { size; cond; _ } ->
      iter_param_refs f size;
      iter_param_refs f cond
  | Uint_var { size; _ } -> iter_param_refs f size
  | Zeroterm_at_most { size } -> iter_param_refs f size
  | Single_elem { size; elem; _ } ->
      iter_param_refs f size;
      iter_param_refs_typ f elem
  | Array { len; elem; _ } ->
      iter_param_refs f len;
      iter_param_refs_typ f elem
  | Repeat { size; elem; _ } ->
      iter_param_refs f size;
      iter_param_refs_typ f elem
  | Where { cond; inner } ->
      iter_param_refs f cond;
      iter_param_refs_typ f inner
  | Optional { present; inner } ->
      iter_param_refs f present;
      iter_param_refs_typ f inner
  | Optional_or { present; inner; _ } ->
      iter_param_refs f present;
      iter_param_refs_typ f inner
  | Apply { typ; args } ->
      iter_param_refs_typ f typ;
      List.iter (fun (Types.Pack_expr e) -> iter_param_refs f e) args
  | Map { inner; _ } -> iter_param_refs_typ f inner
  | Enum { base; _ } -> iter_param_refs_typ f base
  | Casetype { tag; cases; _ } ->
      iter_param_refs_typ f tag;
      List.iter
        (fun (Types.Case_branch { cb_inner; _ }) ->
          iter_param_refs_typ f cb_inner)
        cases
  (* An embedded sub-codec carries its own params (in field sizes / where) that
     the outer codec must surface so its env can bind them. Walk the sub-codec's
     structural form. *)
  | Codec { codec_struct; _ } ->
      iter_param_refs_fields f codec_struct.fields codec_struct.where
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ | Int8 | Int16 _ | Int32 _
  | Int64 _ | Float32 _ | Float64 _ | Bits _ | Unit | All_bytes | All_zeros
  | Zeroterm | Struct _ | Type_ref _ | Qualified_ref _ ->
      ()

and iter_param_refs_fields f fields where =
  List.iter
    (fun (Types.Field fld) ->
      iter_param_refs_typ f fld.field_typ;
      Option.iter (iter_param_refs f) fld.constraint_;
      Option.iter (iter_param_refs_action f) fld.action)
    fields;
  Option.iter (iter_param_refs f) where

let zeroterm_nul_pos buf ~first ~limit =
  let rec go i =
    if i >= limit then raise_missing_terminator ~at:first
    else if Bytes.get_uint8 buf i = 0 then i
    else go (i + 1)
  in
  go first

(* Byte size of a casetype tag. The tag is always a fixed-size scalar int, so
   the common widths answer directly: [field_wire_size] would box a [Some n]
   per call, and a casetype element reads its tag size on every iteration. *)
let tag_byte_size : type a. a typ -> int =
 fun typ ->
  match typ with
  | Uint8 | Int8 -> 1
  | Uint16 _ | Int16 _ -> 2
  | Uint32 _ | Int32 _ -> 4
  | Uint63 _ | Uint64 _ | Int64 _ -> 8
  | _ -> ( match field_wire_size typ with Some n -> n | None -> assert false)

(* Read one element of a typ at a given buffer position. Used by Repeat. *)
let rec read_elem : type a. a typ -> bytes -> int -> a =
 fun typ buf off ->
  match typ with
  | Uint8 -> Bytes.get_uint8 buf off
  | Uint16 Little -> Bytes.get_uint16_le buf off
  | Uint16 Big -> Bytes.get_uint16_be buf off
  | Uint32 Little -> UInt32.le buf off
  | Uint32 Big -> UInt32.be buf off
  | Uint63 Little -> UInt63.le buf off
  | Uint63 Big -> UInt63.be buf off
  | Uint64 Little -> Bytes.get_int64_le buf off
  | Uint64 Big -> Bytes.get_int64_be buf off
  | Int8 -> Bytes.get_int8 buf off
  | Int16 Little -> Bytes.get_int16_le buf off
  | Int16 Big -> Bytes.get_int16_be buf off
  | Int32 Little -> Int32.to_int (Bytes.get_int32_le buf off)
  | Int32 Big -> Int32.to_int (Bytes.get_int32_be buf off)
  | Int64 Little -> Bytes.get_int64_le buf off
  | Int64 Big -> Bytes.get_int64_be buf off
  | Float32 Little -> Int32.float_of_bits (Bytes.get_int32_le buf off)
  | Float32 Big -> Int32.float_of_bits (Bytes.get_int32_be buf off)
  | Float64 Little -> Int64.float_of_bits (Bytes.get_int64_le buf off)
  | Float64 Big -> Int64.float_of_bits (Bytes.get_int64_be buf off)
  | Uint_var { size = Int n; endian } -> Uint_var.read endian buf off n
  | Codec { codec_decode; _ } -> codec_decode buf off
  | Map { inner; decode; _ } -> decode (read_elem inner buf off)
  | Where { inner; _ } -> read_elem inner buf off
  | Enum { base; cases; closed; _ } ->
      enum_check cases closed ~at:off (read_elem base buf off)
  | Casetype { tag; cases; _ } ->
      let tag_val = read_elem tag buf off in
      read_case_body ~at:off ~tag cases tag_val buf (off + tag_byte_size tag)
  | Unit -> ()
  (* NUL-terminated string element: the bytes up to (not including) the NUL. *)
  | Zeroterm ->
      let nul = zeroterm_nul_pos buf ~first:off ~limit:(Bytes.length buf) in
      Bytes.sub_string buf off (nul - off)
  (* Same, bounded to a fixed [n]-byte region (the element spans the whole
     region; the value is the bytes up to the NUL within it). *)
  | Zeroterm_at_most { size = Int n } ->
      let nul = zeroterm_nul_pos buf ~first:off ~limit:(off + n) in
      Bytes.sub_string buf off (nul - off)
  (* Fixed-size byte spans as repeat / array elements: a list of n-byte
     chunks. The size is the element's own constant width. *)
  | Byte_array { size = Int n } -> Bytes.sub_string buf off n
  | Byte_array_where { size = Int n; _ } -> Bytes.sub_string buf off n
  | Byte_slice { size = Int n } -> Slice.make_or_eod buf ~first:off ~length:n
  (* A lone bitfield occupies its base word; extract the value at its bit
     offset. *)
  | Bits { width; base; bit_order } ->
      let total = bf_base_total_bits base in
      let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
      build_bf_reader base 0 shift width buf off
  (* A fixed-count array inside a [nested] region (or a casetype case body):
     decode each element at its stride. *)
  | Array { len = Int n; elem; seq = Seq_map s } -> (
      match field_wire_size elem with
      | None -> failwith "read_elem: variable-size array element"
      | Some esz ->
          let acc = Stdlib.ref s.empty in
          for i = 0 to n - 1 do
            acc := s.add !acc (read_elem elem buf (off + (i * esz)))
          done;
          s.finish !acc)
  (* A nested region as an element: decode its inner at the region start; the
     enclosing region size (padding) is accounted for by the caller. *)
  | Single_elem { elem; _ } -> read_elem elem buf off
  | _ -> failwith "read_elem: unsupported element type in repeat"

(* Dispatch a casetype's matched case body. A top-level recursion rather than a
   [let rec find] inside [read_elem]: the inner form would allocate a fresh
   closure on every casetype element decoded. *)
and read_case_body : type a k.
    at:int -> tag:k typ -> (a, k) case_branch list -> k -> bytes -> int -> a =
 fun ~at ~tag cases tag_val buf body_off ->
  match cases with
  | [] ->
      (* No branch matched the discriminant. [tag] is integer-typed in the
         common case (a version/type field); a non-integer (string) tag has no
         [int] form and reports index 0. *)
      raise_invalid_tag ~at (Option.value ~default:0 (Eval.int_of tag tag_val))
  | Case_branch { cb_tag = Some t; cb_inner; cb_inject; _ } :: _
    when t = tag_val ->
      cb_inject tag_val (read_elem cb_inner buf body_off)
  | Case_branch { cb_tag = None; cb_inner; cb_inject; _ } :: _ ->
      cb_inject tag_val (read_elem cb_inner buf body_off)
  | _ :: rest -> read_case_body ~at ~tag rest tag_val buf body_off

(* Write one element of a typ at a given buffer position. Used by Repeat. *)
let rec write_elem : type a. a typ -> bytes -> int -> a -> unit =
 fun typ buf off v ->
  match typ with
  | Uint8 -> Bytes.set_uint8 buf off v
  | Uint16 Little -> Bytes.set_uint16_le buf off v
  | Uint16 Big -> Bytes.set_uint16_be buf off v
  | Uint32 Little -> UInt32.set_le buf off v
  | Uint32 Big -> UInt32.set_be buf off v
  | Uint63 Little -> UInt63.set_le buf off v
  | Uint63 Big -> UInt63.set_be buf off v
  | Uint64 Little -> Bytes.set_int64_le buf off v
  | Uint64 Big -> Bytes.set_int64_be buf off v
  | Int8 -> Bytes.set_int8 buf off v
  | Int16 Little -> Bytes.set_int16_le buf off v
  | Int16 Big -> Bytes.set_int16_be buf off v
  | Int32 Little -> Bytes.set_int32_le buf off (Int32.of_int v)
  | Int32 Big -> Bytes.set_int32_be buf off (Int32.of_int v)
  | Int64 Little -> Bytes.set_int64_le buf off v
  | Int64 Big -> Bytes.set_int64_be buf off v
  | Float32 Little -> Bytes.set_int32_le buf off (Int32.bits_of_float v)
  | Float32 Big -> Bytes.set_int32_be buf off (Int32.bits_of_float v)
  | Float64 Little -> Bytes.set_int64_le buf off (Int64.bits_of_float v)
  | Float64 Big -> Bytes.set_int64_be buf off (Int64.bits_of_float v)
  | Uint_var { size = Int n; endian } -> Uint_var.write endian buf off n v
  | Codec { codec_encode; _ } -> ignore (codec_encode v buf off : int)
  | Map { inner; encode; _ } -> write_elem inner buf off (encode v)
  | Where { inner; _ } -> write_elem inner buf off v
  | Enum { base; _ } -> write_elem base buf off v
  | Unit -> ()
  (* Casetype reuses the full field encoder (tag + dispatched body); the
     repeat loop advances by [elem_size_of], so the returned offset is
     discarded here. *)
  | Casetype _ -> ignore (build_field_encoder typ buf off v : int)
  (* NUL-terminated string element: the bytes followed by a NUL terminator. *)
  | Zeroterm ->
      let n = String.length v in
      Bytes.blit_string v 0 buf off n;
      Bytes.set_uint8 buf (off + n) 0
  (* Fixed-size byte spans: the field encoder blits / pads the constant width
     and returns the advanced offset, which the repeat loop recomputes. *)
  | Byte_array { size = Int _ } ->
      ignore (build_field_encoder typ buf off v : int)
  | Byte_slice { size = Int _ } ->
      ignore (build_field_encoder typ buf off v : int)
  | _ -> failwith "write_elem: unsupported element type in repeat"

(* Compute the wire size of one element at a buffer position. Used by Repeat
   for variable-size elements. *)
let rec elem_size_of : type a. a typ -> bytes -> int -> int =
 fun typ buf off ->
  match typ with
  | Codec { codec_size_of; _ } -> codec_size_of buf off
  | Casetype { tag; cases; _ } ->
      let tag_size = tag_byte_size tag in
      let tag_val = read_elem tag buf off in
      tag_size + size_case_body ~at:off ~tag cases tag_val buf (off + tag_size)
  | Map { inner; _ } -> elem_size_of inner buf off
  | Where { inner; _ } -> elem_size_of inner buf off
  (* Bytes up to the NUL plus the one-byte terminator. *)
  | Zeroterm ->
      let nul = zeroterm_nul_pos buf ~first:off ~limit:(Bytes.length buf) in
      nul - off + 1
  (* A bounded NUL-terminated string spans its whole fixed [n]-byte region. *)
  | Zeroterm_at_most { size = Int n } -> n
  (* A lone bitfield occupies its base word. *)
  | Bits { base; _ } -> bf_base_byte_size base
  (* A nested region spans its whole fixed size, regardless of the inner. *)
  | Single_elem { size = Int n; _ } -> n
  | _ -> (
      match field_wire_size typ with
      | Some n -> n
      | None -> failwith "elem_size_of: cannot determine element size")

(* Wire size of a casetype's matched case body. Top-level for the same reason
   as [read_case_body]: a per-call [let rec find] would allocate a closure on
   every element. *)
and size_case_body : type a k.
    at:int -> tag:k typ -> (a, k) case_branch list -> k -> bytes -> int -> int =
 fun ~at ~tag cases tag_val buf body_off ->
  match cases with
  | [] ->
      raise_invalid_tag ~at (Option.value ~default:0 (Eval.int_of tag tag_val))
  | Case_branch { cb_tag = Some t; cb_inner; _ } :: _ when t = tag_val ->
      elem_size_of cb_inner buf body_off
  | Case_branch { cb_tag = None; cb_inner; _ } :: _ ->
      elem_size_of cb_inner buf body_off
  | _ :: rest -> size_case_body ~at ~tag rest tag_val buf body_off

(* -- Compiled field: intermediate plan for one field's contribution -- *)

(* A [compiled_field] is the self-contained plan for appending one field to a
   record state. The per-type [compile_*] helpers build one; [apply_compiled]
   consumes it and produces the updated record state. *)
type ('a, 'r) compiled_field = {
  raw_reader : bytes -> int -> 'a;
  raw_writer : 'r -> bytes -> int -> int -> int;
      (* [raw_writer v buf base write_off] writes the field's bytes
         starting at [write_off] in [buf] and returns the offset just
         after them. [base] is the record's base offset, used by
         buffer-driven helpers (size cross-checks, dynamic-gate cross
         checks) -- the field's own write position comes from
         [write_off]. *)
  extra_writers : ('r -> bytes -> int -> int -> int) list;
      (* Writers that run strictly before [raw_writer] -- e.g. a bf_clear that
         opens a new packed base word. Returns the input [write_off] unchanged. *)
  field_access : field_access;
  size_delta : int; (* added to [min_wire_size] *)
  next_off : next_off; (* new [next_off] *)
  bf_after : bf_codec_state option;
  int_reader : bytes -> int -> int;
      (* Entry stored in [field_readers] under the field name; const 0 for
         composite types that have no int-array slot of their own. *)
  nested_readers : field_reader list;
      (* Embedded sub-codec field readers, shifted into the parent frame. *)
  validator_off : int; (* [-1] when the byte offset is dynamic *)
  populate : slots -> bytes -> int -> unit;
      (* Pre-built populate function for the int-array validator path; the
         slot index (n_fields at the time the field is compiled) is baked
         in. Composite types use a no-op. *)
}

(* Parameter-free slice of the record state that [compile_field] needs. Kept
   separate so the per-type helpers do not need polymorphic recursion across
   the record's 'f type. *)
type layout_ctx = {
  next_off : next_off;
  bf : bf_codec_state option;
  field_readers : field_reader list;
  n_fields : int;
}

let layout_ctx_of : type f r. (f, r) record -> layout_ctx =
 fun (Record r) ->
  {
    next_off = r.next_off;
    bf = r.r_bf;
    field_readers = r.field_readers;
    n_fields = r.n_fields;
  }

(* -- Layout helpers shared by per-type plans -- *)

let static_off_of (ctx : layout_ctx) : int option =
  match ctx.next_off with Static n -> Some n | Dynamic _ -> None

let off_fn_of (ctx : layout_ctx) : bytes -> int -> int =
  match ctx.next_off with
  | Static n -> fun _buf _base -> n
  | Dynamic f -> fun buf base -> f buf base - base

(* Byte offset used as [sizeof_this] when compiling constraints/actions: a
   real static offset when known, the [-1] sentinel otherwise. *)
let validator_off_of (ctx : layout_ctx) : int =
  match ctx.next_off with Static n -> n | Dynamic _ -> -1

(* Field access that stores a constant or dynamic offset, based on [ctx]. *)
let fixed_or_dynamic_fa (ctx : layout_ctx) : field_access =
  match ctx.next_off with
  | Static n -> Fixed n
  | Dynamic _ -> Dynamic (off_fn_of ctx)

let require_static_off (ctx : layout_ctx) ~what : int =
  match ctx.next_off with
  | Static n -> n
  | Dynamic _ ->
      invalid_arg
        ("add_field: " ^ what ^ " after variable-size field not supported")

(* New [next_off] after appending a fixed-size contribution of [n] bytes. *)
let advance_next_off (no : next_off) (n : int) : next_off =
  match no with
  | Static k -> Static (k + n)
  | Dynamic f -> Dynamic (fun buf base -> f buf base + n)

let null_int_reader : bytes -> int -> int = fun _buf _base -> 0
let no_populate : slots -> bytes -> int -> unit = fun _arr _buf _base -> ()

(* Reader+writer pair for a Codec-or-scalar inner type placed at the current
   frame offset. Extracted so [compile_optional] and [compile_optional_or]
   share it. *)
let inner_codec_accessors : type w.
    w typ -> layout_ctx -> (bytes -> int -> w) * (bytes -> int -> w -> int) =
 fun inner ctx ->
  let field_off_static = static_off_of ctx in
  let field_off_fn = off_fn_of ctx in
  let reader : bytes -> int -> w =
    match inner with
    | Codec { codec_decode; _ } -> (
        match field_off_static with
        | Some fo -> fun buf base -> codec_decode buf (base + fo)
        | None ->
            fun buf base ->
              let fo = field_off_fn buf base in
              codec_decode buf (base + fo))
    | _ -> (
        match field_off_static with
        | Some fo -> build_field_reader inner fo
        | None ->
            let reader_at_0 = build_field_reader inner 0 in
            fun buf base ->
              let fo = field_off_fn buf base in
              reader_at_0 buf (base + fo))
  in
  (* Writer takes the absolute byte position to write at. The caller
     gets this from the threaded [write_off]; no buffer-driven
     [off_fn] lookup happens on the encode path. *)
  let writer : bytes -> int -> w -> int =
    match inner with
    | Codec { codec_encode; _ } -> fun buf off iv -> codec_encode iv buf off
    | _ ->
        let enc = build_field_encoder inner in
        fun buf off iv -> enc buf off iv
  in
  (reader, writer)

let build_nested_readers ctx readers =
  match static_off_of ctx with
  | Some fo ->
      List.map
        (fun (n, reader) -> (n, fun buf base -> reader buf (base + fo)))
        readers
  | None ->
      let off_fn = off_fn_of ctx in
      List.map
        (fun (n, reader) ->
          (n, fun buf base -> reader buf (base + off_fn buf base)))
        readers

(* -- Per-type [compile_field] helpers -- *)

(* Dispatch on the field's typ once, then delegate to the relevant per-type
   helper. Each helper builds the [compiled_field] for its case; neither the
   helpers nor [compile_field] itself apply constraints or actions -- that
   happens in [apply_compiled]. *)
let compile_bits : type r.
    layout_ctx ->
    (int, r) field ->
    int ->
    bitfield_base ->
    bit_order ->
    (int, r) compiled_field =
 fun ctx fld width base bit_order ->
  let total = bf_base_total_bits base in
  let static_off = require_static_off ctx ~what:"bitfields" in
  let base_byte_size = bf_base_byte_size base in
  let is_continuation =
    match ctx.bf with
    | Some bf ->
        bf_base_equal bf.base base
        && equal_bit_order bf.bit_order bit_order
        && bf.bits_used + width <= bf.total_bits
    | None -> false
  in
  let base_off, bits_used, size_delta, extra_writers =
    if is_continuation then
      match ctx.bf with
      | Some bf -> (bf.base_off, bf.bits_used, 0, [])
      | None -> assert false
    else
      let clear_at = build_bf_clear base 0 in
      ( static_off,
        0,
        base_byte_size,
        [
          (fun _v buf _base write_off ->
            clear_at buf write_off;
            write_off);
        ] )
  in
  let shift = Bitfield.shift ~bit_order ~total ~bits_used ~width in
  let raw_reader = build_bf_reader base base_off shift width in
  let raw_writer_inner = build_bf_writer base 0 shift width in
  let get = fld.get in
  let back, advance =
    if is_continuation then (base_byte_size, 0) else (0, base_byte_size)
  in
  let raw_writer v buf _base write_off =
    raw_writer_inner buf (write_off - back) (get v);
    write_off + advance
  in
  {
    raw_reader;
    raw_writer;
    extra_writers;
    field_access = Bitfield { base; byte_off = base_off; shift; width };
    size_delta;
    next_off = Static (static_off + size_delta);
    bf_after =
      Some
        {
          base;
          bit_order;
          base_off;
          bits_used = bits_used + width;
          total_bits = total;
        };
    int_reader = raw_reader;
    nested_readers = [];
    validator_off = static_off;
    populate = build_populate fld.typ ctx.n_fields raw_reader;
  }

let compile_codec_variable : type a r.
    layout_ctx ->
    get:(r -> a) ->
    codec_decode:(bytes -> int -> a) ->
    codec_encode:(a -> bytes -> int -> int) ->
    codec_size_of:(bytes -> int -> int) ->
    nested_readers:field_reader list ->
    (a, r) compiled_field =
 fun ctx ~get ~codec_decode ~codec_encode ~codec_size_of ~nested_readers ->
  let off_fn, (field_access : field_access), validator_off =
    match ctx.next_off with
    | Static n ->
        let size_fn buf base = codec_size_of buf (base + n) in
        ((fun _buf _base -> n), Variable { off = n; size_fn }, n)
    | Dynamic prev_end ->
        let off_fn buf base = prev_end buf base - base in
        let size_fn buf base = codec_size_of buf (base + off_fn buf base) in
        (off_fn, Variable_dynamic { off_fn; size_fn }, -1)
  in
  let size_fn =
    match field_access with
    | Variable { size_fn; _ } -> size_fn
    | Variable_dynamic { size_fn; _ } -> size_fn
    | _ -> assert false
  in
  {
    raw_reader = (fun buf base -> codec_decode buf (base + off_fn buf base));
    raw_writer =
      (fun v buf _base write_off -> codec_encode (get v) buf write_off);
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic (fun buf base -> base + off_fn buf base + size_fn buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers;
    validator_off;
    populate = no_populate;
  }

let compile_codec : type a r.
    layout_ctx ->
    (a, r) field ->
    codec_decode:(bytes -> int -> a) ->
    codec_encode:(a -> bytes -> int -> int) ->
    codec_fixed_size:int option ->
    codec_size_of:(bytes -> int -> int) ->
    codec_field_readers:field_reader list ->
    (a, r) compiled_field =
 fun ctx fld ~codec_decode ~codec_encode ~codec_fixed_size ~codec_size_of
     ~codec_field_readers ->
  let nested_readers = build_nested_readers ctx codec_field_readers in
  let get = fld.get in
  match codec_fixed_size with
  | Some fsize ->
      let raw_reader, inner_writer = inner_codec_accessors fld.typ ctx in
      let raw_writer v buf _base write_off =
        inner_writer buf write_off (get v)
      in
      {
        raw_reader;
        raw_writer;
        extra_writers = [];
        field_access = fixed_or_dynamic_fa ctx;
        size_delta = fsize;
        next_off = advance_next_off ctx.next_off fsize;
        bf_after = None;
        int_reader = null_int_reader;
        nested_readers;
        validator_off = validator_off_of ctx;
        populate = no_populate;
      }
  | None ->
      compile_codec_variable ctx ~get ~codec_decode ~codec_encode ~codec_size_of
        ~nested_readers

(* Variable-size sub-codec: dispatch on whether we sit at a static or a
   dynamic running offset. Mirrors [compile_var_bytes] -- both flavours
   produce a [Variable]/[Variable_dynamic] [field_access] that downstream
   [build_staged_reader]/[build_staged_writer] already know how to thread.
   Without the dynamic case, two consecutive variable-size sub-codec fields
   trip [require_static_off]. *)
let dynamic_optional_next_off ctx present_fn fsize =
  let base_off = ctx.next_off in
  Dynamic
    (fun buf base ->
      let off =
        match base_off with Static n -> base + n | Dynamic f -> f buf base
      in
      if present_fn buf base then off + fsize else off)

(* Absolute byte position a [next_off] points at, given the record [base]. *)
let next_off_pos : next_off -> bytes -> int -> int = function
  | Static n -> fun _buf base -> base + n
  | Dynamic f -> f

let optional_compiled : type a r.
    layout_ctx ->
    ?int_reader:(bytes -> int -> int) ->
    raw_reader:(bytes -> int -> a) ->
    raw_writer:(r -> bytes -> int -> int -> int) ->
    size_delta:int ->
    next_off:next_off ->
    populate:(slots -> bytes -> int -> unit) ->
    unit ->
    (a, r) compiled_field =
 fun ctx ?(int_reader = null_int_reader) ~raw_reader ~raw_writer ~size_delta
     ~next_off ~populate () ->
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access = fixed_or_dynamic_fa ctx;
    size_delta;
    next_off;
    bf_after = None;
    (* [int_reader] is the entry cross-field size/offset expressions read this
       field through. For an [optional_or] with an int inner it must read the
       present-or-default value, not the [null_int_reader] const 0 that left a
       byte_array sized by an optional_or field resolving to length 0. *)
    int_reader;
    nested_readers = [];
    validator_off = validator_off_of ctx;
    populate;
  }

let repeat_raw_fixed : type elt seq.
    (elt, seq) seq_map ->
    elt typ ->
    int ->
    off_fn:(bytes -> int -> int) ->
    size_fn:(bytes -> int -> int) ->
    bytes ->
    int ->
    seq =
 fun (Seq_map seq) elem esz ~off_fn ~size_fn buf base ->
  let budget = size_fn buf base in
  let start = base + off_fn buf base in
  (* The budget comes from a length field, i.e. untrusted input. Bound it to the
     buffer before reading so an oversized length fails cleanly instead of
     crashing [read_elem] with an out-of-range [Bytes.sub]. *)
  if budget < 0 || start + budget > Bytes.length buf then
    raise_eof ~at:start ~expected:(start + budget) ~got:(Bytes.length buf);
  let n = if esz > 0 then budget / esz else 0 in
  let rec loop acc i =
    if i >= n then seq.finish acc
    else loop (seq.add acc (read_elem elem buf (start + (i * esz)))) (i + 1)
  in
  loop seq.empty 0

let repeat_raw_variable : type elt seq.
    (elt, seq) seq_map ->
    elt typ ->
    off_fn:(bytes -> int -> int) ->
    size_fn:(bytes -> int -> int) ->
    bytes ->
    int ->
    seq =
 fun (Seq_map seq) elem ~off_fn ~size_fn buf base ->
  let budget = size_fn buf base in
  let start = base + off_fn buf base in
  (* Bound the untrusted budget to the buffer (see [repeat_raw_fixed]). *)
  if budget < 0 || start + budget > Bytes.length buf then
    raise_eof ~at:start ~expected:(start + budget) ~got:(Bytes.length buf);
  let rec loop acc pos remaining =
    if remaining <= 0 then seq.finish acc
    else
      let v = read_elem elem buf pos in
      let esz = elem_size_of elem buf pos in
      loop (seq.add acc v) (pos + esz) (remaining - esz)
  in
  loop seq.empty start budget

let repeat_raw_reader : type elt seq.
    (elt, seq) seq_map ->
    elt typ ->
    elem_size:int option ->
    off_fn:(bytes -> int -> int) ->
    size_fn:(bytes -> int -> int) ->
    bytes ->
    int ->
    seq =
 fun seq elem ~elem_size ~off_fn ~size_fn ->
  match elem_size with
  | Some esz -> repeat_raw_fixed seq elem esz ~off_fn ~size_fn
  | None -> repeat_raw_variable seq elem ~off_fn ~size_fn

let compile_repeat : type elt seq r.
    layout_ctx ->
    (seq, r) field ->
    int expr ->
    elt typ ->
    (elt, seq) seq_map ->
    (seq, r) compiled_field =
 fun ctx fld size_expr elem (Seq_map seq) ->
  (* Same dynamic-offset dispatch as [compile_var_bytes] / [compile_codec]:
     when a [Repeat] sits after a variable-size field, the running offset is
     [Dynamic] and is resolved at runtime rather than from a static offset. *)
  let off_fn, (field_access : field_access), validator_off =
    let sizeof_this : bytes -> int -> int =
      match ctx.next_off with
      | Static n -> fun _buf _base -> n
      | Dynamic prev_end -> fun buf base -> prev_end buf base - base
    in
    let size_fn = compile_expr ~sizeof_this ctx.field_readers size_expr in
    match ctx.next_off with
    | Static n -> ((fun _buf _base -> n), Variable { off = n; size_fn }, n)
    | Dynamic prev_end ->
        let off_fn buf base = prev_end buf base - base in
        (off_fn, Variable_dynamic { off_fn; size_fn }, -1)
  in
  let size_fn =
    match field_access with
    | Variable { size_fn; _ } -> size_fn
    | Variable_dynamic { size_fn; _ } -> size_fn
    | _ -> assert false
  in
  let elem_size = field_wire_size elem in
  let raw_reader =
    repeat_raw_reader (Seq_map seq) elem ~elem_size ~off_fn ~size_fn
  in
  let get = fld.get in
  let step =
    match elem_size with
    | Some esz -> fun _buf _pos -> esz
    | None -> fun buf pos -> elem_size_of elem buf pos
  in
  let raw_writer v buf _base write_off =
    let items = get v in
    let pos = Stdlib.ref write_off in
    seq.iter
      (fun item ->
        write_elem elem buf !pos item;
        pos := !pos + step buf !pos)
      items;
    !pos
  in
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic (fun buf base -> base + off_fn buf base + size_fn buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers = [];
    validator_off;
    populate = no_populate;
  }

(* Absolute index of the first NUL at or after [first], searching up to (but
   not including) [limit]. Raises [Parse_error] when the region ends before a
   terminator is found -- a truncated / unterminated zero-terminated string. *)
let var_bytes_reader : type a.
    a typ -> (bytes -> int -> int) -> (bytes -> int -> int) -> bytes -> int -> a
    =
 fun typ off_fn size_fn buf base ->
  let fo = off_fn buf base in
  let sz = size_fn buf base in
  let first = base + fo in
  (* [fo] and [sz] derive from length fields, i.e. untrusted input. A field
     sized past the buffer (or a negative size from an overrun offset) would
     crash [Bytes.sub]; fail cleanly instead. [Byte_slice] is handled by
     [make_or_eod]. *)
  (match typ with
  | Byte_slice _ ->
      (* A byte_slice reading at or past the end yields an eod slice (an oversize
         length is caught by the top-level bounds check), but a negative size or
         offset, from a [Sub] or an overrun length field, would crash
         [make_or_eod] with a raw [Invalid_argument] that escapes the decode
         result. Fail cleanly instead. *)
      if sz < 0 || first < 0 then
        raise_eof ~at:first ~expected:(first + sz) ~got:(Bytes.length buf)
  | _ ->
      if sz < 0 || first < 0 || first + sz > Bytes.length buf then
        raise_eof ~at:first ~expected:(first + sz) ~got:(Bytes.length buf));
  match typ with
  | Byte_slice _ -> Slice.make_or_eod buf ~first:(base + fo) ~length:sz
  | Byte_array _ -> Bytes.sub_string buf (base + fo) sz
  | Byte_array_where _ -> Bytes.sub_string buf (base + fo) sz
  | All_bytes -> Bytes.sub_string buf (base + fo) sz
  (* [sz] already counts the terminator (see [compile_var_size_fn]). *)
  | Zeroterm -> Bytes.sub_string buf (base + fo) (sz - 1)
  | Zeroterm_at_most _ ->
      let first = base + fo in
      let nul = zeroterm_nul_pos buf ~first ~limit:(first + sz) in
      Bytes.sub_string buf first (nul - first)
  | All_zeros ->
      let s = Bytes.sub_string buf (base + fo) sz in
      String.iteri
        (fun i c ->
          if c <> '\000' then raise_non_zero_padding ~at:(base + fo + i))
        s;
      s
  | Casetype _ -> read_elem typ buf (base + fo)
  | Single_elem { elem; _ } -> read_elem elem buf (base + fo)
  | _ -> assert false

(* Kept at top level: as local closures they would be heap-allocated on
   every call (two allocations per variable-bytes field on each encode). *)
let var_bytes_check_len size_fn buf base ~actual =
  let expected = size_fn buf base in
  if actual <> expected then
    Fmt.invalid_arg
      "Codec.encode: byte field length %d does not match expected %d \
       (parametric size, bind via ?env)"
      actual expected

let var_bytes_write_str buf write_off s =
  let len = String.length s in
  Bytes.blit_string s 0 buf write_off len;
  write_off + len

let var_bytes_writer : type a r.
    a typ ->
    (r -> a) ->
    (bytes -> int -> int) ->
    r ->
    bytes ->
    int ->
    int ->
    int =
 fun typ get size_fn v buf base write_off ->
  let value = get v in
  match typ with
  | Byte_slice _ ->
      let src = (value : Slice.t) in
      let len = Slice.length src in
      var_bytes_check_len size_fn buf base ~actual:len;
      Bytes.blit (Slice.bytes src) (Slice.first src) buf write_off len;
      write_off + len
  | Byte_array _ ->
      let s = (value : string) in
      var_bytes_check_len size_fn buf base ~actual:(String.length s);
      var_bytes_write_str buf write_off s
  | Byte_array_where _ ->
      let s = (value : string) in
      var_bytes_check_len size_fn buf base ~actual:(String.length s);
      var_bytes_write_str buf write_off s
  | All_bytes -> var_bytes_write_str buf write_off (value : string)
  | All_zeros -> var_bytes_write_str buf write_off (value : string)
  | Zeroterm ->
      let s = (value : string) in
      if String.contains s '\000' then
        invalid_arg "Codec.encode: zeroterm string contains a NUL byte";
      let e = var_bytes_write_str buf write_off s in
      Bytes.set_uint8 buf e 0;
      e + 1
  | Zeroterm_at_most _ ->
      let s = (value : string) in
      if String.contains s '\000' then
        invalid_arg "Codec.encode: zeroterm string contains a NUL byte";
      let region = size_fn buf base in
      let len = String.length s in
      if len + 1 > region then
        Fmt.invalid_arg
          "Codec.encode: zeroterm string needs %d bytes but region is %d"
          (len + 1) region;
      Bytes.blit_string s 0 buf write_off len;
      (* Single fill covers the NUL terminator and any trailing padding. *)
      Bytes.fill buf (write_off + len) (region - len) '\x00';
      write_off + region
  | Casetype _ -> build_field_encoder typ buf write_off value
  | Single_elem { elem; _ } ->
      let n = size_fn buf base in
      let inner_end = build_field_encoder elem buf write_off value in
      if inner_end < write_off + n then
        Bytes.fill buf inner_end (write_off + n - inner_end) '\x00';
      write_off + n
  | _ -> assert false

let compile_var_size_fn : type a.
    layout_ctx -> a typ -> off_fn:(bytes -> int -> int) -> bytes -> int -> int =
 fun ctx typ ~off_fn ->
  match typ with
  | All_bytes | All_zeros ->
      (* Trailing "rest of buffer": size is whatever bytes remain past the
         current offset. The OCaml decoder gets the buffer length via
         [Bytes.length buf]; the 3D projection emits [all_bytes], which
         3D handles natively. *)
      fun buf base -> Bytes.length buf - (base + off_fn buf base)
  | Casetype _ ->
      (* Tag is decoded first; the case body's size is whatever the
         selected case's inner typ reports. *)
      fun buf base -> elem_size_of typ buf (base + off_fn buf base)
  | Zeroterm ->
      (* Consumed bytes = string length plus the one-byte NUL terminator. *)
      fun buf base ->
        let first = base + off_fn buf base in
        zeroterm_nul_pos buf ~first ~limit:(Bytes.length buf) - first + 1
  | _ ->
      let size_expr =
        match typ with
        | Byte_slice { size } -> size
        | Byte_array { size } -> size
        | Byte_array_where { size; _ } -> size
        | Uint_var { size; _ } -> size
        | Single_elem { size; _ } -> size
        | Zeroterm_at_most { size } -> size
        | _ -> invalid_arg "add_field: unsupported variable-size field type"
      in
      let sizeof_this : bytes -> int -> int =
        match ctx.next_off with
        | Static n -> fun _buf _base -> n
        | Dynamic prev_end -> fun buf base -> prev_end buf base - base
      in
      compile_expr ~sizeof_this ctx.field_readers size_expr

let compile_var_bytes : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  let typ = fld.typ in
  let off_fn, validator_off =
    match ctx.next_off with
    | Static n -> ((fun (_buf : bytes) (_base : int) -> n), n)
    | Dynamic prev_end -> ((fun buf base -> prev_end buf base - base), -1)
  in
  let size_fn = compile_var_size_fn ctx typ ~off_fn in
  let field_access : field_access =
    match ctx.next_off with
    | Static n -> Variable { off = n; size_fn }
    | Dynamic _ -> Variable_dynamic { off_fn; size_fn }
  in
  let raw_reader, raw_writer, int_reader =
    match typ with
    | Uint_var { endian; _ } ->
        let get = fld.get in
        let raw_reader : bytes -> int -> a =
         fun buf base ->
          let fo = off_fn buf base in
          let sz = size_fn buf base in
          Uint_var.read endian buf (base + fo) sz
        in
        let raw_writer : r -> bytes -> int -> int -> int =
         fun v buf base write_off ->
          let sz = size_fn buf base in
          Uint_var.write endian buf write_off sz (get v);
          write_off + sz
        in
        let int_reader : bytes -> int -> int = raw_reader in
        (raw_reader, raw_writer, int_reader)
    | _ ->
        let raw_reader = var_bytes_reader typ off_fn size_fn in
        let raw_writer = var_bytes_writer typ fld.get size_fn in
        let int_reader buf base = int_of_typ_value typ (raw_reader buf base) in
        (raw_reader, raw_writer, int_reader)
  in
  let populate = build_populate typ ctx.n_fields raw_reader in
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic
        (fun buf base ->
          let fo = off_fn buf base in
          base + fo + size_fn buf base);
    bf_after = None;
    int_reader;
    nested_readers = [];
    validator_off;
    populate;
  }

let compile_scalar_or_var : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  let typ = fld.typ in
  let field_off_static = static_off_of ctx in
  let field_off_fn = off_fn_of ctx in
  match field_wire_size typ with
  | Some fsize ->
      let field_off = match field_off_static with Some n -> n | None -> -1 in
      let raw_reader =
        match field_off_static with
        | Some _ -> build_field_reader typ field_off
        | None ->
            let reader_at_0 = build_field_reader typ 0 in
            fun buf base ->
              let off = field_off_fn buf base in
              reader_at_0 buf (base + off)
      in
      let raw_encoder = build_field_encoder typ in
      let get = fld.get in
      let raw_writer : r -> bytes -> int -> int -> int =
       fun v buf _base write_off -> raw_encoder buf write_off (get v)
      in
      let int_reader buf base = int_of_typ_value typ (raw_reader buf base) in
      let populate = build_populate typ ctx.n_fields raw_reader in
      {
        raw_reader;
        raw_writer;
        extra_writers = [];
        field_access = fixed_or_dynamic_fa ctx;
        size_delta = fsize;
        next_off = advance_next_off ctx.next_off fsize;
        bf_after = None;
        int_reader;
        nested_readers = [];
        validator_off = field_off;
        populate;
      }
  | None -> compile_var_bytes ctx fld

let rec compile_field : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  match fld.typ with
  | Map { inner; decode; encode; _ } -> compile_map ctx fld inner decode encode
  | Enum { base; cases; closed; _ } ->
      (* Compile as the base integer, then validate the decoded value is one of
         the named cases. [compile_field] would otherwise strip the cases and
         accept any base value, unlike the EverParse validator. *)
      let cf = compile_field ctx { fld with typ = base } in
      let check = enum_check cases closed in
      {
        cf with
        raw_reader = (fun buf off -> check ~at:off (cf.raw_reader buf off));
        populate =
          (fun arr buf base ->
            cf.populate arr buf base;
            ignore (check ~at:base (cf.int_reader buf base)));
      }
  | Where { inner; _ } -> compile_field ctx { fld with typ = inner }
  | Bits { width; base; bit_order } -> compile_bits ctx fld width base bit_order
  | Codec
      {
        codec_decode;
        codec_encode;
        codec_fixed_size;
        codec_size_of;
        codec_field_readers;
        _;
      } ->
      compile_codec ctx fld ~codec_decode ~codec_encode ~codec_fixed_size
        ~codec_size_of ~codec_field_readers
  | Optional { present; inner } -> compile_optional ctx fld present inner
  | Optional_or { present; inner; default } ->
      compile_optional_or ctx fld present inner default
  | Repeat { size; elem; seq } -> compile_repeat ctx fld size elem seq
  | _ -> compile_scalar_or_var ctx fld

and compile_map : type a w r.
    layout_ctx ->
    (a, r) field ->
    w typ ->
    (w -> a) ->
    (a -> w) ->
    (a, r) compiled_field =
 fun ctx fld inner decode encode ->
  let outer_get = fld.get in
  let inner_fld =
    {
      name = fld.name;
      typ = inner;
      constraint_ = fld.constraint_;
      action = fld.action;
      doc = fld.doc;
      get = (fun v -> encode (outer_get v));
    }
  in
  let cf = compile_field ctx inner_fld in
  let checked_reader buf off = decode (cf.raw_reader buf off) in
  {
    cf with
    raw_reader = checked_reader;
    (* [decode] is where a lookup checks its index is in range; run it in
       populate too so [Codec.validate] enforces the bound decode does. The inner
       populate still fills the int slot from the raw value. *)
    populate =
      (fun arr buf base ->
        cf.populate arr buf base;
        ignore (checked_reader buf base));
  }

and compile_optional : type a r.
    layout_ctx ->
    (a option, r) field ->
    bool expr ->
    a typ ->
    (a option, r) compiled_field =
 fun ctx fld present inner ->
  let inner_size = field_wire_size inner in
  let nfields = ctx.n_fields in
  match (present, inner_size) with
  | Bool true, Some fsize ->
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let inner_populate = build_populate inner nfields inner_reader in
      let get = fld.get in
      optional_compiled ctx
        ~raw_reader:(fun buf base -> Some (inner_reader buf base))
        ~raw_writer:(fun v buf _base write_off ->
          match get v with
          | Some iv -> inner_writer buf write_off iv
          | None -> write_off + fsize)
        ~size_delta:fsize
        ~next_off:(advance_next_off ctx.next_off fsize)
        ~populate:inner_populate ()
  | Bool false, _ ->
      optional_compiled ctx
        ~raw_reader:(fun _buf _base -> None)
        ~raw_writer:(fun _v _buf _base write_off -> write_off)
        ~size_delta:0 ~next_off:ctx.next_off ~populate:no_populate ()
  | _, Some fsize ->
      let present_fn = compile_bool_expr ctx.field_readers present in
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let inner_populate = build_populate inner nfields inner_reader in
      let get = fld.get in
      optional_compiled ctx
        ~raw_reader:(fun buf base ->
          if present_fn buf base then Some (inner_reader buf base) else None)
        ~raw_writer:(fun v buf base write_off ->
          match (present_fn buf base, get v) with
          | true, Some iv -> inner_writer buf write_off iv
          | false, None -> write_off
          | true, None ->
              invalid_arg
                "Codec.encode: optional field absent but presence predicate is \
                 true"
          | false, Some _ ->
              invalid_arg
                "Codec.encode: optional field present but presence predicate \
                 is false")
        ~size_delta:0
        ~next_off:(dynamic_optional_next_off ctx present_fn fsize)
        ~populate:(fun arr buf base ->
          if present_fn buf base then inner_populate arr buf base)
        ()
  | _ -> compile_optional_variable ctx fld present inner

(* Variable-size inner: compile it as its own field and gate that compiled plan
   on [present]. This reuses every per-type path (outer-sized byte slices,
   self-delimiting sub-codecs, casetypes), so the inner decodes and encodes
   exactly as it would unwrapped; an absent gate advances no bytes. *)
and compile_optional_variable : type a r.
    layout_ctx ->
    (a option, r) field ->
    bool expr ->
    a typ ->
    (a option, r) compiled_field =
 fun ctx fld present inner ->
  let present_fn = compile_bool_expr ctx.field_readers present in
  let get = fld.get in
  let inner_fld =
    {
      name = fld.name;
      typ = inner;
      constraint_ = None;
      action = None;
      doc = fld.doc;
      get =
        (fun v ->
          match get v with
          | Some iv -> iv
          | None ->
              invalid_arg
                "Codec.encode: optional field absent but presence predicate is \
                 true");
    }
  in
  let cf = compile_field ctx inner_fld in
  optional_compiled ctx
    ~raw_reader:(fun buf base ->
      if present_fn buf base then Some (cf.raw_reader buf base) else None)
    ~raw_writer:(fun v buf base write_off ->
      match (present_fn buf base, get v) with
      | true, Some _ -> cf.raw_writer v buf base write_off
      | false, None -> write_off
      | true, None ->
          invalid_arg
            "Codec.encode: optional field absent but presence predicate is true"
      | false, Some _ ->
          invalid_arg
            "Codec.encode: optional field present but presence predicate is \
             false")
    ~size_delta:0
    ~next_off:
      (Dynamic
         (fun buf base ->
           if present_fn buf base then next_off_pos cf.next_off buf base
           else next_off_pos ctx.next_off buf base))
    ~populate:(fun arr buf base ->
      if present_fn buf base then cf.populate arr buf base)
    ()

and compile_optional_or : type a r.
    layout_ctx ->
    (a, r) field ->
    bool expr ->
    a typ ->
    a ->
    (a, r) compiled_field =
 fun ctx fld present inner default ->
  let inner_size = field_wire_size inner in
  match (present, inner_size) with
  | Bool true, Some fsize ->
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let get = fld.get in
      let populate = build_populate fld.typ ctx.n_fields inner_reader in
      optional_compiled ctx ~raw_reader:inner_reader
        ~int_reader:(fun buf base ->
          int_of_typ_value inner (inner_reader buf base))
        ~raw_writer:(fun v buf _base write_off ->
          inner_writer buf write_off (get v))
        ~size_delta:fsize
        ~next_off:(advance_next_off ctx.next_off fsize)
        ~populate ()
  | Bool false, _ ->
      optional_compiled ctx
        ~raw_reader:(fun _buf _base -> default)
        ~int_reader:(fun _buf _base -> int_of_typ_value inner default)
        ~raw_writer:(fun _v _buf _base write_off -> write_off)
        ~size_delta:0 ~next_off:ctx.next_off ~populate:no_populate ()
  | _, Some fsize ->
      let present_fn = compile_bool_expr ctx.field_readers present in
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let get = fld.get in
      let raw_reader buf base =
        if present_fn buf base then inner_reader buf base else default
      in
      let populate = build_populate fld.typ ctx.n_fields raw_reader in
      (* Encode is value-driven: the user always has a value, so always
         write [fsize] bytes. The gate is the decode-side oracle that
         chooses between the decoded inner and [default]. Round-trips
         exactly when the gate is set consistently with the user's
         choice; if the user sets the gate to [absent] while the value
         differs from [default], decoding loses the value and falls
         back to [default]. *)
      let raw_writer v buf _base write_off =
        inner_writer buf write_off (get v)
      in
      optional_compiled ctx ~raw_reader
        ~int_reader:(fun buf base ->
          int_of_typ_value inner (raw_reader buf base))
        ~raw_writer ~size_delta:fsize
        ~next_off:(advance_next_off ctx.next_off fsize)
        ~populate ()
  | _ -> compile_optional_or_variable ctx fld present inner default

(* Variable-size inner. As in the fixed case [optional_or] is value-driven and
   always occupies the inner's bytes; the gate only chooses the decoded inner
   vs [default] on the read side, so the layout always advances past it. *)
and compile_optional_or_variable : type a r.
    layout_ctx ->
    (a, r) field ->
    bool expr ->
    a typ ->
    a ->
    (a, r) compiled_field =
 fun ctx fld present inner default ->
  let present_fn = compile_bool_expr ctx.field_readers present in
  let inner_fld =
    {
      name = fld.name;
      typ = inner;
      constraint_ = None;
      action = None;
      doc = fld.doc;
      get = fld.get;
    }
  in
  let cf = compile_field ctx inner_fld in
  optional_compiled ctx
    ~raw_reader:(fun buf base ->
      if present_fn buf base then cf.raw_reader buf base else default)
    ~raw_writer:(fun v buf base write_off -> cf.raw_writer v buf base write_off)
    ~int_reader:cf.int_reader ~size_delta:0 ~next_off:cf.next_off
    ~populate:cf.populate ()

(* -- Apply a compiled plan to the record state -- *)

(* A [Wire.where] constraint lives inside the field's [typ] ([Where {cond; _}]),
   which the field reader passes through transparently. Left there it reaches the
   3D refinement (via [struct_field]) but never the OCaml validators. Collect the
   conds so [apply_compiled] can fold them into the field's check, the same path
   a [~constraint_] takes -- so decode and validate enforce exactly what the 3D
   projection does. *)
let rec typ_where_conds : type a. a Types.typ -> bool Types.expr list = function
  | Types.Where { cond; inner } -> cond :: typ_where_conds inner
  | Types.Map { inner; _ } -> typ_where_conds inner
  | Types.Enum { base; _ } -> typ_where_conds base
  | _ -> []

(* A field's runtime check: its [~constraint_] together with every top-level
   [Wire.where] cond carried in its typ, compiled against [cc]. Folding both
   keeps a [where] enforced on decode and validate, not only in the 3D
   projection. *)
let compile_field_check cc fld =
  let conds =
    (match fld.constraint_ with Some c -> [ c ] | None -> [])
    @ typ_where_conds fld.typ
  in
  match conds with
  | [] -> None
  | first :: rest ->
      Some
        (compile_bool_arr cc
           (List.fold_left (fun a c -> And (a, c)) first rest))

(* The single place that mutates the record accumulator: assemble constraint
   and action checkers, then fold the [compiled_field] into a new record
   state. *)
(* Whether reading a field of this type performs a decode-side check beyond
   storing a scalar slot: enum membership, a lookup index bound, an all-zeros /
   NUL-terminated / refined span, or a nested element's constraints. The
   provably check-free leaves (scalars, bitfields, plain byte spans, unit, an
   open enum, a bare bit/bool map) return [false]; everything else is treated as
   a check ([| _ -> true]), so a new or uncertain type is never wrongly skipped.
   A field's own [constraint_] / [action] / a [where] clause are accounted for
   separately, so this is only about the type's intrinsic read-time validation. *)
let rec field_reader_validates : type a. a Types.typ -> bool = function
  | Types.Uint8 | Types.Uint16 _ | Types.Uint32 _ | Types.Uint63 _
  | Types.Uint64 _ | Types.Int8 | Types.Int16 _ | Types.Int32 _ | Types.Int64 _
  | Types.Float32 _ | Types.Float64 _ | Types.Uint_var _ | Types.Bits _
  | Types.Byte_array _ | Types.Byte_slice _ | Types.All_bytes | Types.Unit ->
      false
  | Types.Enum { closed; _ } -> closed
  | Types.Map { inner; index_bound; _ } ->
      index_bound <> None || field_reader_validates inner
  | Types.Optional { inner; _ } -> field_reader_validates inner
  | Types.Optional_or { inner; _ } -> field_reader_validates inner
  | Types.Single_elem { elem; _ } -> field_reader_validates elem
  | Types.Array { elem; _ } -> field_reader_validates elem
  | Types.Repeat { elem; _ } -> field_reader_validates elem
  | _ -> true

(* Prepend [name] to the field path of a parse error escaping [f], so a failure
   deep in a nested decode accumulates its root-to-leaf path as the exception
   unwinds. Only fields that can raise an attributable error are wrapped; a plain
   scalar read raises only [Unexpected_eof] from the record's bounds check, which
   is recorded without a path. *)
let prepend_field name f buf base =
  try f buf base
  with Types.Parse_error e ->
    raise (Types.Parse_error { e with field = name :: e.field })

let prepend_field_check name f arr buf base =
  try f arr buf base
  with Types.Parse_error e ->
    raise (Types.Parse_error { e with field = name :: e.field })

(* Tag a field's reader and its two validators with [name] so a parse error
   escaping any of them carries the field in its path; [attributable = false] (a
   plain scalar) leaves them untouched, keeping the hot read path free of an
   exception frame. *)
let wrap_field_errors ~validates ~check ~act name raw_reader full check_only =
  if Option.is_some check || Option.is_some act || validates then
    ( prepend_field name raw_reader,
      prepend_field_check name full,
      prepend_field_check name check_only )
  else (raw_reader, full, check_only)

let apply_compiled : type a f r.
    (a -> f, r) record -> (a, r) field -> (a, r) compiled_field -> (f, r) record
    =
 fun (Record r) fld cf ->
  let action_vanames =
    match fld.action with
    | None -> []
    | Some (Types.Success stmts | Types.Act stmts) ->
        List.fold_left action_vars [] stmts
  in
  let n_extra_vars = List.length action_vanames in
  let field_idx = r.n_fields in
  let dummy_reader _buf _base = 0 in
  let cc_readers =
    let base = (fld.name, dummy_reader) :: r.field_readers in
    List.fold_left (fun acc vn -> (vn, dummy_reader) :: acc) base action_vanames
  in
  let idx = build_idx cc_readers in
  let cc =
    mk_ctx ~sizeof_this:cf.validator_off ~field_pos:field_idx
      ~param_slots:r.param_slots idx
  in
  let check = compile_field_check cc fld in
  let act = compile_action cc fld.action in
  let check_only arr buf base =
    cf.populate arr buf base;
    match check with
    | Some f when not (f arr) -> raise_field_constraint ~field_idx ~at:base arr
    | _ -> ()
  in
  (* The full validator is the read-and-check plus the field action. Decode and
     [Codec.validate] both run it, so an action never fires on one path only. *)
  let full arr buf base =
    check_only arr buf base;
    match act with Some f -> f arr | None -> ()
  in
  let faction =
    match act with None -> None | Some act_fn -> Some (fld.name, act_fn)
  in
  let byte_off = cf.validator_off in
  let new_writers_rev = (cf.raw_writer :: cf.extra_writers) @ r.writers_rev in
  let new_field_readers =
    cf.nested_readers @ ((fld.name, cf.int_reader) :: r.field_readers)
  in
  let field_typ = fld.typ in
  let field_get = fld.get in
  (* Bitfields share a base word; [cf.size_delta] is the correct
     non-overlapping contribution. Key this off the compiled access rather than
     [field_typ]: wrapped bitfields such as [bit (bits ...)] arrive here as
     [Map]/[Where]/[Enum] even though they still pack into the current word. *)
  let field_size_of_value =
    match cf.field_access with
    | Bitfield _ -> fun _ -> cf.size_delta
    | _ -> fun v -> size_of_typ_value field_typ (field_get v)
  in
  let raw_reader, full, check_only =
    wrap_field_errors
      ~validates:(field_reader_validates fld.typ)
      ~check ~act fld.name cf.raw_reader full check_only
  in
  Record
    {
      name = r.name;
      make = r.make;
      readers = Snoc (r.readers, raw_reader);
      writers_rev = new_writers_rev;
      size_of_value_rev = field_size_of_value :: r.size_of_value_rev;
      min_wire_size = r.min_wire_size + cf.size_delta;
      next_off = cf.next_off;
      fields_rev = struct_field fld :: r.fields_rev;
      validators_rev = (byte_off, full) :: r.validators_rev;
      checkers_rev = (byte_off, check_only) :: r.checkers_rev;
      field_actions_rev =
        (match faction with
        | Some fa -> fa :: r.field_actions_rev
        | None -> r.field_actions_rev);
      n_fields = List.length new_field_readers;
      n_array_slots = List.length new_field_readers + n_extra_vars;
      r_bf = cf.bf_after;
      field_access_rev = (fld.name, cf.field_access) :: r.field_access_rev;
      field_readers = new_field_readers;
      where = r.where;
      doc = r.doc;
      param_slots = r.param_slots;
    }

let add_field : type a f r. (a -> f, r) record -> (a, r) field -> (f, r) record
    =
 fun (Record _ as record) fld ->
  let ctx = layout_ctx_of record in
  let cf = compile_field ctx fld in
  apply_compiled record fld cf

(* Forward reader list: cons-list dual of [readers] snoc-list.
   Built once at seal time by [to_fwd]; applied at decode time by
   [apply_fwd], which saturates the constructor in a single call for up to
   32 fields and otherwise unrolls 32 per step (1 partial application per
   extra 32 fields). A saturated call returns the record with no
   intermediate closure; a partial application allocates a [caml_curry]
   closure per decode (visible only under flambda-off), so the threshold is
   set above the widest real protocol header (IPv4 and USLP reach ~28
   fields; a wide TCP header with its 8 flag bits split out is 18). *)
type (_, _) readers_fwd =
  | FNil : ('r, 'r) readers_fwd
  | FCons :
      (bytes -> int -> 'a) * ('rest, 'result) readers_fwd
      -> ('a -> 'rest, 'result) readers_fwd

(* Convert a [readers] snoc-list to a [readers_fwd] cons-list. O(n), runs
   once at seal time. *)
let to_fwd : type full result.
    (full, result) readers -> (full, result) readers_fwd =
 fun readers ->
  let rec go : type mid.
      (full, mid) readers ->
      (mid, result) readers_fwd ->
      (full, result) readers_fwd =
   fun readers acc ->
    match readers with
    | Nil -> acc
    | Snoc (rest, reader) -> go rest (FCons (reader, acc))
  in
  go readers FNil

(* Apply a forward reader list to [make], saturating the constructor in a
   single call for up to 32 fields, so no intermediate closure is allocated
   per decode. Beyond 32 fields one partial application is made per extra 32
   (a [caml_curry] chain under flambda-off); the threshold clears every real
   protocol header (the widest, IPv4 and USLP, reach ~28 fields) in one
   saturated call.

   The cases are a saturation table, one row per arity. The formatter is
   disabled here because it would otherwise staircase the deep [FCons]
   patterns across ~500 lines; the flat table is both shorter and easier to
   read. *)
let rec apply_fwd : type mid result.
    mid -> (mid, result) readers_fwd -> bytes -> int -> result =
 fun f fwd buf off ->
  match fwd with
  | FNil -> f
  | FCons (r1, FNil) ->
      f (r1 buf off)
  | FCons (r1, FCons (r2, FNil)) ->
      f (r1 buf off) (r2 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FNil))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FNil)))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FNil))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FNil)))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FNil))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FNil)))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FNil))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FNil)))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FNil))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FNil)))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FNil))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FNil)))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FNil))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FNil)))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FNil))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FNil)))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FNil))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FNil)))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FNil))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FNil)))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FNil))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FNil)))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FNil))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FNil)))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FNil))))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off) (r27 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FNil)))))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off) (r27 buf off) (r28 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FNil))))))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off) (r27 buf off) (r28 buf off) (r29 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FNil)))))))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off) (r27 buf off) (r28 buf off) (r29 buf off) (r30 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FCons (r31, FNil))))))))))))))))))))))))))))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off) (r27 buf off) (r28 buf off) (r29 buf off) (r30 buf off) (r31 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FCons (r31, FCons (r32, rest)))))))))))))))))))))))))))))))) ->
      apply_fwd (f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off) (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off) (r14 buf off) (r15 buf off) (r16 buf off) (r17 buf off) (r18 buf off) (r19 buf off) (r20 buf off) (r21 buf off) (r22 buf off) (r23 buf off) (r24 buf off) (r25 buf off) (r26 buf off) (r27 buf off) (r28 buf off) (r29 buf off) (r30 buf off) (r31 buf off) (r32 buf off)) rest buf off
[@@ocamlformat "disable"]

let build_decode : type full r. full -> (full, r) readers -> bytes -> int -> r =
 fun make readers ->
  match readers with
  | Nil -> fun _buf _off -> make
  | Snoc (Nil, r1) -> fun buf off -> make (r1 buf off)
  | Snoc (Snoc (Nil, r1), r2) -> fun buf off -> make (r1 buf off) (r2 buf off)
  | Snoc (Snoc (Snoc (Nil, r1), r2), r3) ->
      fun buf off -> make (r1 buf off) (r2 buf off) (r3 buf off)
  | Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4) ->
      fun buf off -> make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
  | Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5) ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
  | Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6) ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off)
  | Snoc
      (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6), r7)
    ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off) (r7 buf off)
  | Snoc
      ( Snoc
          ( Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6),
            r7 ),
        r8 ) ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off) (r7 buf off) (r8 buf off)
  | readers ->
      let fwd = to_fwd readers in
      fun buf off -> apply_fwd make fwd buf off

let lookup_reader_idx rev_readers name =
  let rec find i = function
    | [] -> failwith ("unbound field: " ^ name)
    | (n, _) :: _ when n = name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 rev_readers

let compile_where_clause param_slots field_readers where =
  match where with
  | None -> None
  | Some cond ->
      let rev_readers = List.rev field_readers in
      let idx name = lookup_reader_idx rev_readers name in
      let cc = mk_ctx ~param_slots idx in
      Some (compile_bool_arr cc cond)

(* Run a validation pass, turning an out-of-bounds read into a clean eof. A
   check may read a field whose offset depends on a length read from the buffer;
   on a buffer too short to hold it that read is out of bounds. [decode] runs
   [t.decode] first (which bounds-checks), but [Codec.validate] runs the check
   kernel directly, so this keeps a short buffer a clean failure, not a crash. *)
let validate_or_eof buf f =
  try f ()
  with Invalid_argument _ ->
    let len = Bytes.length buf in
    raise_eof ~at:len ~expected:(len + 1) ~got:len

let build_validators validators_rev checkers_rev compiled_where struct_fields
    n_total =
  let validator_fns = Array.of_list (List.map snd validators_rev) in
  let n_validators = Array.length validator_fns in
  let validate_arr arr buf off =
    for i = 0 to n_validators - 1 do
      validator_fns.(i) arr buf off
    done;
    match compiled_where with
    | Some f when not (f arr) -> raise_constraint ~at:off ~which:Where ()
    | _ -> ()
  in
  let checker_fns = Array.of_list (List.map snd checkers_rev) in
  let n_checkers = Array.length checker_fns in
  let populate arr buf off =
    for i = 0 to n_checkers - 1 do
      checker_fns.(i) arr buf off
    done
  in
  (* Run the validator pass only when the struct actually has something to
     check; the structural bounds check runs unconditionally one layer up in
     [validate_with_bounds]. A codec of plain scalars and byte spans then
     validates with no allocation, so validating it on a hot read path (a
     [Codec.get] preceded by [validate]) stays cheap. *)
  let has_checks =
    compiled_where <> None
    || List.exists
         (fun (Types.Field f) ->
           f.constraint_ <> None || f.action <> None
           || field_reader_validates f.field_typ)
         struct_fields
  in
  let validate =
    if not has_checks then fun ?env_slots:_ _buf _off -> ()
    else
      let get_arr = domain_local_slots n_total in
      fun ?env_slots buf off ->
        let arr = get_arr () in
        clear_slots arr n_total;
        (match env_slots with
        | Some slots ->
            (* Seed param slots so [where] clauses referencing [Param.input]
               evaluate correctly. *)
            let n = Array.length slots in
            Array.blit slots 0 arr.ints (n_total - n) n
        | None -> ());
        validate_or_eof buf (fun () -> validate_arr arr buf off)
  in
  (validate_arr, populate, validate)

let collect_param_handles struct_fields where =
  let seen = Hashtbl.create 4 in
  let handles = Stdlib.ref ([] : Param.packed list) in
  let visit (Param.Pack p as packed) =
    if not (Hashtbl.mem seen p.name) then begin
      Hashtbl.add seen p.name ();
      handles := packed :: !handles
    end
  in
  iter_param_refs_fields visit struct_fields where;
  List.rev !handles

(* Fill a codec's param name to decode-slot map: handle [i] sits at
   [param_base + i] in the decode array (and at env slot [i]). *)
let fill_param_slots tbl param_base handles =
  Hashtbl.reset tbl;
  List.iteri
    (fun i (Param.Pack p) -> Hashtbl.replace tbl p.Types.name (param_base + i))
    handles

(* The structural bounds check decode runs before reading any field: the buffer
   must hold at least [min_size] bytes, and the codec's resolved wire size must
   fit. Shared with [validate] so a constraint-free codec is still bounds-checked
   before a zero-copy [get] reads its fields. *)
let check_decode_bounds wire_size_info min_size buf off =
  if off + min_size > Bytes.length buf then
    raise_eof ~at:off ~expected:min_size ~got:(Bytes.length buf - off);
  match wire_size_info with
  | Fixed _ -> ()
  | Variable { compute; _ } ->
      let end_off =
        try compute buf off
        with Invalid_argument _ ->
          raise_eof ~at:off ~expected:min_size ~got:(Bytes.length buf - off)
      in
      if end_off < off + min_size then
        raise_eof ~at:off ~expected:min_size ~got:(Bytes.length buf - off);
      if end_off > Bytes.length buf then
        raise_eof ~at:off ~expected:(end_off - off) ~got:(Bytes.length buf - off)

let build_checked_decode raw_decode wire_size_info min_size =
 fun buf off ->
  check_decode_bounds wire_size_info min_size buf off;
  raw_decode buf off

(* [Codec.validate] is the safety gate before a batch of zero-copy [get] calls on
   untrusted input, so it must run decode's structural bounds check even for a
   constraint-free codec (whose constraint validator [validate_checks] is a
   no-op), or a [get] on a truncated buffer would read out of bounds. *)
let validate_with_bounds wire_size_info min_size validate_checks =
 fun ?env_slots buf off ->
  check_decode_bounds wire_size_info min_size buf off;
  validate_checks ?env_slots buf off

(* Whether a field consumes the rest of the buffer: a greedy leaf ([all_bytes] /
   [all_zeros]), an embedded sub-codec whose own last field does, or a casetype
   any of whose case bodies does (if that case is selected its greedy tail has no
   boundary). A greedy tail has no boundary once another field follows it, so
   each of these is just as unbounded as a bare greedy field. *)
let rec field_consumes_rest : type a. a Types.typ -> bool =
 fun t ->
  is_greedy t
  ||
  match t with
  | Types.Codec { codec_struct; _ } -> (
      match List.rev codec_struct.fields with
      | Types.Field f :: _ -> field_consumes_rest f.field_typ
      | [] -> false)
  | Types.Casetype { cases; _ } ->
      List.exists
        (fun (Types.Case_branch { cb_inner; _ }) ->
          field_consumes_rest cb_inner)
        cases
  | Types.Map { inner; _ } -> field_consumes_rest inner
  | Types.Where { inner; _ } -> field_consumes_rest inner
  | Types.Enum { base; _ } -> field_consumes_rest base
  | _ -> false

(* A greedy field consumes the rest of the buffer, so it is only meaningful as
   the last field: an earlier one starves every field after it (and 3D's
   [:consume-all] must be last too). This also covers an embedded sub-codec
   ending in a greedy field, whose tail would otherwise swallow the following
   field's bytes. Reject it at construction rather than silently truncating
   later fields at decode. *)
let reject_greedy_not_last name fields =
  let rec check = function
    | [] | [ _ ] -> ()
    | Types.Field f :: rest ->
        if field_consumes_rest f.field_typ then
          Fmt.invalid_arg
            "Codec.v %s: a field that consumes the rest of the buffer \
             (all_bytes / all_zeros, or a sub-codec ending in one) must be the \
             last field, but %s is followed by more fields"
            name
            (Option.value ~default:"<anon>" f.field_name);
        check rest
  in
  check fields

(* A [Wire.where] is expressible in 3D only as a top-level field refinement
   ([UINT8 g { cond }], which projects and is enforced). Inside a container the
   projection emits invalid 3D that 3d.exe rejects ([UINT8 { cond } vals[N]] for
   an array element, [UINT8 { cond } opt[:byte-size ...]] for an optional inner):
   an element refinement cannot reference the outer field it needs. Such a where
   would ship a codec whose [.3d] does not compile while OCaml decode silently
   drops the constraint, so reject it at construction. *)
let reject_nested_where name fields =
  (* A [where true] is elided by the projection (no refinement is emitted), so it
     is a harmless no-op wrapper even inside a container. Only a non-trivial cond
     emits a refined element, which is what EverParse rejects. *)
  let rec any_real_where : type a. a Types.typ -> bool = function
    | Types.Where { cond = Types.Bool true; inner } -> any_real_where inner
    | Types.Where _ -> true
    | Types.Map { inner; _ } -> any_real_where inner
    | Types.Enum { base; _ } -> any_real_where base
    | Types.Array { elem; _ } -> any_real_where elem
    | Types.Repeat { elem; _ } -> any_real_where elem
    | Types.Optional { inner; _ } -> any_real_where inner
    | Types.Optional_or { inner; _ } -> any_real_where inner
    | Types.Single_elem { elem; _ } -> any_real_where elem
    | _ -> false
  in
  let fail fname =
    Fmt.invalid_arg
      "Codec.v %s: field %s puts a Wire.where inside a container element; a \
       where projects to 3D only as a top-level field refinement, so this \
       shape has no verified validator. Move the constraint to the field \
       itself or a codec ~where."
      name fname
  in
  let rec check_typ : type a. string -> a Types.typ -> unit =
   fun fname t ->
    match t with
    | Types.Where { inner; _ } -> check_typ fname inner
    | Types.Map { inner; _ } -> check_typ fname inner
    | Types.Enum { base; _ } -> check_typ fname base
    | Types.Array { elem; _ } -> if any_real_where elem then fail fname
    | Types.Repeat { elem; _ } -> if any_real_where elem then fail fname
    | Types.Single_elem { elem; _ } -> if any_real_where elem then fail fname
    | Types.Optional { inner; _ } -> if any_real_where inner then fail fname
    | Types.Optional_or { inner; _ } -> if any_real_where inner then fail fname
    | Types.Casetype { cases; _ } ->
        (* A [where] as a casetype case body projects to [case k: T { cond } v;],
           which is not valid 3D (a refinement is not allowed on a case body),
           unlike a top-level field refinement. Reject a non-trivial one. *)
        List.iter
          (fun (Types.Case_branch { cb_inner; _ }) ->
            if any_real_where cb_inner then fail fname)
          cases
    | _ -> ()
  in
  List.iter
    (fun (Types.Field f) ->
      check_typ (Option.value ~default:"<anon>" f.field_name) f.field_typ)
    fields

let seal : type r. (r, r) record -> r t =
 fun (Record r) ->
  let codec_id = Atomic.fetch_and_add id_counter 1 in
  let field_access = List.rev r.field_access_rev in
  let wire_size_info =
    match r.next_off with
    | Static n -> Fixed n
    | Dynamic f -> Variable { min_size = r.min_wire_size; compute = f }
  in
  let min_size = r.min_wire_size in
  let writers = Array.of_list (List.rev r.writers_rev) in
  let n_writers = Array.length writers in
  let raw_decode = build_decode r.make r.readers in
  let validators = List.rev r.validators_rev in
  let param_base = r.n_array_slots in
  (* Collect and index params *)
  let struct_fields = List.rev r.fields_rev in
  reject_greedy_not_last r.name struct_fields;
  reject_nested_where r.name struct_fields;
  let param_handles = collect_param_handles struct_fields r.where in
  let n_params = List.length param_handles in
  fill_param_slots r.param_slots param_base param_handles;
  let n_total = param_base + n_params in
  let compiled_where =
    compile_where_clause r.param_slots r.field_readers r.where
  in
  let validate_arr, populate, validate_checks =
    build_validators validators (List.rev r.checkers_rev) compiled_where
      struct_fields n_total
  in
  (* Per-field action runners *)
  let field_actions = List.rev r.field_actions_rev in
  let size_funcs = Array.of_list (List.rev r.size_of_value_rev) in
  let n_size_funcs = Array.length size_funcs in
  let size_of_value v =
    let total = Stdlib.ref 0 in
    for i = 0 to n_size_funcs - 1 do
      total := !total + size_funcs.(i) v
    done;
    !total
  in
  {
    id = codec_id;
    name = r.name;
    size_of_value;
    field_access;
    field_readers = List.rev r.field_readers;
    field_actions;
    decode = build_checked_decode raw_decode wire_size_info min_size;
    encode =
      (fun v buf off ->
        let need = size_of_value v in
        if off + need > Bytes.length buf then
          Fmt.invalid_arg "Codec.encode %s: buffer too short (need %d, got %d)"
            r.name need
            (Bytes.length buf - off);
        let write_off = Stdlib.ref off in
        for i = 0 to n_writers - 1 do
          write_off := writers.(i) v buf off !write_off
        done;
        !write_off);
    wire_size = wire_size_info;
    struct_fields;
    validate = validate_with_bounds wire_size_info min_size validate_checks;
    validate_arr;
    populate;
    n_array_slots = n_total;
    decode_scratch = domain_local_slots n_total;
    param_base;
    n_params;
    param_handles;
    where = r.where;
    doc = r.doc;
  }

(* -- Validator-only path: build a struct validator from [Types.struct_]
      without requiring a record constructor. Reuses the same int-array
      validation kernel ([compile_field], [build_validators]) that
      [Codec.v]/[Codec.decode] uses; only the writer/reader projections
      are skipped. Lets [Wire.of_string]/[Wire.decode] for [Struct]
      types share one code path with [Codec.decode]. *)

type validator = {
  min_size : int;
  wire_size : wire_size_info;
  validate : bytes -> int -> unit;
}

(* Internal accumulator -- the validator-relevant subset of [record]. *)
type validator_acc = {
  validators_rev : (int * (slots -> bytes -> int -> unit)) list;
  checkers_rev : (int * (slots -> bytes -> int -> unit)) list;
  field_readers : field_reader list;
  n_fields : int;
  n_array_slots : int;
  min_size : int;
  next_off : next_off;
  bf : bf_codec_state option;
  param_slots : (string, int) Hashtbl.t;
}

let empty_validator_acc () =
  {
    validators_rev = [];
    checkers_rev = [];
    field_readers = [];
    n_fields = 0;
    n_array_slots = 0;
    min_size = 0;
    next_off = Static 0;
    bf = None;
    param_slots = Hashtbl.create 4;
  }

let layout_ctx_of_validator_acc acc =
  {
    next_off = acc.next_off;
    bf = acc.bf;
    field_readers = acc.field_readers;
    n_fields = acc.n_fields;
  }

(* Per-field step: open [Types.Field]'s existential ['a], build a fake
   [('a, unit) field] with an [assert false] projection (never invoked on
   the validator path), call [compile_field], and accumulate the
   validator-relevant pieces of the resulting [compiled_field].

   [Struct]-typed fields are handled specially -- [compile_field] does
   not support them (a struct has no [field_wire_size] without its inner
   fields being walked). For nested structs we build a sub-validator
   recursively and inline its [validate] at the right offset. Inner
   field references stay scoped to the inner struct, matching the old
   [parse_struct_fields] semantics. *)
(* Common access patterns over [next_off]: a fresh-buffer offset
   function and a "previous end" helper that hides the static/dynamic
   split. *)
let acc_off_fn (acc : validator_acc) : bytes -> int -> int =
  match acc.next_off with
  | Static n -> fun _buf _base -> n
  | Dynamic f -> fun buf base -> f buf base - base

let acc_prev_end (acc : validator_acc) : bytes -> int -> int =
  match acc.next_off with
  | Static n -> fun _buf base -> base + n
  | Dynamic f -> f

(* Validator step for a [Struct]-typed field. [compile_field] doesn't
   accept [Struct] (it has no [field_wire_size] without walking inner
   fields), so build a sub-validator recursively and inline its
   [validate] at the right offset. *)
(* Build the [full] / [check_only] per-field validator functions and
   return the action var count. Takes only the non-existential fields
   of [compiled_field] ([populate], [validator_off]) so the caller can
   open the [Types.Field] existential, call [compile_field], and pass
   the relevant pieces here without leaking the field's ['a]. *)
let build_field_checks acc ~populate ~validator_off ~name ~action ~constraint_ =
  let action_vanames =
    match action with
    | None -> []
    | Some (Types.Success stmts | Types.Act stmts) ->
        List.fold_left action_vars [] stmts
  in
  let dummy_reader _buf _base = 0 in
  let cc_readers =
    let base = (name, dummy_reader) :: acc.field_readers in
    List.fold_left
      (fun acc' vn -> (vn, dummy_reader) :: acc')
      base action_vanames
  in
  let idx = build_idx cc_readers in
  let cc =
    mk_ctx ~sizeof_this:validator_off ~field_pos:acc.n_fields
      ~param_slots:acc.param_slots idx
  in
  let check = Option.map (compile_bool_arr cc) constraint_ in
  let act = compile_action cc action in
  let raise_check_failed arr base =
    raise_field_constraint ~field_idx:acc.n_fields ~at:base arr
  in
  let full arr buf base =
    populate arr buf base;
    (match check with
    | Some f when not (f arr) -> raise_check_failed arr base
    | _ -> ());
    Option.iter (fun f -> f arr) act
  in
  let check_only arr buf base =
    populate arr buf base;
    match check with
    | Some f when not (f arr) -> raise_check_failed arr base
    | _ -> ()
  in
  (full, check_only, List.length action_vanames)

let rec apply_struct_field acc inner_struct =
  let inner_v = validator_of_struct inner_struct in
  let static_off = match acc.next_off with Static n -> n | Dynamic _ -> -1 in
  let off_fn = acc_off_fn acc in
  let validator _arr buf base = inner_v.validate buf (base + off_fn buf base) in
  let prev_end = acc_prev_end acc in
  let size_delta, next_off =
    match (acc.next_off, inner_v.wire_size) with
    | Static n, Fixed sz -> (sz, Static (n + sz))
    | _, Fixed sz -> (sz, Dynamic (fun buf base -> prev_end buf base + sz))
    | _, Variable { min_size; compute } ->
        (min_size, Dynamic (fun buf base -> compute buf (prev_end buf base)))
  in
  {
    validators_rev = (static_off, validator) :: acc.validators_rev;
    checkers_rev = (static_off, validator) :: acc.checkers_rev;
    field_readers = acc.field_readers;
    n_fields = acc.n_fields;
    n_array_slots = acc.n_array_slots;
    min_size = acc.min_size + size_delta;
    next_off;
    bf = None;
    param_slots = acc.param_slots;
  }

and apply_field_to_validator_acc acc (Types.Field f) =
  match f.field_typ with
  | Types.Struct inner_struct -> apply_struct_field acc inner_struct
  | _ ->
      let name = Option.value f.field_name ~default:"" in
      let codec_field : (_, unit) field =
        {
          name;
          typ = f.field_typ;
          constraint_ = f.constraint_;
          action = f.action;
          doc = f.field_doc;
          get = (fun () -> assert false);
        }
      in
      let layout = layout_ctx_of_validator_acc acc in
      let cf = compile_field layout codec_field in
      let full, check_only, n_extra_vars =
        build_field_checks acc ~populate:cf.populate
          ~validator_off:cf.validator_off ~name ~action:f.action
          ~constraint_:f.constraint_
      in
      let new_field_readers =
        cf.nested_readers @ ((name, cf.int_reader) :: acc.field_readers)
      in
      {
        validators_rev = (cf.validator_off, full) :: acc.validators_rev;
        checkers_rev = (cf.validator_off, check_only) :: acc.checkers_rev;
        field_readers = new_field_readers;
        n_fields = List.length new_field_readers;
        n_array_slots = List.length new_field_readers + n_extra_vars;
        min_size = acc.min_size + cf.size_delta;
        next_off = cf.next_off;
        bf = cf.bf_after;
        param_slots = acc.param_slots;
      }

and validator_of_struct (s : Types.struct_) : validator =
  let acc =
    List.fold_left apply_field_to_validator_acc (empty_validator_acc ())
      s.fields
  in
  let wire_size_info =
    match acc.next_off with
    | Static n -> Fixed n
    | Dynamic f -> Variable { min_size = acc.min_size; compute = f }
  in
  let param_handles = collect_param_handles s.fields s.where in
  let n_params = List.length param_handles in
  let param_base = acc.n_array_slots in
  fill_param_slots acc.param_slots param_base param_handles;
  let n_total = param_base + n_params in
  let compiled_where =
    compile_where_clause acc.param_slots acc.field_readers s.where
  in
  let validate_arr, _populate, _validate =
    build_validators
      (List.rev acc.validators_rev)
      (List.rev acc.checkers_rev)
      compiled_where s.fields n_total
  in
  (* Full validation: fire field actions and check the where clause.
     [build_validators]'s third return [validate] is checkers-only (no
     actions). [validate_arr] runs full validators against a per-domain
     scratch, zeroed on entry. *)
  let get_arr = domain_local_slots n_total in
  let validate buf off =
    let arr = get_arr () in
    if n_total > 0 then clear_slots arr n_total;
    validate_arr arr buf off
  in
  { min_size = acc.min_size; wire_size = wire_size_info; validate }

let validate_struct (v : validator) buf off = v.validate buf off

let struct_size_of (v : validator) buf off =
  match v.wire_size with
  | Fixed n -> n
  | Variable { compute; _ } -> compute buf off - off

let struct_min_size (v : validator) = v.min_size

let wire_size_info_of_validator (v : validator) =
  match v.wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } -> `Variable compute

(* Heterogeneous field list. [] seals the view; (::) adds a field.
   Tracks the constructor type: ('a -> 'b -> 'r, 'r) matches a
   constructor (fun a b -> ...). *)
type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

let view : type f r.
    string -> ?where:bool expr -> ?doc:string -> f -> (f, r) fields -> r t =
 fun name ?where ?doc constructor flds ->
  let rec add : type g. (g, r) record -> (g, r) fields -> r t =
   fun r flds ->
    match flds with [] -> seal r | f :: rest -> add (add_field r f) rest
  in
  add (record_start ?where ?doc name constructor) flds

let v = view

let wire_size (t : _ t) =
  match t.wire_size with
  | Fixed n -> n
  | Variable _ ->
      invalid_arg
        "Codec.wire_size: variable-size codec (use min_wire_size or \
         compute_wire_size instead)"

let min_wire_size (t : _ t) =
  match t.wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size

let wire_size_at (t : _ t) buf off =
  match t.wire_size with
  | Fixed n -> n
  | Variable { compute; _ } -> compute buf off - off

let size_of_value (t : _ t) v = t.size_of_value v

let is_fixed (t : _ t) =
  match t.wire_size with Fixed _ -> true | Variable _ -> false

let raw_decode (t : _ t) buf off = t.decode buf off

(* Copy each input param's env value into its [cell]. The encode
   closures read [Param_ref p] via [!(p.cell ())] (see [bytes_leaves]
   in [compile_expr]), so the cells are the runtime backing for
   parametric field sizes. Mirror of the env -> array blit in
   [decode_exn]. *)
let load_env_into_cells (t : 'r t) (env : Param.env) =
  (* The env slot of [param_handles.(i)] is [i] (the env is built in the same
     order, see [env] below). *)
  List.iteri
    (fun i (Param.Pack p) -> p.cell () := env.slots.(i))
    t.param_handles

(* Reject [encode] on a parametric codec without an env, and reject envs
   that left an input param unbound. Either case would silently resolve
   parametric sizes to 0 (the cell init value), producing zero-byte
   regions for byte_array / byte_slice / uint_var fields and writing the
   rest of the record at the wrong offsets. *)
let unbound_params (t : 'r t) (env : Param.env) : string list =
  List.mapi
    (fun i (Param.Pack p) ->
      if (not p.Types.mutable_) && not env.bound.(i) then Some p.name else None)
    t.param_handles
  |> List.filter_map Fun.id

(* Input params (read from the env) drive field sizes/offsets, so encoding or
   decoding without their values would resolve those to 0. Output params are
   written by decode-side actions and never read on encode, and on decode an
   absent env just means the caller does not want them back, so a codec whose
   only params are outputs needs no env. *)
let has_input_params (t : 'r t) =
  List.exists (fun (Param.Pack p) -> not p.Types.mutable_) t.param_handles

let require_env ~op t = function
  | None when not (has_input_params t) -> ()
  | None ->
      Fmt.invalid_arg
        "Codec.%s: codec %s has input params; pass ?env (e.g. [Codec.env c |> \
         Param.bind p N])."
        op t.name
  | Some env -> (
      match unbound_params t env with
      | [] -> ()
      | missing ->
          Fmt.invalid_arg
            "Codec.%s: codec %s has unbound input params [%s]; bind every one \
             before use."
            op t.name
            (String.concat ", " missing))

let raw_encode ?env:e t v buf off =
  require_env ~op:"encode" t e;
  (match e with Some env -> load_env_into_cells t env | None -> ());
  t.encode v buf off

(* Encode a sub-codec embedded as a field/element. The enclosing codec has
   already seeded every input param's [cell] from its own env (its
   [param_handles] include the sub's, see [iter_param_refs_typ]), so the sub
   must not re-run [require_env] (it has no env of its own here). *)
let embed_encode (t : 'r t) v buf off = t.encode v buf off

(* Decode a sub-codec embedded as a field/element, enforcing its [where] and
   field constraints (the plain field-reader decode skips them). Param values
   come from the [cell]s the enclosing codec seeded; copy them into the
   sub's per-decode slots so its constraints resolve correctly. *)
let embed_decode (t : 'r t) buf off =
  let v = t.decode buf off in
  let env_slots =
    Array.of_list
      (List.map (fun (Param.Pack p) -> !(p.Types.cell ())) t.param_handles)
  in
  t.validate ~env_slots buf off;
  v

let wire_size_info (t : _ t) =
  match t.wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } -> `Variable (fun buf off -> compute buf off - off)

let env (t : _ t) : Param.env =
  {
    Types.codec_id = t.id;
    names =
      Array.of_list
        (List.map (fun (Param.Pack p) -> p.Types.name) t.param_handles);
    slots = Array.make t.n_params 0;
    bound = Array.make t.n_params false;
  }

let decode_exn ?env:e t buf off =
  (* Require an env that binds every input param, as encode does: an unbound
     input param resolves a parametric size to 0, silently truncating the field
     and misaligning the rest of the record. This is a usage error, not malformed
     input, so it raises [Invalid_argument] rather than returning a parse error. *)
  require_env ~op:"decode" t e;
  (* Seed the input cells from the env before reading fields: the field
     readers resolve [Param_ref p] via [!(p.cell ())], so a parametric size
     (byte_array / byte_slice / uint_var) must see the env's value. Mirror of
     the seeding [encode] does. [Param.bind] also writes the cell directly, but
     [Param.bind_by_name] only has the name and writes [env.slots]; without
     this it would read as 0 and silently truncate the field. *)
  (match e with
  | Some env -> load_env_into_cells t env
  | None -> ());
  let v = t.decode buf off in
  let arr = t.decode_scratch () in
  clear_slots arr t.n_array_slots;
  (match e with
  | Some (e : Param.env) when t.n_params > 0 ->
      Array.blit e.slots 0 arr.ints t.param_base t.n_params
  | _ -> ());
  t.validate_arr arr buf off;
  (match e with
  | Some (e : Param.env) ->
      (* Array slot of [param_handles.(i)] is [param_base + i]; its env slot is
         [i]. Write decoded output params back into the env (and the cell). *)
      List.iteri
        (fun i (Param.Pack p) ->
          let v = arr.ints.(t.param_base + i) in
          e.slots.(i) <- v;
          p.cell () := v)
        t.param_handles
  | None -> ());
  v

let decode ?env t buf off =
  try Ok (decode_exn ?env t buf off) with Types.Parse_error e -> Error e

let encode ?env:e t v buf off =
  require_env ~op:"encode" t e;
  (match e with Some env -> load_env_into_cells t env | None -> ());
  let expected = t.size_of_value v in
  let actual = t.encode v buf off - off in
  (* Underrun = silent data corruption: the trailing bytes the caller
     allocated stay uninitialised and the decoder reads them as part
     of the value. Overrun is loud already. *)
  if actual < expected then
    Fmt.invalid_arg
      "Codec.encode %s: size_of_value reported %d bytes but the encoded form \
       spans %d -- writer wrote fewer bytes than promised"
      t.name expected actual

let collect_params (fields : Types.field list) where =
  collect_param_handles fields where
  |> List.map (fun (Param.Pack p) ->
      { param_name = p.name; param_typ = p.packed_typ; mutable_ = p.mutable_ })

let to_struct t =
  let formals = collect_params t.struct_fields t.where in
  match (formals, t.where) with
  | [], None -> struct' t.name t.struct_fields
  | _ -> param_struct t.name formals ?where:t.where t.struct_fields

let validate ?env (t : _ t) buf off =
  let env_slots = Option.map (fun (e : Param.env) -> e.slots) env in
  t.validate ?env_slots buf off

(* Build a staged reader from field type and access info.
   For Fixed: use build_field_reader which handles Where/Enum/Map.
   For Bitfield: the GADT ensures 'a = int via Bits constructor.
   For Dynamic: compute offset at read time. *)
let rec build_staged_reader : type a. a typ -> field_access -> bytes -> int -> a
    =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      build_bf_reader base byte_off shift width
  | _, Fixed off -> build_field_reader typ off
  | _, Dynamic fn ->
      let reader_at_0 = build_field_reader typ 0 in
      fun buf base ->
        let off = fn buf base in
        reader_at_0 buf (base + off)
  | Byte_slice _, Variable { off; size_fn } ->
      fun buf base ->
        let sz = size_fn buf base in
        Slice.make_or_eod buf ~first:(base + off) ~length:sz
  | Byte_array _, Variable { off; size_fn } ->
      fun buf base ->
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + off) sz
  | Byte_array_where _, Variable { off; size_fn } ->
      fun buf base ->
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + off) sz
  | Byte_slice _, Variable_dynamic { off_fn; size_fn } ->
      fun buf base ->
        let fo = off_fn buf base in
        let sz = size_fn buf base in
        Slice.make_or_eod buf ~first:(base + fo) ~length:sz
  | Byte_array _, Variable_dynamic { off_fn; size_fn } ->
      fun buf base ->
        let fo = off_fn buf base in
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + fo) sz
  | Byte_array_where _, Variable_dynamic { off_fn; size_fn } ->
      fun buf base ->
        let fo = off_fn buf base in
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + fo) sz
  | Where { inner; _ }, _ -> build_staged_reader inner access
  | Enum { base; cases; closed; _ }, _ ->
      let read = build_staged_reader base access in
      let check = enum_check cases closed in
      fun buf base -> check ~at:base (read buf base)
  | Map { inner; decode; _ }, _ ->
      let read = build_staged_reader inner access in
      fun buf base -> decode (read buf base)
  | _, Bitfield _ ->
      invalid_arg "Codec.get: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.get: unsupported variable-size field type"

(* Build a staged writer from field type and access info. *)
let rec build_staged_writer : type a.
    a typ -> field_access -> bytes -> int -> a -> unit =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      build_bf_accessor_writer base byte_off shift width
  | _, Fixed off ->
      let enc = build_field_encoder typ in
      fun buf base value ->
        let _ = enc buf (base + off) value in
        ()
  | _, Dynamic fn ->
      let enc = build_field_encoder typ in
      fun buf base value ->
        let off = fn buf base in
        let _ = enc buf (base + off) value in
        ()
  | Byte_slice _, Variable { off; _ } ->
      fun buf base value ->
        let src = (value : Slice.t) in
        let len = Slice.length src in
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + off) len
  | Byte_array _, Variable { off; _ } ->
      fun buf base value ->
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + off) (String.length s)
  | Byte_array_where _, Variable { off; _ } ->
      fun buf base value ->
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + off) (String.length s)
  | Byte_slice _, Variable_dynamic { off_fn; _ } ->
      fun buf base value ->
        let fo = off_fn buf base in
        let src = (value : Slice.t) in
        let len = Slice.length src in
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + fo) len
  | Byte_array _, Variable_dynamic { off_fn; _ } ->
      fun buf base value ->
        let fo = off_fn buf base in
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + fo) (String.length s)
  | Byte_array_where _, Variable_dynamic { off_fn; _ } ->
      fun buf base value ->
        let fo = off_fn buf base in
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + fo) (String.length s)
  | Where { inner; _ }, _ -> build_staged_writer inner access
  | Enum { base; _ }, _ -> build_staged_writer base access
  | Map { inner; encode; _ }, _ ->
      let write = build_staged_writer inner access in
      fun buf base value -> write buf base (encode value)
  | _, Bitfield _ ->
      invalid_arg "Codec.set: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.set: unsupported variable-size field type"

let field_access (codec : _ t) name =
  match List.assoc_opt name codec.field_access with
  | Some a -> a
  | None ->
      Fmt.invalid_arg "Codec: field %S not found in codec %S" name codec.name

let[@inline] get (type a r) ?env (codec : r t) (f : (a, r) field) :
    (bytes -> int -> a) Staged.t =
  let access = field_access codec f.name in
  let read = build_staged_reader f.typ access in
  match List.assoc_opt f.name codec.field_actions with
  | None -> Staged.stage read
  | Some act ->
      (* Reuse the codec's own domain-local scratch rather than minting another
         [Domain.DLS] key here: [get] returns a staged reader, but a caller that
         re-stages instead of reusing would leak a key per call. [decode_scratch]
         is sized [n_array_slots] and only ever touched by one operation at a
         time on a given domain, so sharing it with decode is safe. *)
      let get_arr = codec.decode_scratch in
      let n = codec.n_array_slots in
      let populate = codec.populate in
      let n_params = codec.n_params in
      let sync, blit_input =
        match env with
        | None -> ((fun _arr -> ()), fun _arr -> ())
        | Some (e : Param.env) ->
            if e.codec_id <> codec.id then
              Fmt.invalid_arg
                "Codec.get: env was not created by Codec.env for %S" codec.name;
            let param_handles = codec.param_handles in
            let param_base = codec.param_base in
            ( (fun arr ->
                List.iteri
                  (fun i (Param.Pack p) ->
                    let v = arr.ints.(param_base + i) in
                    e.slots.(i) <- v;
                    p.cell () := v)
                  param_handles),
              fun arr ->
                if n_params > 0 then
                  Array.blit e.slots 0 arr.ints param_base n_params )
      in
      Staged.stage (fun buf off ->
          let v = read buf off in
          let arr = get_arr () in
          clear_slots arr n;
          blit_input arr;
          populate arr buf off;
          act arr;
          sync arr;
          v)

let[@inline] set (type a r) (codec : r t) (f : (a, r) field) :
    (bytes -> int -> a -> unit) Staged.t =
  let access = field_access codec f.name in
  Staged.stage (build_staged_writer f.typ access)

let name (t : _ t) = t.name
let rename new_name (t : _ t) = { t with name = new_name }
let doc (t : _ t) = t.doc
let field_readers (t : _ t) = t.field_readers
let pp ppf (t : _ t) = Fmt.string ppf t.name
let field_ref (type a r) (f : (a, r) field) : int expr = Ref (I, f.name)

(* -- Slice navigation: zero-copy access to the offset/length of a
      [Byte_slice] field. Used to descend into a nested codec without
      allocating a [Slice.t]. The type signature constrains the field's
      payload type to [Slice.t], so passing a non-slice field is a
      compile-time error. -- *)

(* All access flavours produce an absolute byte offset for a slice field:
   - [Fixed off]: byte_slice with static size (and no preceding variable
     fields) -- offset is [base + off].
   - [Dynamic fn]: byte_slice with static size after a variable field --
     offset is [base + fn buf base].
   - [Variable { off; _ }]: byte_slice with variable size, statically
     positioned -- offset is [base + off].
   - [Variable_dynamic { off_fn; _ }]: variable size, dynamic position. *)
let[@inline] slice_offset (type r) (codec : r t) (f : (Slice.t, r) field) :
    (bytes -> int -> int) Staged.t =
  match field_access codec f.name with
  | Fixed off -> Staged.stage (fun _buf base -> base + off)
  | Dynamic fn -> Staged.stage (fun buf base -> base + fn buf base)
  | Variable { off; _ } -> Staged.stage (fun _buf base -> base + off)
  | Variable_dynamic { off_fn; _ } ->
      Staged.stage (fun buf base -> base + off_fn buf base)
  | Bitfield _ ->
      Fmt.invalid_arg
        "Codec.slice_offset: field %S is a bitfield, not a byte slice" f.name

let[@inline] slice_length (type r) (codec : r t) (f : (Slice.t, r) field) :
    (bytes -> int -> int) Staged.t =
  match (f.typ, field_access codec f.name) with
  | Byte_slice { size }, Fixed _ | Byte_slice { size }, Dynamic _ -> (
      (* Static-size byte_slice: size is a constant expression. *)
      match size with
      | Int n -> Staged.stage (fun _buf _base -> n)
      | _ ->
          Fmt.invalid_arg
            "Codec.slice_length: field %S has dynamic size in a static access \
             -- internal inconsistency"
            f.name)
  | _, Variable { size_fn; _ } | _, Variable_dynamic { size_fn; _ } ->
      Staged.stage size_fn
  | _, (Fixed _ | Dynamic _) ->
      Fmt.invalid_arg "Codec.slice_length: field %S is not a byte slice" f.name
  | _, Bitfield _ ->
      Fmt.invalid_arg
        "Codec.slice_length: field %S is a bitfield, not a byte slice" f.name

(* -- Bitfield batch access -- *)

type bitfield = bf_info

let bitfield (type r) (codec : r t) (f : (int, r) field) : bitfield =
  match field_access codec f.name with
  | Bitfield { base; byte_off; shift; width } ->
      (* Dispatch on [base] once at construction so the resulting closure
         is a direct read of the right width with [byte_off] baked in.
         Avoids the partial-application + runtime [match] that
         [Bitfield.read_word base] would create. *)
      let word_reader =
        match base with
        | U8 -> fun buf off -> Bytes.get_uint8 buf (off + byte_off)
        | U16 Little -> fun buf off -> Bitfield.u16_le buf (off + byte_off)
        | U16 Big -> fun buf off -> Bitfield.u16_be buf (off + byte_off)
        | U32 Little -> fun buf off -> Bitfield.u32_le buf (off + byte_off)
        | U32 Big -> fun buf off -> Bitfield.u32_be buf (off + byte_off)
      in
      let mask = (1 lsl width) - 1 in
      { word_reader; packed = shift lor (mask lsl 8) }
  | _ -> Fmt.invalid_arg "Codec.bitfield: field %S is not a bitfield" f.name

let load_word (bf : bitfield) : (bytes -> int -> int) Staged.t =
  Staged.stage bf.word_reader

let[@inline always] extract (bf : bitfield) word =
  let p = bf.packed in
  (word lsr (p land 0xFF)) land (p lsr 8)

(* -- Snapshot: batch bitfield access -- *)
