open Types
open Codec_leaf

let int_of_typ_value = Eval.int_of_exn

type slots = { ints : int array; int64s : int64 array }
type runtime = Types.eval_ctx

let alloc_slots n = { ints = Array.make n 0; int64s = Array.make n 0L }

let clear_slots s n =
  Array.fill s.ints 0 n 0;
  Array.fill s.int64s 0 n 0L

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
}

let no_runtime = Types.unbound_eval_ctx

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
      ('full, 'a -> 'rest) readers
      * (runtime -> Input_end.t -> bytes -> int -> 'a)
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

let compile_bool_arr cc e =
  let f = Expr_compiler.compile_bool (array_leaves cc) e in
  fun arr -> f arr () () ()

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

type ('f, 'r) record =
  | Record : {
      name : string;
      make : 'full;
      readers : ('full, 'f) readers;
      writers_rev : ('r -> runtime -> bytes -> int -> int -> int) list;
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
      validators_rev :
        (int (* byte offset *)
        * (slots -> runtime -> Input_end.t -> bytes -> int -> unit))
        list;
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

(* What [seal] builds from a field list: the layout, the readers and writers
   and the validators. [view] does not build it; the first operation that needs
   it does (see [compiled]). *)
type 'r compiled = {
  size_of_value : Types.eval_ctx -> 'r -> int;
  field_access : (string * (int * field_access)) list;
  field_readers : field_reader list;
  field_actions : (string * compiled_action) list;
  decode : runtime -> Input_end.t -> bytes -> int -> 'r;
  encode : 'r -> runtime -> bytes -> int -> int;
      (* Public encode entry. [encode v runtime buf base] writes the record at
         [base], threads each per-field writer's return as the next
         writer's [write_off], and returns the final offset. *)
  wire_size : wire_size_info;
  validate : ?env_slots:int array -> bytes -> int -> unit;
  validate_ctx : runtime -> Input_end.t -> bytes -> int -> unit;
      (* [validate] for a codec embedded in another: the same bounds check and
         the same field pass, with parameters resolved by name against the
         caller's context rather than a [Param.env]. This is what a use site of
         the codec runs, so a sub-codec's checks are enforced wherever it
         appears. *)
  validate_arr : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  n_array_slots : int;
  decode_scratch : unit -> slots;
      (* Validation scratch, one per domain (see [domain_local_slots]) and
         zeroed per decode (see [decode_exn]), mirroring the [validate]
         closure, the struct validator, and [Codec.get]'s staged reader.
         Domain-local so concurrent decode of one codec value on separate
         domains does not share the scratch. *)
  param_base : int;
}

type 'r t = {
  id : int;
  name : string;
  struct_fields : Types.field list;
  n_params : int;
  param_handles : Param.packed list;
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
      writers_rev = [];
      size_of_value_rev = [];
      min_wire_size = 0;
      next_off = Static 0;
      fixed_prefix = Stdlib.ref 0;
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
