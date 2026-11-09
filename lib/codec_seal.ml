open Types
open Codec_leaf
open Codec_repr
open Codec_elem
open Codec_compile

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
  | Types.Uint8 | Types.Uint16 _ | Types.Uint32 _ | Types.Uint64 _ | Types.Int8
  | Types.Int16 _ | Types.Int32 _ | Types.Int64 _ | Types.Float32 _
  | Types.Float64 _ | Types.Uint_var _ | Types.Bits _ | Types.Byte_array _
  | Types.Byte_slice _ | Types.All_bytes | Types.Unit ->
      false
  | Types.Enum { closed; _ } -> closed
  | Types.Map { inner; index_bound; _ } ->
      index_bound <> None || field_reader_validates inner
  | Types.Optional { inner; _ } -> field_reader_validates inner
  | Types.Optional_or { inner; _ } -> field_reader_validates inner
  | Types.Single_elem { elem; _ } -> field_reader_validates elem
  | Types.Array { elem; _ } -> field_reader_validates elem
  (* A [repeat] checks its own byte budget whatever the element does: the
     budget must fit the buffer and split into whole elements. The record's
     bounds check covers the span but not the split, so this cannot recurse
     into [elem]. *)
  | Types.Repeat _ -> true
  | _ -> true

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

(* The bounds check a field's read needs before it touches the buffer, or
   [None] when the read guards itself. Such a read can be asked for a field the
   buffer does not reach: an earlier variable-size field pushes it past the end,
   or the enclosing record is short. Checking the field's byte extent first
   reports truncation as end-of-input at the span that was missing.

   Only accesses with a known extent get a guard. A variable-width field reads
   through a span check of its own ([var_bytes_reader], or [check_span_bounds]
   directly for a [uint_var] of computed width). A conditional optional has
   no extent here -- [field_wire_size] is [None] for it, since an absent one
   occupies nothing -- so it carries its own presence-gated check instead, in
   [present_in_bounds]. *)
let read_guard : type a r.
    (a, r) compiled_field ->
    a typ ->
    (runtime -> Input_end.t -> bytes -> int -> unit) option =
 fun cf typ ->
  match cf.field_access with
  | Bitfield { base = word; byte_off; _ } ->
      let width = Bitfield.byte_size word in
      Some
        (fun _runtime input_end _buf base ->
          check_read_bounds input_end ~at:(base + byte_off) ~width)
  | Fixed off -> (
      match field_wire_size typ with
      | Some width when width = cf.size_delta ->
          Some
            (fun _runtime input_end _buf base ->
              check_read_bounds input_end ~at:(base + off) ~width)
      | _ -> None)
  | Dynamic off_fn -> (
      match field_wire_size typ with
      | Some width when width = cf.size_delta ->
          Some
            (fun runtime input_end buf base ->
              check_read_bounds input_end
                ~at:(base + off_fn runtime input_end buf base)
                ~width)
      | _ -> None)
  | Variable _ | Variable_dynamic _ -> None

(* A fixed-width field after a variable one is the only record reader whose
   position can outrun the buffer without the whole-record bounds check seeing
   it: a trailing greedy field can resolve the final end back to the buffer
   length. Static reads are covered by [min_wire_size], while variable reads
   guard their own spans, so keep their hot paths unchanged. *)
let guarded_dynamic_reader : type a r.
    (a, r) compiled_field ->
    a typ ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    a =
 fun cf typ ->
  let reader = cf.raw_reader in
  match cf.field_access with
  | Dynamic _ -> (
      match read_guard cf typ with
      | None -> reader
      | Some guard ->
          fun runtime input_end buf base ->
            guard runtime input_end buf base;
            reader runtime input_end buf base)
  | Fixed _ | Bitfield _ | Variable _ | Variable_dynamic _ -> reader

(* Size, offset, presence and [where] expressions read earlier fields through
   these readers. *)
let expr_reader : type a r. (a, r) compiled_field -> a typ -> _ =
 fun cf typ ->
  let reader = cf.int_reader in
  match read_guard cf typ with
  | None -> reader
  | Some guard ->
      fun runtime input_end buf base ->
        guard runtime input_end buf base;
        reader runtime input_end buf base

(* [Codec.validate] reaches a field's bytes only through its populate: unlike
   decode, it never walks the record field by field, so a field pushed past the
   end by an overlong variable-size predecessor is read at its computed offset
   with nothing in between to notice. Guard it with the same extent check the
   expression readers use, so validating truncated input ends in a located
   end-of-input rather than a raw out-of-bounds [Invalid_argument]. *)
let guarded_populate : type a r.
    (a, r) compiled_field ->
    a typ ->
    slots ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    unit =
 fun cf typ ->
  let populate = cf.populate in
  match read_guard cf typ with
  | None -> populate
  | Some guard ->
      fun arr runtime input_end buf base ->
        guard runtime input_end buf base;
        populate arr runtime input_end buf base

(* A field's two validators. [check_only] fills the field's int slot and tests
   its constraint; [full] adds the field action. Decode and [Codec.validate]
   both run [full], so an action never fires on one path only. *)
let field_validators ~field_idx ~validator_off ~populate ~check ~act =
  let check_only arr runtime input_end buf base =
    populate arr runtime input_end buf base;
    let at = at_of base validator_off in
    match check with
    | Some f when not (eval_arr_at ~at f arr) ->
        raise_field_constraint ~field_idx ~at arr
    | _ -> ()
  in
  let full arr runtime input_end buf base =
    check_only arr runtime input_end buf base;
    match act with
    | Some f -> eval_arr_at ~at:(at_of base validator_off) f arr
    | None -> ()
  in
  (check_only, full)

(* The record's minimum size after [cf]: what [decode] checks the buffer
   against before its unchecked reads, so it only grows, and stays a byte count
   a buffer can hold. *)
let add_min_size record field min_size cf =
  if cf.size_delta < 0 || min_size > Sys.max_string_length - cf.size_delta then
    Fmt.invalid_arg
      "Codec %s: field %s takes the record past the largest buffer size" record
      field;
  min_size + cf.size_delta

(* Bitfields share a base word; [cf.size_delta] is the correct non-overlapping
   contribution. Key this off the compiled access rather than the field's type:
   wrapped bitfields such as [bit (bits ...)] arrive here as [Map]/[Where]/[Enum]
   even though they still pack into the current word. *)
let field_size_of_value (fld : (_, _) field) cf =
  match cf.field_access with
  | Bitfield _ -> fun _ _ -> cf.size_delta
  | _ -> fun ctx v -> size_of_typ_value ctx fld.typ (fld.get v)

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
  let dummy_reader _runtime _input_end _buf _base = 0 in
  let cc_readers =
    let base = extend_scope fld.name dummy_reader [] r.field_readers in
    List.fold_left
      (fun acc vn -> (Some vn, dummy_reader) :: acc)
      base action_vanames
  in
  let idx = build_idx cc_readers in
  let cc =
    mk_ctx ~sizeof_this:r.fixed_prefix ~field_pos:field_idx
      ~param_slots:r.param_slots idx
  in
  let check = compile_field_check cc fld in
  let act = compile_action cc fld.action in
  let check_only, full =
    field_validators ~field_idx ~validator_off:cf.validator_off
      ~populate:(guarded_populate cf fld.typ)
      ~check ~act
  in
  let faction =
    match act with None -> None | Some act_fn -> Some (fld.name, act_fn)
  in
  let byte_off = cf.validator_off in
  let new_writers_rev = (cf.raw_writer :: cf.extra_writers) @ r.writers_rev in
  let new_field_readers =
    extend_scope fld.name (expr_reader cf fld.typ) cf.nested_readers
      r.field_readers
  in
  let field_size_of_value = field_size_of_value fld cf in
  let raw_reader = guarded_dynamic_reader cf fld.typ in
  let validates = field_reader_validates fld.typ in
  let raw_reader, full, check_only =
    wrap_field_errors ~validates ~check ~act fld.name raw_reader full check_only
  in
  note_fixed_prefix ~before:r.next_off ~after:cf.next_off r.fixed_prefix;
  let min_wire_size = add_min_size r.name fld.name r.min_wire_size cf in
  Record
    {
      name = r.name;
      make = r.make;
      readers =
        Snoc
          ( r.readers,
            field_read ~min_wire_size cf.field_access fld.typ raw_reader
              ~plain:
                (Option.is_none check && Option.is_none act && not validates) );
      writers_rev = new_writers_rev;
      size_of_value_rev = field_size_of_value :: r.size_of_value_rev;
      min_wire_size;
      next_off = cf.next_off;
      fixed_prefix = r.fixed_prefix;
      validators_rev = (byte_off, full) :: r.validators_rev;
      checkers_rev = (byte_off, check_only) :: r.checkers_rev;
      field_actions_rev =
        (match faction with
        | Some fa -> fa :: r.field_actions_rev
        | None -> r.field_actions_rev);
      n_fields = List.length new_field_readers;
      n_array_slots = List.length new_field_readers + n_extra_vars;
      r_bf = cf.bf_after;
      field_access_rev =
        (fld.name, (fld.id, cf.field_access)) :: r.field_access_rev;
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
      'a field_read * ('rest, 'result) readers_fwd
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

(* Read every field and apply [make] to the values, in one saturated call for
   up to 32 fields; past 32, one partial application per extra 32. The reads
   are let-bound so they run in field order: a record that fails to read fails
   on its first unreadable field. One row per arity; the formatter would
   staircase the nested [FCons] patterns. *)
let rec apply_fwd : type mid result.
    mid ->
    (mid, result) readers_fwd ->
    runtime -> Input_end.t -> bytes -> int -> result =
 fun f fwd runtime input_end buf off ->
  match fwd with
  | FNil -> f
  | FCons (r1, FNil) ->
      let v1 = read_field r1 runtime input_end buf off in f v1
  | FCons (r1, FCons (r2, FNil)) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in f v1 v2
  | FCons (r1, FCons (r2, FCons (r3, FNil))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in f v1 v2 v3
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FNil)))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in f v1 v2 v3 v4
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FNil))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in f v1 v2 v3 v4 v5
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FNil)))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in f v1 v2 v3 v4 v5 v6
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FNil))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FNil)))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FNil))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FNil)))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FNil))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FNil)))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FNil))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FNil)))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FNil))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FNil)))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FNil))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FNil)))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FNil))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FNil)))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FNil))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FNil)))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FNil))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FNil)))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FNil))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FNil)))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FNil))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FNil)))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in let v28 = read_field r28 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FNil))))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in let v28 = read_field r28 runtime input_end buf off in let v29 = read_field r29 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FNil)))))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in let v28 = read_field r28 runtime input_end buf off in let v29 = read_field r29 runtime input_end buf off in let v30 = read_field r30 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FCons (r31, FNil))))))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in let v28 = read_field r28 runtime input_end buf off in let v29 = read_field r29 runtime input_end buf off in let v30 = read_field r30 runtime input_end buf off in let v31 = read_field r31 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FCons (r31, FCons (r32, FNil)))))))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in let v28 = read_field r28 runtime input_end buf off in let v29 = read_field r29 runtime input_end buf off in let v30 = read_field r30 runtime input_end buf off in let v31 = read_field r31 runtime input_end buf off in let v32 = read_field r32 runtime input_end buf off in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, FCons (r9, FCons (r10, FCons (r11, FCons (r12, FCons (r13, FCons (r14, FCons (r15, FCons (r16, FCons (r17, FCons (r18, FCons (r19, FCons (r20, FCons (r21, FCons (r22, FCons (r23, FCons (r24, FCons (r25, FCons (r26, FCons (r27, FCons (r28, FCons (r29, FCons (r30, FCons (r31, FCons (r32, rest)))))))))))))))))))))))))))))))) ->
      let v1 = read_field r1 runtime input_end buf off in let v2 = read_field r2 runtime input_end buf off in let v3 = read_field r3 runtime input_end buf off in let v4 = read_field r4 runtime input_end buf off in let v5 = read_field r5 runtime input_end buf off in let v6 = read_field r6 runtime input_end buf off in let v7 = read_field r7 runtime input_end buf off in let v8 = read_field r8 runtime input_end buf off in let v9 = read_field r9 runtime input_end buf off in let v10 = read_field r10 runtime input_end buf off in let v11 = read_field r11 runtime input_end buf off in let v12 = read_field r12 runtime input_end buf off in let v13 = read_field r13 runtime input_end buf off in let v14 = read_field r14 runtime input_end buf off in let v15 = read_field r15 runtime input_end buf off in let v16 = read_field r16 runtime input_end buf off in let v17 = read_field r17 runtime input_end buf off in let v18 = read_field r18 runtime input_end buf off in let v19 = read_field r19 runtime input_end buf off in let v20 = read_field r20 runtime input_end buf off in let v21 = read_field r21 runtime input_end buf off in let v22 = read_field r22 runtime input_end buf off in let v23 = read_field r23 runtime input_end buf off in let v24 = read_field r24 runtime input_end buf off in let v25 = read_field r25 runtime input_end buf off in let v26 = read_field r26 runtime input_end buf off in let v27 = read_field r27 runtime input_end buf off in let v28 = read_field r28 runtime input_end buf off in let v29 = read_field r29 runtime input_end buf off in let v30 = read_field r30 runtime input_end buf off in let v31 = read_field r31 runtime input_end buf off in let v32 = read_field r32 runtime input_end buf off in apply_fwd (f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32) rest runtime input_end buf off
[@@ocamlformat "disable"]

let lookup_reader_idx rev_readers name =
  let rec find i = function
    | [] -> unbound_where_ref name
    | (n, _) :: _ when n = Some name -> i
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

(* A field whose size the data decides: its own bounds check refuses a size
   that is negative or runs past the end, which the record's bounds check can
   miss when a later field that takes the rest gives the bytes back. A field
   that takes the rest has no size of its own to check. *)
let data_sized : type a. a Types.typ -> bool = function
  | Types.All_bytes -> false
  | typ -> Option.is_none (Types.field_wire_size typ)

let has_validation_checks compiled_where struct_fields =
  compiled_where <> None
  || List.exists
       (fun (Types.Field f) ->
         f.constraint_ <> None || f.action <> None
         || field_reader_validates f.field_typ
         || data_sized f.field_typ)
       struct_fields

let seed_param_slots arr n_total = function
  | None -> ()
  | Some slots ->
      let n = Array.length slots in
      Array.blit slots 0 arr.ints (n_total - n) n

(* A codec with no parameters gets the constant context: with an empty slot
   table the two lookups below can only answer 0 and drop the write, which is
   what an unbound context already does, and an unbound context is an immediate
   rather than two closures, a record and a block. The saving is per
   [Codec.validate] call, and a validate is what a server runs per packet. *)
let validation_runtime param_slots arr =
  if Hashtbl.length param_slots = 0 then no_runtime
  else
    Types.eval_ctx
      ~set_param:(fun name value ->
        match Hashtbl.find_opt param_slots name with
        | Some idx -> arr.ints.(idx) <- value
        | None -> ())
      (fun name ->
        match Hashtbl.find_opt param_slots name with
        | Some idx -> arr.ints.(idx)
        | None -> 0)

let build_validators validators_rev checkers_rev compiled_where struct_fields
    param_slots n_total =
  let validator_fns = Array.of_list (List.map snd validators_rev) in
  let n_validators = Array.length validator_fns in
  let validate_arr arr runtime input_end buf off =
    for i = 0 to n_validators - 1 do
      validator_fns.(i) arr runtime input_end buf off
    done;
    match compiled_where with
    | Some f when not (eval_arr_at ~at:off f arr) ->
        raise_constraint ~at:off ~which:Where ()
    | _ -> ()
  in
  let checker_fns = Array.of_list (List.map snd checkers_rev) in
  let n_checkers = Array.length checker_fns in
  let populate arr runtime input_end buf off =
    for i = 0 to n_checkers - 1 do
      checker_fns.(i) arr runtime input_end buf off
    done
  in
  (* Run the validator pass only when the struct actually has something to
     check; the structural bounds check runs unconditionally one layer up in
     [validate_with_bounds]. A codec of plain scalars and byte spans then
     validates with no allocation, so validating it on a hot read path (a
     [Codec.get] preceded by [validate]) stays cheap. *)
  let has_checks = has_validation_checks compiled_where struct_fields in
  let validate =
    if not has_checks then fun ?env_slots:_ _buf _off -> ()
    else
      let get_arr = domain_local_slots n_total in
      fun ?env_slots buf off ->
        let arr = get_arr () in
        clear_slots arr n_total;
        (* Seed param slots so [where] clauses referencing [Param.input]
           evaluate correctly. *)
        seed_param_slots arr n_total env_slots;
        let runtime = validation_runtime param_slots arr in
        validate_arr arr runtime (Input_end.of_bytes buf) buf off
  in
  (validate_arr, populate, validate)

let collect_param_handles codec_name struct_fields where =
  let seen = Hashtbl.create 4 in
  let handles = Stdlib.ref ([] : Param.packed list) in
  let visit (Param.Pack p as packed) =
    match Hashtbl.find_opt seen p.name with
    | None ->
        Hashtbl.add seen p.name packed;
        handles := packed :: !handles
    | Some (Param.Pack existing) ->
        if p.Types.id <> existing.Types.id then
          Fmt.invalid_arg "Codec.v %S: duplicate parameter name %S" codec_name
            p.name
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

let min_bounds_error min_size buf off =
  if off < 0 then Fmt.invalid_arg "Codec: negative offset %d" off;
  raise_eof ~at:off ~expected:min_size
    ~got:(bytes_from (Input_end.of_bytes buf) off)

(* The structural bounds check decode runs before reading any field: the buffer
   must hold at least [min_size] bytes, and the codec's resolved wire size must
   fit. Shared with [validate] so a constraint-free codec is still bounds-checked
   before a zero-copy [get] reads its fields. [decode] then reads the fixed
   fields with no check of their own, so the test cannot overflow, and its one
   call is on the error path, where nothing is live after it. *)
let check_min_bounds min_size buf off =
  if off < 0 || off > Bytes.length buf - min_size then
    min_bounds_error min_size buf off

(* The record ending at [end_off] holds [min_size] bytes and fits the buffer. *)
let check_end_bounds min_size buf off end_off =
  if end_off < off + min_size then
    raise_eof ~at:off ~expected:min_size
      ~got:(bytes_from (Input_end.of_bytes buf) off);
  if end_off > Bytes.length buf then
    raise_eof ~at:off ~expected:(end_off - off)
      ~got:(bytes_from (Input_end.of_bytes buf) off)

let check_decode_bounds wire_size_info min_size runtime input_end buf off =
  check_min_bounds min_size buf off;
  match wire_size_info with
  | Fixed _ -> ()
  | Variable { compute; _ } ->
      (* [compute] reads the length fields the codec's span depends on. Those
         reads bound-check themselves ([expr_reader]), so a buffer too short to
         hold them already surfaces as end-of-input here; anything else they
         raise is a real error and propagates. *)
      check_end_bounds min_size buf off (compute runtime input_end buf off)

let build_plain_decode make fwd wire_size_info min_size =
  match wire_size_info with
  | Fixed _ ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        if off < 0 || off > Input_end.to_int input_end - min_size then
          min_bounds_error min_size buf off;
        apply_fwd make fwd no_runtime input_end buf off
  | Variable _ ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        check_decode_bounds wire_size_info min_size no_runtime input_end buf off;
        apply_fwd make fwd no_runtime input_end buf off

let build_checked_decode raw_decode wire_size_info min_size =
 fun runtime input_end buf off ->
  check_decode_bounds wire_size_info min_size runtime input_end buf off;
  raw_decode runtime input_end buf off

(* [Codec.validate] is the safety gate before a batch of zero-copy [get] calls on
   untrusted input, so it must run decode's structural bounds check even for a
   constraint-free codec (whose constraint validator [validate_checks] is a
   no-op), or a [get] on a truncated buffer would read out of bounds. *)
let validate_with_bounds wire_size_info min_size param_slots param_base
    validate_checks =
 fun ?env_slots buf off ->
  let runtime =
    match env_slots with
    | None -> no_runtime
    | Some slots ->
        Types.eval_ctx (fun name ->
            match Hashtbl.find_opt param_slots name with
            | Some idx -> slots.(idx - param_base)
            | None -> 0)
  in
  check_decode_bounds wire_size_info min_size runtime (Input_end.of_bytes buf)
    buf off;
  validate_checks ?env_slots buf off

let build_size_of_value size_funcs =
  let n = Array.length size_funcs in
  fun ctx value ->
    let total = Stdlib.ref 0 in
    for i = 0 to n - 1 do
      total := !total + size_funcs.(i) ctx value
    done;
    !total

(* Whether encode has to re-read the record it just wrote. A [Wire.where] cond
   or a field [~constraint_] reads sibling fields, so neither can be judged from
   one field's value in isolation -- only against the assembled bytes.
   [struct_field] folds a typ-level [where] into the field's [constraint_], so
   both arrive here as one predicate. A cond that is constantly true restricts
   nothing and does not arm the pass. *)
let is_real_cond : bool Types.expr -> bool = function
  | Types.Bool true -> false
  | _ -> true

let has_encode_checks struct_fields where =
  Option.fold ~none:false ~some:is_real_cond where
  || List.exists
       (fun (Types.Field f) ->
         Option.fold ~none:false ~some:is_real_cond f.constraint_)
       struct_fields

(* Bind the codec's formals from the caller's context, and write the mutable
   ones back after the pass. Written as loops taking everything as arguments
   rather than as [List.iteri] over a closure: this runs once per sub-codec per
   element of an enclosing repeat, where the element count is the sender's to
   choose, and a closure capturing the slots and the context is two heap blocks
   a validator has no business turning over. *)
let rec seed_params arr runtime param_base i = function
  | [] -> ()
  | Param.Pack p :: rest ->
      arr.ints.(param_base + i) <- Types.eval_param runtime p.Types.name;
      seed_params arr runtime param_base (i + 1) rest

let rec writeback_params arr runtime param_base i = function
  | [] -> ()
  | Param.Pack p :: rest ->
      if p.Types.mutable_ then
        Types.eval_set_param runtime p.Types.name arr.ints.(param_base + i);
      writeback_params arr runtime param_base (i + 1) rest

(* Replay the decode-side check pass over the record encode just assembled, so a
   cond that reads sibling fields is enforced on both halves. Field actions are
   deliberately excluded: [populate] runs the check-only validators, so encoding
   never fires an [~action]'s side effects. *)
let encode_checker ~scratch ~n_total ~param_base ~param_handles ~populate
    ~compiled_where runtime buf off =
  let arr = scratch () in
  clear_slots arr n_total;
  seed_params arr runtime param_base 0 param_handles;
  populate arr runtime (Input_end.of_bytes buf) buf off;
  match compiled_where with
  | Some f when not (eval_arr_at ~at:off f arr) ->
      raise_constraint ~at:off ~which:Where ()
  | _ -> ()

(* The check pass over one codec's fields at [off], with its parameters bound
   from the caller's context. Taken apart from [validate_runtime] so [seal] can
   build the same pass into the sub-codec entry point a use site calls, and the
   two cannot drift. *)
let validate_runtime_of ~scratch ~n_total ~param_base ~param_handles
    ~validate_arr runtime input_end buf off =
  let arr = scratch () in
  clear_slots arr n_total;
  seed_params arr runtime param_base 0 param_handles;
  validate_arr arr runtime input_end buf off;
  writeback_params arr runtime param_base 0 param_handles

let build_record_encoder name writers size_of_value ~check =
  let n = Array.length writers in
  fun value runtime buf off ->
    let need = size_of_value runtime value in
    if off + need > Bytes.length buf then
      Fmt.invalid_arg "Codec.encode %s: buffer too short (need %d, got %d)" name
        need
        (Bytes.length buf - off);
    let write_off = Stdlib.ref off in
    for i = 0 to n - 1 do
      write_off := writers.(i) value runtime buf off !write_off
    done;
    (match check with
    | None -> ()
    | Some f -> (
        try f runtime buf off
        with Types.Parse_error e ->
          Fmt.invalid_arg
            "Codec.encode %s: the encoded record is rejected by its own \
             constraints (%a)"
            name Types.pp_parse_error e));
    !write_off

let build_decoders make readers wire_size_info min_size ~plain =
  let fwd = to_fwd readers in
  let raw_decode runtime input_end buf off =
    apply_fwd make fwd runtime input_end buf off
  in
  ( build_checked_decode raw_decode wire_size_info min_size,
    if plain then Some (build_plain_decode make fwd wire_size_info min_size)
    else None )

(* [struct_fields] and [param_handles] are the ones [view] derived and checked
   from the same field list. *)
let seal : type r.
    struct_fields:Types.field list ->
    param_handles:Param.packed list ->
    (r, r) record ->
    r compiled =
 fun ~struct_fields ~param_handles (Record r) ->
  let field_access = List.rev r.field_access_rev in
  let wire_size_info =
    match r.next_off with
    | Static n -> Fixed n
    | Dynamic f -> Variable { min_size = r.min_wire_size; compute = f }
  in
  let min_size = r.min_wire_size in
  let writers = Array.of_list (List.rev r.writers_rev) in
  let validators = List.rev r.validators_rev in
  let param_base = r.n_array_slots in
  let n_params = List.length param_handles in
  fill_param_slots r.param_slots param_base param_handles;
  let n_total = param_base + n_params in
  let compiled_where =
    compile_where_clause r.param_slots r.field_readers r.where
  in
  let validate_arr, populate, validate_checks =
    build_validators validators (List.rev r.checkers_rev) compiled_where
      struct_fields r.param_slots n_total
  in
  let scratch = domain_local_slots n_total in
  let encode_check =
    if not (has_encode_checks struct_fields r.where) then None
    else
      Some
        (encode_checker ~scratch ~n_total ~param_base ~param_handles ~populate
           ~compiled_where)
  in
  (* Per-field action runners *)
  let field_actions = List.rev r.field_actions_rev in
  let size_funcs = Array.of_list (List.rev r.size_of_value_rev) in
  let size_of_value = build_size_of_value size_funcs in
  let has_checks = has_validation_checks compiled_where struct_fields in
  let decode, plain_decode =
    build_decoders r.make r.readers wire_size_info min_size
      ~plain:((not has_checks) && n_params = 0)
  in
  {
    size_of_value;
    field_access;
    field_readers = List.rev r.field_readers;
    field_actions;
    decode;
    plain_decode;
    encode =
      (let size_of_value =
         match wire_size_info with
         | Fixed n -> fun _ _ -> n
         | Variable _ -> size_of_value
       in
       build_record_encoder r.name writers size_of_value ~check:encode_check);
    wire_size = wire_size_info;
    validate =
      validate_with_bounds wire_size_info min_size r.param_slots param_base
        validate_checks;
    validate_ctx =
      (fun runtime input_end buf off ->
        check_decode_bounds wire_size_info min_size runtime input_end buf off;
        validate_runtime_of ~scratch ~n_total ~param_base ~param_handles
          ~validate_arr runtime input_end buf off);
    validate_arr;
    populate;
    n_array_slots = n_total;
    has_checks;
    decode_scratch = scratch;
    param_base;
  }
