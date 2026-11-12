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
let field_conds fld =
  (match fld.constraint_ with Some c -> [ c ] | None -> [])
  @ typ_where_conds fld.typ

let compile_field_check cc fld =
  match field_conds fld with
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

(* The interval a conjunction of comparisons between the field [name] and
   literals allows, or [None] for any other constraint. *)
(* Sets of ints as sorted, disjoint intervals. *)
let set_inter a b =
  let rec go a b =
    match (a, b) with
    | [], _ | _, [] -> []
    | (l1, h1) :: r1, (l2, h2) :: r2 ->
        let rest = if h1 < h2 then go r1 b else go a r2 in
        let lo = max l1 l2 and hi = min h1 h2 in
        if lo <= hi then (lo, hi) :: rest else rest
  in
  go a b

let set_union a b =
  let rec merge = function
    | (l1, h1) :: (l2, h2) :: rest when h1 = max_int || l2 <= h1 + 1 ->
        merge ((l1, max h1 h2) :: rest)
    | x :: rest -> x :: merge rest
    | [] -> []
  in
  merge (List.sort compare (a @ b))

let all_ints = [ (min_int, max_int) ]

(* The values [cmp] keeps when comparing with [n]. *)
let cmp_set cmp n =
  match cmp with
  | Eq -> [ (n, n) ]
  | Ge -> [ (n, max_int) ]
  | Le -> [ (min_int, n) ]
  | Gt -> if n = max_int then [] else [ (n + 1, max_int) ]
  | Lt -> if n = min_int then [] else [ (min_int, n - 1) ]
  | Ne ->
      set_union
        (if n = min_int then [] else [ (min_int, n - 1) ])
        (if n = max_int then [] else [ (n + 1, max_int) ])

(* The values of the field [name] that [e] accepts, when [e] only compares the
   field with literals, through [&&] and [||]. *)
let rec self_set name (e : bool expr) =
  let bound : type a. cmp -> a expr -> a expr -> (int * int) list option =
   fun op x y ->
    match (x, y) with
    | Ref (I, a), Int n when a = name -> Some (cmp_set op n)
    | Int n, Ref (I, a) when a = name -> Some (cmp_set (flip op) n)
    | _ -> None
  in
  let both f a b =
    match (self_set name a, self_set name b) with
    | Some a, Some b -> Some (f a b)
    | _ -> None
  in
  match e with
  | Bool true -> Some all_ints
  | Bool false -> Some []
  | And (a, b) -> both set_inter a b
  | Or (a, b) -> both set_union a b
  | Eq (x, y) -> bound Eq x y
  | Ne (x, y) -> bound Ne x y
  | Lt (x, y) -> bound Lt x y
  | Le (x, y) -> bound Le x y
  | Gt (x, y) -> bound Gt x y
  | Ge (x, y) -> bound Ge x y
  | _ -> None

type base_view = Base : 'b typ * (int * int) list list -> base_view

(* The scalar on the wire under [where], [map] and [enum] wrappers, with the
   sets of values a closed enum and a lookup [map] accept. Constraints and these
   sets bound the wire value, as in the 3D projection, so the check pass reads
   the scalar and never calls a [map]'s decode. *)
let rec typ_view : type a. a typ -> base_view option = function
  | Uint8 -> Some (Base (Uint8, []))
  | Uint16 e -> Some (Base (Uint16 e, []))
  | Uint32 e -> Some (Base (Uint32 e, []))
  | Int8 -> Some (Base (Int8, []))
  | Int16 e -> Some (Base (Int16 e, []))
  | Int32 e -> Some (Base (Int32 e, []))
  | Bits _ as t -> Some (Base (t, []))
  | Where { inner; _ } -> typ_view inner
  | Map { inner; index_bound; _ } ->
      Option.map
        (fun (Base (b, sets)) ->
          match index_bound with
          | None -> Base (b, sets)
          | Some n -> Base (b, [ (0, n - 1) ] :: sets))
        (typ_view inner)
  | Enum { base; cases; closed; _ } ->
      Option.map
        (fun (Base (b, sets)) ->
          if not closed then Base (b, sets)
          else
            let members =
              List.fold_left
                (fun acc (_, v) -> set_union acc [ (v, v) ])
                [] cases
            in
            Base (b, members :: sets))
        (typ_view base)
  | _ -> None

(* The scalar the check pass reads for a field, and the sets its type adds. *)
let check_view ~min_wire_size access typ ~act =
  if Option.is_some act then None
  else
    match typ_view typ with
    | Some (Base (b, sets)) -> (
        match field_scalar ~min_wire_size access b with
        | Some s -> Some (Ps s, sets)
        | None -> None)
    | None -> None

(* The check-pass steps of a field, last first. *)
let field_steps ~used ~name ~slot ~voff ~conds ~span view check full =
  match view with
  | None when span && Option.is_none check -> [ Covered full ]
  | Some (Ps scalar, typ_sets) when slot_scalar scalar -> (
      let cset =
        match conds with
        | [] -> Some all_ints
        | c :: cs -> self_set name (List.fold_left (fun a c -> And (a, c)) c cs)
      in
      match (check, cset, typ_sets) with
      | None, _, [] -> [ Slot { slot; scalar } ]
      | _, Some cset, _ ->
          (* Only the field's own constraint has named its slot so far; a later
             field or the [where] clause that reads it marks it again. *)
          Hashtbl.remove used slot;
          let set = Array.of_list (List.fold_left set_inter cset typ_sets) in
          let fail runtime input_end buf off =
            full (alloc_slots (slot + 1)) runtime input_end buf off
          in
          [ Range { name; slot; scalar; voff; set; fail; store = true } ]
      | Some cond, None, [] ->
          Hashtbl.replace used slot ();
          [ Check { name; slot; voff; cond }; Slot { slot; scalar } ]
      | _ -> [ Full full ])
  | _ -> [ Full full ]

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

let plain_span : type a. a typ -> bool = function
  | Byte_array _ | Byte_slice _ | All_bytes -> true
  | _ -> false

let field_layout : type a r.
    layout list option ->
    (a, r) compiled_field ->
    (a, r) field ->
    layout list option =
 fun layout cf fld ->
  match (layout, cf.field_access, fld.typ) with
  | None, _, _ -> None
  | Some l, Bitfield _, _ -> Some (Lfix cf.size_delta :: l)
  | Some l, (Fixed _ | Dynamic _), typ
    when Types.field_wire_size typ = Some cf.size_delta ->
      Some (Lfix cf.size_delta :: l)
  | ( Some l,
      (Variable _ | Variable_dynamic _),
      (Byte_array { size } | Byte_slice { size }) ) ->
      Some (Lspan size :: l)
  | Some l, (Variable _ | Variable_dynamic _), All_bytes -> Some (Lrest :: l)
  | Some _, _, _ -> None

(* The fields the layout pass reads a span's size from: those whose value an
   int holds exactly, as for the check pass's slots. *)
let field_prefix prefix ~min_wire_size (cf : (_, _) compiled_field)
    (fld : (_, _) field) =
  match check_view ~min_wire_size cf.field_access fld.typ ~act:None with
  | Some ((Ps scalar as s), _) when slot_scalar scalar ->
      (fld.name, s) :: prefix
  | Some _ | None -> prefix

(* A field's reader for a decode that has run the check pass, and for one
   that reads before it checks. *)
let field_reads ~min_wire_size ~validates cf (fld : (_, _) field) act raw_reader
    =
  ( field_read ~min_wire_size cf.field_access fld.typ raw_reader
      ~plain:(Option.is_none act),
    field_read ~min_wire_size cf.field_access fld.typ raw_reader
      ~plain:(Option.is_none act && not validates) )

(* The encoder's write of a field, while every field so far has one. *)
let field_put : type a r.
    r put list option -> (a, r) field -> a field_read -> r put list option =
 fun puts fld checked_read ->
  match (puts, checked_read) with
  | Some puts, Data scalar when int_scalar scalar ->
      Some (Put { get = fld.get; scalar } :: puts)
  | Some puts, Span (at, len) -> Some (Blit { get = fld.get; at; len } :: puts)
  | _ -> None

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
  let field_idx = r.n_fields in
  let dummy_reader _runtime _input_end _buf _base = 0 in
  let cc_readers =
    let base = extend_scope fld.name dummy_reader [] r.field_readers in
    List.fold_left
      (fun acc vn -> (Some vn, dummy_reader) :: acc)
      base action_vanames
  in
  let idx = used_idx r.used_slots (build_idx cc_readers) in
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
  let byte_off = cf.validator_off in
  let new_field_readers =
    extend_scope fld.name (expr_reader cf fld.typ) cf.nested_readers
      r.field_readers
  in
  let raw_reader = guarded_dynamic_reader cf fld.typ in
  let validates = field_reader_validates fld.typ in
  let raw_reader, full, check_only =
    wrap_field_errors ~validates ~check ~act fld.name raw_reader full check_only
  in
  note_fixed_prefix ~before:r.next_off ~after:cf.next_off r.fixed_prefix;
  let min_wire_size = add_min_size r.name fld.name r.min_wire_size cf in
  let read, checked_read =
    field_reads ~min_wire_size ~validates cf fld act raw_reader
  in
  Record
    {
      name = r.name;
      make = r.make;
      readers = Snoc (r.readers, read);
      checked_readers = Snoc (r.checked_readers, checked_read);
      writers_rev = (cf.raw_writer :: cf.extra_writers) @ r.writers_rev;
      puts_rev = field_put r.puts_rev fld checked_read;
      layout_rev = field_layout r.layout_rev cf fld;
      prefix = field_prefix r.prefix ~min_wire_size cf fld;
      size_of_value_rev = field_size_of_value fld cf :: r.size_of_value_rev;
      min_wire_size;
      next_off = cf.next_off;
      fixed_prefix = r.fixed_prefix;
      used_slots = r.used_slots;
      validators_rev =
        List.map
          (fun step -> (byte_off, step))
          (field_steps ~used:r.used_slots ~name:fld.name ~slot:field_idx
             ~voff:byte_off ~conds:(field_conds fld)
             ~span:(plain_span fld.typ && Option.is_none act)
             (check_view ~min_wire_size cf.field_access fld.typ ~act)
             check full)
        @ r.validators_rev;
      checkers_rev = (byte_off, check_only) :: r.checkers_rev;
      field_actions_rev =
        (match act with
        | Some act_fn -> (fld.name, act_fn) :: r.field_actions_rev
        | None -> r.field_actions_rev);
      n_fields = List.length new_field_readers;
      n_array_slots = List.length new_field_readers + List.length action_vanames;
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

let compile_where_clause ?(used = Hashtbl.create 0) param_slots field_readers
    where =
  match where with
  | None -> None
  | Some cond ->
      let rev_readers = List.rev field_readers in
      let idx = used_idx used (lookup_reader_idx rev_readers) in
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

(* A failed [Check], off the hot path. *)
let check_failed ~name ~slot ~voff arr base =
  try raise_field_constraint ~field_idx:slot ~at:(at_of base voff) arr
  with Types.Parse_error e ->
    raise (Types.Parse_error { e with field = name :: e.field })

(* A failed [Range], with the value it read. *)
let range_failed ~name ~voff v base =
  try
    raise_constraint ~at:(at_of base voff) ~which:Field ~value:(Int64.of_int v)
      ()
  with Types.Parse_error e ->
    raise (Types.Parse_error { e with field = name :: e.field })

let check_cond ~name ~voff cond arr base =
  try cond arr
  with Types.Parse_error e ->
    raise
      (Types.Parse_error
         { e with at = at_of base voff; field = name :: e.field })

(* Top level, so a test allocates no closure. *)
let rec in_set_from (set : (int * int) array) (v : int) i =
  i < Array.length set
  &&
  let lo, hi = Array.unsafe_get set i in
  (v >= lo && v <= hi) || in_set_from set v (i + 1)

let in_set (set : (int * int) array) v =
  if Array.length set = 1 then
    let lo, hi = Array.unsafe_get set 0 in
    v >= lo && v <= hi
  else in_set_from set v 0

(* A failed set test: the field's general validator raises the field's own
   error. It always does, as the set is what its checks accept; the constraint
   error after it only stands in if they ever disagree. *)
let set_failed ~name ~voff ~fail v runtime input_end buf off =
  fail runtime input_end buf off;
  range_failed ~name ~voff v off

(* [i] ranges over the array's own indices, so its reads need no check. *)
let run_steps steps arr runtime input_end buf off =
  for i = 0 to Array.length steps - 1 do
    match Array.unsafe_get steps i with
    | Slot { slot; scalar } -> arr.ints.(slot) <- read_slot scalar buf off
    | Check { name; slot; voff; cond } ->
        if not (check_cond ~name ~voff cond arr off) then
          check_failed ~name ~slot ~voff arr off
    | Range { name; slot; scalar; voff; set; fail; store } ->
        let v = read_slot scalar buf off in
        if store then arr.ints.(slot) <- v;
        if not (in_set set v) then
          set_failed ~name ~voff ~fail v runtime input_end buf off
    | Full f | Covered f -> f arr runtime input_end buf off
  done

(* The steps whose work is needed: a slot nothing reads is not filled. *)
let used_steps used steps =
  let read slot =
    match used with None -> true | Some u -> Hashtbl.mem u slot
  in
  List.filter_map
    (function
      | Slot { slot; _ } as s -> if read slot then Some s else None
      | Range r -> Some (Range { r with store = read r.slot })
      | (Check _ | Full _ | Covered _) as s -> Some s)
    steps

(* Every slot is written before it is read when each step fills its own slot
   and nothing else lives in the array, so there is nothing to clear. *)
let scratch_clear steps n_total =
  let own_slot = function
    | Slot _ | Range _ -> true
    | Check _ | Full _ | Covered _ -> false
  in
  let n_own = List.length (List.filter own_slot steps) in
  let general = List.exists (function Full _ -> true | _ -> false) steps in
  if (not general) && n_own = n_total then fun _ -> ()
  else fun arr -> clear_slots arr n_total

(* The check pass: the steps, then the [where] clause. *)
let run_pass steps where arr runtime input_end buf off =
  run_steps steps arr runtime input_end buf off;
  match where with
  | Some f when not (eval_arr_at ~at:off f arr) ->
      raise_constraint ~at:off ~which:Where ()
  | _ -> ()

let build_validators ?used validators_rev checkers_rev compiled_where
    struct_fields param_slots n_total =
  let all_steps = List.map snd validators_rev in
  let steps = Array.of_list (used_steps used all_steps) in
  let clear = scratch_clear all_steps n_total in
  let uses_arr = function
    | Range { store; _ } -> store
    | Slot _ | Check _ | Full _ | Covered _ -> true
  in
  let needs_arr =
    Option.is_some compiled_where || Array.exists uses_arr steps
  in
  let validate_arr arr runtime input_end buf off =
    run_pass steps compiled_where arr runtime input_end buf off
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
        clear arr;
        (* Seed param slots so [where] clauses referencing [Param.input]
           evaluate correctly. *)
        seed_param_slots arr n_total env_slots;
        let runtime = validation_runtime param_slots arr in
        validate_arr arr runtime (Input_end.of_bytes buf) buf off
  in
  (validate_arr, populate, validate, clear, (steps, needs_arr))

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

let no_slots = { ints = [||]; int64s = [||] }

(* A set test as ints: the read's kind, its offset, the bitfield shift and mask
   (0 and -1 for a whole scalar), the number of intervals, then their bounds. *)
let flat_interval : type a. a scalar -> (int * int) array -> int array =
 fun scalar set ->
  let kind, off, shift, mask =
    match scalar with
    | Byte off -> (0, off, 0, -1)
    | U16le off -> (1, off, 0, -1)
    | U16be off -> (2, off, 0, -1)
    | Sbyte off -> (3, off, 0, -1)
    | I16le off -> (4, off, 0, -1)
    | I16be off -> (5, off, 0, -1)
    | U32le off -> (6, off, 0, -1)
    | U32be off -> (7, off, 0, -1)
    | I32le off -> (8, off, 0, -1)
    | I32be off -> (9, off, 0, -1)
    | Bits8 { off; shift; mask } -> (0, off, shift, mask)
    | Bits16le { off; shift; mask } -> (1, off, shift, mask)
    | Bits16be { off; shift; mask } -> (2, off, shift, mask)
  in
  Array.concat
    [
      [| kind; off; shift; mask; Array.length set |];
      Array.concat
        (Array.to_list (Array.map (fun (lo, hi) -> [| lo; hi |]) set));
    ]

(* The index of the first failed test in [d], from [i], or -1. The read is
   dispatched in place and nothing is called, so the loop keeps its state in
   registers. The offsets lie inside the frame the caller has checked. *)
let rec scan_intervals d tables buf off n i =
  if i >= n then -1
  else
    let q = off + Array.unsafe_get d (i + 1) in
    let v =
      match Array.unsafe_get d i with
      | 0 -> Unchecked.u8 buf q
      | 1 -> Unchecked.u16le buf q
      | 2 -> Unchecked.u16be buf q
      | 3 -> SInt8.to_int (SInt8.unsafe_get buf q)
      | 4 -> SInt16.to_int (SInt16.unsafe_le buf q)
      | 5 -> SInt16.to_int (SInt16.unsafe_be buf q)
      | 6 -> UInt32.to_int (UInt32.unsafe_le buf q)
      | 7 -> UInt32.to_int (UInt32.unsafe_be buf q)
      | 8 -> SInt32.to_int (SInt32.unsafe_le buf q)
      | 9 -> SInt32.to_int (SInt32.unsafe_be buf q)
      | k ->
          (* A byte table: kind [10 + n] looks the byte up in table [n]. *)
          Char.code
            (String.unsafe_get
               (Array.unsafe_get tables (k - 10))
               (Unchecked.u8 buf q))
    in
    let v =
      (v lsr Array.unsafe_get d (i + 2)) land Array.unsafe_get d (i + 3)
    in
    let k = Array.unsafe_get d (i + 4) in
    if k = 1 then
      if v >= Array.unsafe_get d (i + 5) && v <= Array.unsafe_get d (i + 6) then
        scan_intervals d tables buf off n (i + 7)
      else i
    else scan_set d tables buf off n i v (i + 5) (i + 5 + (2 * k))

(* The rest of a test with several intervals. Every call in the scan is a tail
   call, so the scan keeps its state in registers. *)
and scan_set d tables buf off n i (v : int) j stop =
  if j >= stop then i
  else if v >= Array.unsafe_get d j && v <= Array.unsafe_get d (j + 1) then
    scan_intervals d tables buf off n stop
  else scan_set d tables buf off n i v (j + 2) stop

(* The first test, in declaration order, the value fails. *)
let first_failed intervals input_end buf off =
  let rec go i =
    if i < Array.length intervals then
      let (Interval { scalar; set; fail; name; voff }) = intervals.(i) in
      let v = read_slot scalar buf off in
      if not (in_set set v) then
        set_failed ~name ~voff ~fail v no_runtime input_end buf off
      else go (i + 1)
  in
  go 0

let run_plain_pass pass input_end buf off =
  match pass with
  | No_checks -> ()
  | Intervals { flat; tables; intervals } ->
      if scan_intervals flat tables buf off (Array.length flat) 0 >= 0 then
        first_failed intervals input_end buf off
  | Steps { steps; where; scratch; clear } ->
      let arr =
        match scratch with
        | None -> no_slots
        | Some scratch ->
            let arr = scratch () in
            clear arr;
            arr
      in
      run_pass steps where arr no_runtime input_end buf off

(* The byte an interval reads, and the field value a byte [b] gives, for the
   intervals that read a single byte. *)
let byte_view : type a. a scalar -> (int * (int -> int)) option = function
  | Byte off -> Some (off, Fun.id)
  | Sbyte off ->
      Some (off, fun b -> (b lsl (Sys.int_size - 8)) asr (Sys.int_size - 8))
  | Bits8 { off; shift; mask } -> Some (off, fun b -> (b lsr shift) land mask)
  | _ -> None

(* Several intervals on one byte become one lookup in a table of the 256 byte
   values, whatever the fields they test: [version = 4 && ihl >= 5] is one load
   and one lookup. *)
let flatten_intervals intervals =
  let groups = Hashtbl.create 8 in
  Array.iter
    (fun (Interval { scalar; set; _ }) ->
      match byte_view scalar with
      | Some (off, value) ->
          let prev = Option.value ~default:[] (Hashtbl.find_opt groups off) in
          Hashtbl.replace groups off ((value, set) :: prev)
      | None -> ())
    intervals;
  let tables = Stdlib.ref [] and n_tables = Stdlib.ref 0 in
  let emitted = Hashtbl.create 8 in
  let table off tests =
    let accept b =
      List.for_all (fun (value, set) -> in_set set (value b)) tests
    in
    let t = String.init 256 (fun b -> if accept b then '\001' else '\000') in
    tables := t :: !tables;
    let i = !n_tables in
    incr n_tables;
    [| 10 + i; off; 0; -1; 1; 1; 1 |]
  in
  let flat =
    Array.to_list intervals
    |> List.filter_map (fun (Interval { scalar; set; _ }) ->
        match byte_view scalar with
        | Some (off, _) when List.length (Hashtbl.find groups off) > 1 ->
            if Hashtbl.mem emitted off then None
            else begin
              Hashtbl.replace emitted off ();
              Some (table off (Hashtbl.find groups off))
            end
        | _ -> Some (flat_interval scalar set))
  in
  (Array.concat flat, Array.of_list (List.rev !tables))

type layout_item =
  | Fix of int
  | Span of (bytes -> int -> unit -> unit -> int)
  | Rest

let rec layout_ok items buf off len cur =
  match items with
  | [] -> cur <= len
  | Fix k :: rest -> layout_ok rest buf off len (cur + k)
  | Span size :: rest ->
      let sz = size buf off () () in
      sz >= 0 && cur <= len - sz && layout_ok rest buf off len (cur + sz)
  | Rest :: rest -> cur <= len && layout_ok rest buf off len len

(* A span's size: a field, or a field times, plus or minus a literal, is one
   closure that reads the field; any other expression goes through
   [Expr_compiler]. The arithmetic is checked, as [decode]'s is: a product
   that wraps on a 31-bit int is a size [decode] refuses. *)
let span_size prefix leaves (e : int expr) =
  let field name =
    match List.assoc_opt name prefix with
    | Some (Ps s) -> Some (fun buf off -> read_slot s buf off)
    | None -> None
  in
  let general () = Expr_compiler.compile_int leaves e in
  match e with
  | Ref (I, a) -> (
      match field a with
      | Some r -> fun buf off () () -> r buf off
      | None -> general ())
  | Mul (Ref (I, a), Int k) | Mul (Int k, Ref (I, a)) -> (
      match field a with
      | Some r when k >= 0 && k <= 0xffff ->
          fun buf off () () -> checked_mul (r buf off) k
      | _ -> general ())
  | Add (Ref (I, a), Int k) | Add (Int k, Ref (I, a)) -> (
      match field a with
      | Some r when abs k <= 0xffff ->
          fun buf off () () -> checked_add (r buf off) k
      | _ -> general ())
  | Sub (Ref (I, a), Int k) -> (
      match field a with
      | Some r when abs k <= 0xffff ->
          fun buf off () () -> checked_sub (r buf off) k
      | _ -> general ())
  | _ -> general ()

(* The layout pass of a variable-size record without parameters whose
   variable fields are plain byte spans sized by earlier fixed-offset fields:
   one walk that reads each length field once and checks each span against the
   buffer. It only ever accepts; anything it does not accept goes to the
   general bounds check, which raises the error it always raised. *)
let build_layout ~min_size prefix layout =
  let unplanned _ = raise Exit in
  let leaves : (bytes, int, unit, unit) Expr_compiler.leaves =
    {
      ref_ =
        (fun name ->
          match List.assoc_opt name prefix with
          | Some (Ps s) -> fun buf off () () -> read_slot s buf off
          | None -> raise Exit);
      i64 = unplanned;
      param_ref = unplanned;
      sizeof_typ =
        (fun (Pack_typ t) ->
          let n = Option.value ~default:0 (field_wire_size t) in
          fun _ _ () () -> n);
      sizeof_this = (fun _ _ () () -> raise Exit);
      field_pos = (fun _ _ () () -> raise Exit);
    }
  in
  match layout with
  | None -> None
  | Some layout -> (
      match
        List.rev_map
          (function
            | Lfix k -> Fix k
            | Lspan e -> Span (span_size prefix leaves e)
            | Lrest -> Rest)
          layout
      with
      | exception Exit -> None
      | items ->
          Some
            (fun buf off ->
              let len = Bytes.length buf in
              off >= 0
              && off <= len - min_size
              &&
                try layout_ok items buf off len off
                with Types.Parse_error _ | Exit -> false))

let step_uses_arr = function
  | Range { store; _ } -> store
  | Slot _ | Check _ | Full _ | Covered _ -> true

(* The steps the plain check pass runs: a layout pass covers the plain spans'
   bounds checks. *)
let plain_steps ~where steps =
  let steps =
    Array.of_list
      (List.filter
         (function Covered _ -> false | _ -> true)
         (Array.to_list steps))
  in
  (steps, Option.is_some where || Array.exists step_uses_arr steps)

let plain_pass ~has_checks ~needs_arr ~scratch ~clear steps where =
  let interval = function
    | Range { scalar; set; fail; name; voff; store = false; _ } ->
        Some (Interval { scalar; set; fail; name; voff })
    | Range _ | Slot _ | Check _ | Full _ | Covered _ -> None
  in
  let intervals = Array.map interval steps in
  if not has_checks then No_checks
  else if Option.is_none where && Array.for_all Option.is_some intervals then
    let intervals = Array.map Option.get intervals in
    let flat, tables = flatten_intervals intervals in
    Intervals { flat; tables; intervals }
  else
    Steps
      {
        steps;
        where;
        scratch = (if needs_arr then Some scratch else None);
        clear;
      }

(* Frame, check, read: the three phases of docs/execution.md, for a codec
   without parameters. *)
(* The frame and check pass of a variable-size record: the layout pass and the
   checks it leaves when it accepts, else the general bounds check and every
   check, the span checks included. *)
let variable_frame ~layout wire_size_info min_size input_end buf off =
  match layout with
  | Some (ok, fast, _) when ok buf off -> run_plain_pass fast input_end buf off
  | Some _ | None -> (
      check_decode_bounds wire_size_info min_size no_runtime input_end buf off;
      match layout with
      | Some (_, _, full) -> run_plain_pass full input_end buf off
      | None -> ())

let build_plain_decode ~layout make fwd wire_size_info min_size pass =
  match (wire_size_info, pass) with
  | Fixed _, No_checks ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        if off < 0 || off > Input_end.to_int input_end - min_size then
          min_bounds_error min_size buf off
        else apply_fwd make fwd no_runtime input_end buf off
  | Fixed _, pass ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        if off < 0 || off > Input_end.to_int input_end - min_size then
          min_bounds_error min_size buf off
        else begin
          run_plain_pass pass input_end buf off;
          apply_fwd make fwd no_runtime input_end buf off
        end
  | Variable _, pass ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        variable_frame ~layout wire_size_info min_size input_end buf off;
        if Option.is_none layout then run_plain_pass pass input_end buf off;
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
let validate_runtime_of ~scratch ~clear ~param_base ~param_handles ~validate_arr
    runtime input_end buf off =
  let arr = scratch () in
  clear arr;
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

let build_decoders ~layout make readers checked_readers wire_size_info min_size
    ~plain ~check =
  let fwd = to_fwd readers and checked = to_fwd checked_readers in
  let raw_decode runtime input_end buf off =
    apply_fwd make checked runtime input_end buf off
  in
  ( build_checked_decode raw_decode wire_size_info min_size,
    if plain then
      Some (build_plain_decode ~layout make fwd wire_size_info min_size check)
    else None )

(* The check pass [decode] runs between the frame and the reads. *)
(* [validate] of a codec without parameters: the frame, then [check]. *)
let plain_validate ~layout wire_size_info min_size pass =
  match (wire_size_info, pass) with
  | Fixed _, No_checks ->
      fun buf off ->
        if off < 0 || off > Bytes.length buf - min_size then
          min_bounds_error min_size buf off
  | Fixed _, pass ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        if off < 0 || off > Input_end.to_int input_end - min_size then
          min_bounds_error min_size buf off
        else run_plain_pass pass input_end buf off
  | Variable _, pass ->
      fun buf off ->
        let input_end = Input_end.of_bytes buf in
        variable_frame ~layout wire_size_info min_size input_end buf off;
        if Option.is_none layout then run_plain_pass pass input_end buf off

let encode_too_short name n buf off =
  if off < 0 then Fmt.invalid_arg "Codec.encode %s: negative offset %d" name off;
  Fmt.invalid_arg "Codec.encode %s: buffer too short (need %d, got %d)" name n
    (Bytes.length buf - off)

let encode_rejected name e =
  Fmt.invalid_arg
    "Codec.encode %s: the encoded record is rejected by its own constraints \
     (%a)"
    name Types.pp_parse_error e

(* The words the bitfields are or-ed into, zeroed before the writes. *)
let bitfield_words puts =
  let words =
    List.filter_map
      (function
        | Put { scalar = Bits8 { off; _ }; _ } -> Some (off, false)
        | Put { scalar = Bits16le { off; _ } | Bits16be { off; _ }; _ } ->
            Some (off, true)
        | Put _ | Blit _ -> None)
      puts
  in
  List.sort_uniq compare words

(* Frame, then one write per field. The frame covers every write, so none is
   checked; a codec with constraints and no actions runs [validate]'s pass over
   what it wrote. *)
(* A field the encoder writes from an int: its getter, the proof its value is
   an int, and where and how to store it. [kind] picks the store: a byte, a
   16-bit or 32-bit word in either order, or a bitfield or-ed into a byte or a
   16-bit word. *)
type 'r wput = {
  get : 'r -> int;
  kind : int;
  at : int;
  shift : int;
  mask : int;
}

(* The getter of an int-valued field, retyped as one: the proof makes it the
   same closure. *)
let int_getter : type a r. (a, int) Type.eq -> (r -> a) -> r -> int =
 fun Type.Equal get -> get

let wput_of : type r. r put -> r wput option = function
  | Blit _ -> None
  | Put { get; scalar } -> (
      let w eq kind at ~shift ~mask =
        Some { get = int_getter eq get; kind; at; shift; mask }
      in
      let opt eq f = match eq with Some eq -> f eq | None -> None in
      match scalar with
      | Byte at -> w UInt8.int_eq 0 at ~shift:0 ~mask:0
      | Sbyte at -> w SInt8.int_eq 0 at ~shift:0 ~mask:0
      | U16be at -> w UInt16.int_eq 1 at ~shift:0 ~mask:0
      | I16be at -> w SInt16.int_eq 1 at ~shift:0 ~mask:0
      | U16le at -> w UInt16.int_eq 2 at ~shift:0 ~mask:0
      | I16le at -> w SInt16.int_eq 2 at ~shift:0 ~mask:0
      | U32be at -> opt UInt32.int_eq (fun eq -> w eq 3 at ~shift:0 ~mask:0)
      | I32be at -> opt SInt32.int_eq (fun eq -> w eq 3 at ~shift:0 ~mask:0)
      | U32le at -> opt UInt32.int_eq (fun eq -> w eq 4 at ~shift:0 ~mask:0)
      | I32le at -> opt SInt32.int_eq (fun eq -> w eq 4 at ~shift:0 ~mask:0)
      | Bits8 { off; shift; mask } -> w Type.Equal 5 off ~shift ~mask
      | Bits16be { off; shift; mask } -> w Type.Equal 6 off ~shift ~mask
      | Bits16le { off; shift; mask } -> w Type.Equal 7 off ~shift ~mask)

(* One store, chosen in place. A bitfield value wider than its field sets
   [bad] instead of raising, so the row makes no call but the getters. *)
let[@inline] put_word buf off kind at shift mask x bad =
  let p = off + at in
  if kind = 0 then Unchecked.set_u8 buf p x
  else if kind = 1 then Unchecked.set_u16be buf p x
  else if kind = 2 then Unchecked.set_u16le buf p x
  else if kind = 3 then Unchecked.set_i32be buf p (Int32.of_int x)
  else if kind = 4 then Unchecked.set_i32le buf p (Int32.of_int x)
  else begin
    bad := !bad lor (x land lnot mask);
    if kind = 5 then
      Unchecked.set_u8 buf p (Unchecked.u8 buf p lor (x lsl shift))
    else if kind = 6 then
      Unchecked.set_u16be buf p (Unchecked.u16be buf p lor (x lsl shift))
    else Unchecked.set_u16le buf p (Unchecked.u16le buf p lor (x lsl shift))
  end

(* The writes of a fixed record of int fields, staged four to a row. A row
   calls its getters, stores their values in place and hands the bits that did
   not fit their bitfield to the row after it. After the last, [finish] runs
   the check pass on what was written, or [slow], the general encoder, reports
   the bitfield value too wide for its field. *)
let rec stage_rows : type r.
    r wput list ->
    slow:(r -> bytes -> int -> unit) ->
    finish:(bytes -> int -> unit) ->
    r ->
    bytes ->
    int ->
    int ->
    unit =
 fun puts ~slow ~finish ->
  let[@inline] last v buf off bad =
    if bad <> 0 then slow v buf off else finish buf off
  in
  match puts with
  | [] -> fun v buf off bad -> last v buf off bad
  | [ p1 ] ->
      let { get = g1; kind = k1; at = a1; shift = s1; mask = m1 } = p1 in
      fun v buf off bad ->
        let bad = Stdlib.ref bad in
        put_word buf off k1 a1 s1 m1 (g1 v) bad;
        last v buf off !bad
  | [ p1; p2 ] ->
      let { get = g1; kind = k1; at = a1; shift = s1; mask = m1 } = p1 in
      let { get = g2; kind = k2; at = a2; shift = s2; mask = m2 } = p2 in
      fun v buf off bad ->
        let bad = Stdlib.ref bad in
        put_word buf off k1 a1 s1 m1 (g1 v) bad;
        put_word buf off k2 a2 s2 m2 (g2 v) bad;
        last v buf off !bad
  | [ p1; p2; p3 ] ->
      let { get = g1; kind = k1; at = a1; shift = s1; mask = m1 } = p1 in
      let { get = g2; kind = k2; at = a2; shift = s2; mask = m2 } = p2 in
      let { get = g3; kind = k3; at = a3; shift = s3; mask = m3 } = p3 in
      fun v buf off bad ->
        let bad = Stdlib.ref bad in
        put_word buf off k1 a1 s1 m1 (g1 v) bad;
        put_word buf off k2 a2 s2 m2 (g2 v) bad;
        put_word buf off k3 a3 s3 m3 (g3 v) bad;
        last v buf off !bad
  | p1 :: p2 :: p3 :: p4 :: rest ->
      let { get = g1; kind = k1; at = a1; shift = s1; mask = m1 } = p1 in
      let { get = g2; kind = k2; at = a2; shift = s2; mask = m2 } = p2 in
      let { get = g3; kind = k3; at = a3; shift = s3; mask = m3 } = p3 in
      let { get = g4; kind = k4; at = a4; shift = s4; mask = m4 } = p4 in
      let rest = stage_rows rest ~slow ~finish in
      fun v buf off bad ->
        let bad = Stdlib.ref bad in
        put_word buf off k1 a1 s1 m1 (g1 v) bad;
        put_word buf off k2 a2 s2 m2 (g2 v) bad;
        put_word buf off k3 a3 s3 m3 (g3 v) bad;
        put_word buf off k4 a4 s4 m4 (g4 v) bad;
        rest v buf off !bad

let zero_words words buf off =
  for i = 0 to Array.length words - 1 do
    let w, wide = Array.unsafe_get words i in
    if wide then Unchecked.set_u16le buf (off + w) 0
    else Unchecked.set_u8 buf (off + w) 0
  done

(* The byte arrays of a record, each getter called once and its string copied
   in when it has the field's length; [false] at the first that does not, for
   the general encoder to write the whole record again. *)
let blit_all blits v buf off =
  let rec go i =
    i >= Array.length blits
    ||
    match Array.unsafe_get blits i with
    | Blit { get; at; len } ->
        let s = get v in
        String.length s = len
        && begin
          Unchecked.blit_string s buf (off + at);
          go (i + 1)
        end
    | Put _ -> go (i + 1)
  in
  go 0

let build_plain_encode name n puts check =
  let words = Array.of_list (bitfield_words puts) in
  let blits, ints =
    List.partition (function Blit _ -> true | Put _ -> false) puts
  in
  let wputs = List.filter_map wput_of ints in
  let all_ints = List.length wputs = List.length ints in
  let blits = Array.of_list blits in
  let puts = Array.of_list puts in
  let finish buf off =
    match check with
    | No_checks -> ()
    | pass -> (
        try run_plain_pass pass (Input_end.of_bytes buf) buf off
        with Types.Parse_error e -> encode_rejected name e)
  in
  let slow v buf off =
    if off < 0 || off > Bytes.length buf - n then
      encode_too_short name n buf off
    else begin
      zero_words words buf off;
      for i = 0 to Array.length puts - 1 do
        match Array.unsafe_get puts i with
        | Put { get; scalar } -> write_scalar scalar buf off (get v)
        | Blit { get; at; len } ->
            ignore (blit_string_exact len buf (off + at) (get v))
      done;
      finish buf off
    end
  in
  let staged =
    if all_ints && wputs <> [] then Some (stage_rows wputs ~slow ~finish)
    else None
  in
  match staged with
  | Some write when Array.length blits = 0 ->
      fun v buf off ->
        if off < 0 || off > Bytes.length buf - n then
          encode_too_short name n buf off
        else begin
          zero_words words buf off;
          write v buf off 0
        end
  | Some write ->
      fun v buf off ->
        if off < 0 || off > Bytes.length buf - n then
          encode_too_short name n buf off
        else if blit_all blits v buf off then begin
          zero_words words buf off;
          write v buf off 0
        end
        else slow v buf off
  | None -> slow

let plain_encoder ~name ~n_params ~wire_size_info ~struct_fields ~encode_check
    puts check =
  let no_actions =
    List.for_all (fun (Types.Field f) -> Option.is_none f.action) struct_fields
  in
  match (wire_size_info, puts) with
  | Fixed n, Some puts
    when n_params = 0 && (Option.is_none encode_check || no_actions) ->
      Some (build_plain_encode name n (List.rev puts) check)
  | _ -> None

(* A fixed-size record's size is known without walking its fields. *)
let record_encoder name writers wire_size_info size_of_value check =
  let size_of_value =
    match wire_size_info with
    | Fixed n -> fun _ _ -> n
    | Variable _ -> size_of_value
  in
  build_record_encoder name writers size_of_value ~check

let validate_ctx wire_size_info min_size ~scratch ~clear ~param_base
    ~param_handles validate_arr runtime input_end buf off =
  check_decode_bounds wire_size_info min_size runtime input_end buf off;
  validate_runtime_of ~scratch ~clear ~param_base ~param_handles ~validate_arr
    runtime input_end buf off

(* The check pass of a record without parameters and, for a variable-size
   one, its layout pass with the check pass it leaves; the full check pass is
   what the layout pass falls back on. *)
let plain_passes ~wire_size_info ~n_params ~min_size prefix layout ~where ~steps
    ~needs_arr pass =
  let check = pass steps needs_arr in
  let layout =
    match wire_size_info with
    | Variable _ when n_params = 0 ->
        Option.map
          (fun ok ->
            let steps, needs_arr = plain_steps ~where steps in
            (ok, pass steps needs_arr, check))
          (build_layout ~min_size prefix layout)
    | _ -> None
  in
  (check, layout)

(* The names of the fast paths a record's shape selects, for the coverage
   test. Each reads the value [seal] dispatches on, so it cannot name a path the
   codec does not run. *)
let pass_paths prefix = function
  | No_checks -> [ prefix ^ ".none" ]
  | Steps _ -> [ prefix ^ ".steps" ]
  | Intervals { flat = [||]; _ } -> [ prefix ^ ".none" ]
  | Intervals { flat; tables; _ } ->
      let rec plain i =
        i < Array.length flat
        && (flat.(i) < 10 || plain (i + 5 + (2 * flat.(i + 4))))
      in
      (if Array.length tables > 0 then [ prefix ^ ".byte_table" ] else [])
      @ if plain 0 then [ prefix ^ ".interval" ] else []

let encode_paths puts =
  let ints = List.filter (function Put _ -> true | Blit _ -> false) puts in
  let wputs = List.filter_map wput_of ints in
  let staged = wputs <> [] && List.length wputs = List.length ints in
  (if staged then
     let n = List.length wputs in
     Fmt.str "encode.staged.%d" (((n - 1) mod 4) + 1)
     :: (if n > 4 then [ "encode.staged.rows" ] else [])
   else [ "encode.plain" ])
  @
  if List.exists (function Blit _ -> true | Put _ -> false) puts then
    [ "encode.blit" ]
  else []

let selected_paths wire_size_info n_params layout check puts =
  if n_params > 0 then []
  else
    (match (wire_size_info, layout) with
      | Fixed _, _ -> "decode.fixed" :: pass_paths "pass" check
      | Variable _, Some (_, fast, _) ->
          ("decode.layout" :: pass_paths "layout" fast)
          @ pass_paths "pass" check
      | Variable _, None -> "decode.variable" :: pass_paths "pass" check)
    @
    match (wire_size_info, puts) with
    | Fixed _, Some puts -> encode_paths (List.rev puts)
    | _ -> []

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
    compile_where_clause ~used:r.used_slots r.param_slots r.field_readers
      r.where
  in
  let validate_arr, populate, validate_checks, clear, (steps, needs_arr) =
    build_validators ~used:r.used_slots validators (List.rev r.checkers_rev)
      compiled_where struct_fields r.param_slots n_total
  in
  let scratch = domain_local_slots n_total in
  let encode_check =
    if not (has_encode_checks struct_fields r.where) then None
    else
      Some
        (encode_checker ~scratch ~n_total ~param_base ~param_handles ~populate
           ~compiled_where)
  in
  let field_actions = List.rev r.field_actions_rev in
  let size_of_value =
    build_size_of_value (Array.of_list (List.rev r.size_of_value_rev))
  in
  let has_checks = has_validation_checks compiled_where struct_fields in
  let check, layout =
    plain_passes ~wire_size_info ~n_params ~min_size r.prefix r.layout_rev
      ~where:compiled_where ~steps ~needs_arr (fun steps needs_arr ->
        plain_pass ~has_checks ~needs_arr ~scratch ~clear steps compiled_where)
  in
  let decode, plain_decode =
    build_decoders ~layout r.make r.readers r.checked_readers wire_size_info
      min_size ~plain:(n_params = 0) ~check
  in
  {
    size_of_value;
    field_access;
    field_readers = List.rev r.field_readers;
    field_actions;
    decode;
    plain_decode;
    encode =
      record_encoder r.name writers wire_size_info size_of_value encode_check;
    wire_size = wire_size_info;
    plain_validate = plain_validate ~layout wire_size_info min_size check;
    plain_encode =
      plain_encoder ~name:r.name ~n_params ~wire_size_info ~struct_fields
        ~encode_check r.puts_rev check;
    fixed_frame = (match wire_size_info with Fixed _ -> min_size | _ -> -1);
    plain_pass = check;
    validate =
      validate_with_bounds wire_size_info min_size r.param_slots param_base
        validate_checks;
    validate_ctx =
      validate_ctx wire_size_info min_size ~scratch ~clear ~param_base
        ~param_handles validate_arr;
    validate_arr;
    clear_scratch = clear;
    populate;
    n_array_slots = n_total;
    has_checks;
    decode_scratch = scratch;
    param_base;
    paths = selected_paths wire_size_info n_params layout check r.puts_rev;
  }
