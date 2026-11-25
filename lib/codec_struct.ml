open Types
open Codec_repr
open Codec_elem
open Codec_compile
open Codec_seal

type t = {
  min_size : int;
  wire_size : wire_size_info;
  validate : bytes -> int -> unit;
  validate_ctx : runtime -> Input_end.t -> bytes -> int -> unit;
}

(* Internal accumulator -- the validator-relevant subset of [record]. *)
type validator_acc = {
  validators_rev :
    (int * (slots -> runtime -> Input_end.t -> bytes -> int -> unit)) list;
  checkers_rev :
    (int * (slots -> runtime -> Input_end.t -> bytes -> int -> unit)) list;
  field_readers : field_reader list;
  typed_readers : typed_reader list;
  field_access_rev : (string * field_access) list;
  n_fields : int;
  n_array_slots : int;
  min_size : int;
  next_off : next_off;
  fixed_prefix : int ref;
  bf : bf_codec_state option;
  param_slots : (string, int) Hashtbl.t;
  slot_typs : (int, Types.packed_typ) Hashtbl.t;
}

let empty_validator_acc () =
  {
    validators_rev = [];
    checkers_rev = [];
    field_readers = [];
    typed_readers = [];
    field_access_rev = [];
    n_fields = 0;
    n_array_slots = 0;
    min_size = 0;
    next_off = Static 0;
    fixed_prefix = Stdlib.ref 0;
    bf = None;
    param_slots = Hashtbl.create 4;
    slot_typs = Hashtbl.create 4;
  }

let layout_ctx_of_validator_acc acc =
  {
    next_off = acc.next_off;
    fixed_prefix = acc.fixed_prefix;
    bf = acc.bf;
    field_readers = acc.field_readers;
    typed_readers = acc.typed_readers;
    field_at =
      (fun name ->
        Option.map access_at (List.assoc_opt name acc.field_access_rev));
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
let acc_off_fn (acc : validator_acc) = next_off_rel acc.next_off

let acc_prev_end (acc : validator_acc) :
    runtime -> Input_end.t -> bytes -> int -> int =
  match acc.next_off with
  | Static n -> fun _runtime _input_end _buf base -> base + n
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
  let dummy_reader _runtime _input_end _buf _base = 0 in
  let cc_readers =
    let base = extend_scope name dummy_reader [] acc.field_readers in
    List.fold_left
      (fun acc' vn -> (Some vn, dummy_reader) :: acc')
      base action_vanames
  in
  let idx = build_idx cc_readers in
  let cc =
    mk_ctx ~sizeof_this:acc.fixed_prefix ~field_pos:acc.n_fields
      ~param_slots:acc.param_slots ~slot_typs:acc.slot_typs idx
  in
  let check = Option.map (compile_bool_arr cc) constraint_ in
  let act = compile_action cc action in
  let check_only, full =
    field_validators ~field_idx:acc.n_fields ~validator_off ~populate ~check
      ~act
  in
  (full, check_only, List.length action_vanames)

let rec apply_struct_field acc inner_struct =
  let inner_v = v inner_struct in
  let static_off = match acc.next_off with Static n -> n | Dynamic _ -> -1 in
  let off_fn = acc_off_fn acc in
  let validator _arr runtime input_end buf base =
    inner_v.validate_ctx runtime input_end buf
      (base + off_fn runtime input_end buf base)
  in
  let prev_end = acc_prev_end acc in
  let size_delta, next_off =
    match (acc.next_off, inner_v.wire_size) with
    | Static n, Fixed sz -> (sz, Static (n + sz))
    | _, Fixed sz ->
        ( sz,
          Dynamic
            (fun runtime input_end buf base ->
              offset_add (prev_end runtime input_end buf base) sz) )
    | _, Variable { min_size; compute } ->
        ( min_size,
          Dynamic
            (fun runtime input_end buf base ->
              compute runtime input_end buf
                (prev_end runtime input_end buf base)) )
  in
  {
    validators_rev = (static_off, validator) :: acc.validators_rev;
    checkers_rev = (static_off, validator) :: acc.checkers_rev;
    field_readers = acc.field_readers;
    typed_readers = acc.typed_readers;
    field_access_rev = acc.field_access_rev;
    n_fields = acc.n_fields;
    n_array_slots = acc.n_array_slots;
    min_size = acc.min_size + size_delta;
    next_off;
    fixed_prefix = acc.fixed_prefix;
    bf = None;
    param_slots = acc.param_slots;
    slot_typs = acc.slot_typs;
  }

and apply_field_to_validator_acc acc (Types.Field f) =
  match f.field_typ with
  | Types.Struct inner_struct -> apply_struct_field acc inner_struct
  | _ ->
      let name = Option.value f.field_name ~default:"" in
      let codec_field : (_, unit) field =
        {
          id = -1;
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
      note_typ acc.slot_typs acc.n_fields f.field_typ;
      let full, check_only, n_extra_vars =
        build_field_checks acc
          ~populate:(guarded_populate cf f.field_typ)
          ~validator_off:cf.validator_off ~name ~action:f.action
          ~constraint_:f.constraint_
      in
      let new_field_readers =
        extend_scope name cf.int_reader cf.nested_readers acc.field_readers
      in
      {
        validators_rev = (cf.validator_off, full) :: acc.validators_rev;
        checkers_rev = (cf.validator_off, check_only) :: acc.checkers_rev;
        field_readers = new_field_readers;
        typed_readers =
          extend_typed name f.field_typ cf.raw_reader acc.typed_readers;
        field_access_rev = (name, cf.field_access) :: acc.field_access_rev;
        n_fields = List.length new_field_readers;
        n_array_slots = List.length new_field_readers + n_extra_vars;
        min_size = acc.min_size + cf.size_delta;
        next_off =
          (note_fixed_prefix ~before:acc.next_off ~after:cf.next_off
             acc.fixed_prefix;
           cf.next_off);
        fixed_prefix = acc.fixed_prefix;
        bf = cf.bf_after;
        param_slots = acc.param_slots;
        slot_typs = acc.slot_typs;
      }

and v (s : Types.struct_) : t =
  let acc =
    List.fold_left apply_field_to_validator_acc (empty_validator_acc ())
      s.fields
  in
  let wire_size_info =
    match acc.next_off with
    | Static n -> Fixed n
    | Dynamic f -> Variable { min_size = acc.min_size; compute = f }
  in
  let param_handles = collect_param_handles s.name s.fields s.where in
  let n_params = List.length param_handles in
  let param_base = acc.n_array_slots in
  fill_param_slots acc.param_slots param_base param_handles;
  let n_total = param_base + n_params in
  let compiled_where =
    compile_where_clause ~slot_typs:acc.slot_typs acc.param_slots
      acc.field_readers s.where
  in
  let validate_arr, _populate, _validate, _clear, _ =
    build_validators
      (List.rev_map (fun (off, f) -> (off, Full f)) acc.validators_rev)
      (List.rev acc.checkers_rev)
      compiled_where s.fields acc.param_slots n_total
  in
  (* Full validation: fire field actions and check the where clause.
     [build_validators]'s third return [validate] is checkers-only (no
     actions). [validate_arr] runs full validators against a per-domain
     scratch, zeroed on entry. *)
  let get_arr = domain_local_slots n_total in
  let validate_ctx runtime input_end buf off =
    let arr = get_arr () in
    if n_total > 0 then clear_slots arr n_total;
    List.iteri
      (fun i (Param.Pack p) ->
        arr.ints.(param_base + i) <- Types.eval_param runtime p.Types.name)
      param_handles;
    validate_arr arr runtime input_end buf off
  in
  let validate buf off =
    validate_ctx no_runtime (Input_end.of_bytes buf) buf off
  in
  {
    min_size = acc.min_size;
    wire_size = wire_size_info;
    validate;
    validate_ctx;
  }

let validate (t : t) buf off = t.validate buf off

let size_of (t : t) buf off =
  match t.wire_size with
  | Fixed n -> n
  | Variable { compute; _ } ->
      compute no_runtime (Input_end.of_bytes buf) buf off - off

let min_size (t : t) = t.min_size

let wire_size_info (t : t) =
  match t.wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } ->
      `Variable
        (fun buf off -> compute no_runtime (Input_end.of_bytes buf) buf off)
