open Types
module Slice = Bytesrw.Bytes.Slice
open Codec_leaf
open Codec_repr
open Codec_elem
open Codec_compile
open Codec_seal

(* The codec is built in phases, one module each, in dependency order:
   [Codec_leaf] (one field type at a fixed offset), [Codec_repr] (scratch,
   field access, builder and sealed types), [Codec_elem] (bitfield words and
   repeat elements), [Codec_compile] (per-field plans) and [Codec_seal] (frame,
   checks, decode and encode). This module holds the struct validator and the
   operations of the interface. *)

type ('a, 'r) field = ('a, 'r) Codec_repr.field
type 'r t = 'r Codec_repr.t

let ( $ ) = ( $ )
let zeroterm_nul_pos = zeroterm_nul_pos
let blit_string_exact = blit_string_exact
let blit_slice_exact = blit_slice_exact
let pp = pp

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
  validate_ctx : runtime -> Input_end.t -> bytes -> int -> unit;
}

(* Internal accumulator -- the validator-relevant subset of [record]. *)
type validator_acc = {
  validators_rev :
    (int * (slots -> runtime -> Input_end.t -> bytes -> int -> unit)) list;
  checkers_rev :
    (int * (slots -> runtime -> Input_end.t -> bytes -> int -> unit)) list;
  field_readers : field_reader list;
  field_access_rev : (string * field_access) list;
  n_fields : int;
  n_array_slots : int;
  min_size : int;
  next_off : next_off;
  fixed_prefix : int ref;
  bf : bf_codec_state option;
  param_slots : (string, int) Hashtbl.t;
}

let empty_validator_acc () =
  {
    validators_rev = [];
    checkers_rev = [];
    field_readers = [];
    field_access_rev = [];
    n_fields = 0;
    n_array_slots = 0;
    min_size = 0;
    next_off = Static 0;
    fixed_prefix = Stdlib.ref 0;
    bf = None;
    param_slots = Hashtbl.create 4;
  }

let layout_ctx_of_validator_acc acc =
  {
    next_off = acc.next_off;
    fixed_prefix = acc.fixed_prefix;
    bf = acc.bf;
    field_readers = acc.field_readers;
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
      ~param_slots:acc.param_slots idx
  in
  let check = Option.map (compile_bool_arr cc) constraint_ in
  let act = compile_action cc action in
  let check_only, full =
    field_validators ~field_idx:acc.n_fields ~validator_off ~populate ~check
      ~act
  in
  (full, check_only, List.length action_vanames)

let rec apply_struct_field acc inner_struct =
  let inner_v = validator_of_struct inner_struct in
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
              prev_end runtime input_end buf base + sz) )
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
    field_access_rev = acc.field_access_rev;
    n_fields = acc.n_fields;
    n_array_slots = acc.n_array_slots;
    min_size = acc.min_size + size_delta;
    next_off;
    fixed_prefix = acc.fixed_prefix;
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
  let param_handles = collect_param_handles s.name s.fields s.where in
  let n_params = List.length param_handles in
  let param_base = acc.n_array_slots in
  fill_param_slots acc.param_slots param_base param_handles;
  let n_total = param_base + n_params in
  let compiled_where =
    compile_where_clause acc.param_slots acc.field_readers s.where
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

let validate_struct (v : validator) buf off = v.validate buf off

let struct_size_of (v : validator) buf off =
  match v.wire_size with
  | Fixed n -> n
  | Variable { compute; _ } ->
      compute no_runtime (Input_end.of_bytes buf) buf off - off

let struct_min_size (v : validator) = v.min_size

let wire_size_info_of_validator (v : validator) =
  match v.wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } ->
      `Variable
        (fun buf off -> compute no_runtime (Input_end.of_bytes buf) buf off)

(* Heterogeneous field list. [] seals the view; (::) adds a field.
   Tracks the constructor type: ('a -> 'b -> 'r, 'r) matches a
   constructor (fun a b -> ...). *)
type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

(* The field refs of an action that [compile_stmt] resolves. *)
let rec iter_stmt_field_refs f = function
  | Types.Assign (_, e) | Types.Var (_, e) -> iter_refs ~param:ignore ~field:f e
  | Types.Return e -> iter_refs ~param:ignore ~field:f e
  | Types.If (c, then_, else_) ->
      iter_refs ~param:ignore ~field:f c;
      List.iter (iter_stmt_field_refs f) then_;
      Option.iter (List.iter (iter_stmt_field_refs f)) else_
  | Types.Field_assign _ | Types.Extern_call _ | Types.Abort -> ()

(* The names a field adds to the scope of later fields and of the [where]
   clause, as [apply_compiled] adds them: an embedded sub-codec brings its own
   fields along. *)
let rec nested_names : type a. a typ -> string list = function
  | Codec { codec_field_readers; _ } -> List.map fst codec_field_readers
  | Map { inner; _ } -> nested_names inner
  | Enum { base; _ } -> nested_names base
  | Where { inner; _ } -> nested_names inner
  | _ -> []

(* A constraint or an action may only name the fields before it, and the
   [where] clause may name them all. Checking that here, with the scope and the
   error the compile uses, makes a bad name fail where the codec is built.
   Returns the scope of the [where] clause. *)
let check_field_refs : type f r. (f, r) fields -> string list =
 fun flds ->
  let rec go : type g. string list -> (g, r) fields -> string list =
   fun scope -> function
     | [] -> scope
     | fld :: rest ->
         let stmts : Types.action_stmt list =
           match fld.action with
           | None -> []
           | Some (Types.Success stmts | Types.Act stmts) -> stmts
         in
         let conds = Option.to_list fld.constraint_ @ typ_where_conds fld.typ in
         if not (List.is_empty conds && List.is_empty stmts) then (
           let visible = List.fold_left action_vars (fld.name :: scope) stmts in
           let check name =
             if not (List.mem name visible) then unbound_check_ref name
           in
           List.iter (iter_refs ~param:ignore ~field:check) conds;
           List.iter (iter_stmt_field_refs check) stmts);
         go (nested_names fld.typ @ (fld.name :: scope)) rest
  in
  go [] flds

let check_where_refs scope where =
  let check name = if not (List.mem name scope) then unbound_where_ref name in
  Option.iter (iter_refs ~param:ignore ~field:check) where

let view : type f r.
    string -> ?where:bool expr -> ?doc:string -> f -> (f, r) fields -> r t =
 fun name ?where ?doc constructor flds ->
  let rec struct_fields : type g. (g, r) fields -> Types.field list = function
    | [] -> []
    | f :: rest -> struct_field f :: struct_fields rest
  in
  (* Check the description now, so a malformed codec fails where it is built.
     The compile waits for the first operation that needs it. *)
  let scope = check_field_refs flds in
  let struct_fields = struct_fields flds in
  Shape.reject_invalid_codec_shape name struct_fields;
  let param_handles = collect_param_handles name struct_fields where in
  check_where_refs scope where;
  let compile name =
    let rec add : type g. (g, r) record -> (g, r) fields -> r compiled =
     fun r flds ->
      match flds with
      | [] -> seal ~struct_fields ~param_handles r
      | f :: rest -> add (add_field r f) rest
    in
    add (record_start ?where ?doc name constructor) flds
  in
  {
    id = Atomic.fetch_and_add id_counter 1;
    name;
    struct_fields;
    n_params = List.length param_handles;
    param_handles;
    where;
    doc;
    compiled = Atomic.make None;
    compile;
  }

let v = view

(* Two domains may both compile a codec neither has used; the first to publish
   wins and the other's result is dropped. Compiling is pure, so the two are
   interchangeable. *)
let rec compile (t : 'r t) : 'r compiled =
  let c = t.compile t.name in
  if Atomic.compare_and_set t.compiled None (Some c) then c
  else match Atomic.get t.compiled with Some c -> c | None -> compile t

let[@inline] compiled (t : 'r t) : 'r compiled =
  match Atomic.get t.compiled with Some c -> c | None -> compile t

let wire_size_opt (t : _ t) =
  match (compiled t).wire_size with Fixed n -> Some n | Variable _ -> None

let wire_size (t : _ t) =
  match (compiled t).wire_size with
  | Fixed n -> n
  | Variable _ ->
      invalid_arg
        "Codec.wire_size: variable-size codec (use wire_size_opt or \
         wire_size_at instead)"

let min_wire_size (t : _ t) =
  match (compiled t).wire_size with
  | Fixed n -> n
  | Variable { min_size; _ } -> min_size

let size_of_value_ctx (t : _ t) = (compiled t).size_of_value

let raw_decode (t : _ t) buf off =
  (compiled t).decode no_runtime (Input_end.of_bytes buf) buf off

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

let check_env_owner ~op t (env : Param.env) =
  if env.codec_id <> t.id then
    Fmt.invalid_arg "Codec.%s: env was not created by Codec.env for %S" op
      t.name

let require_env ~op t = function
  | None when t.n_params = 0 || not (has_input_params t) -> ()
  | None ->
      Fmt.invalid_arg
        "Codec.%s: codec %s has input params; pass ?env (e.g. [Codec.env c |> \
         Param.bind p N])."
        op t.name
  | Some env -> (
      check_env_owner ~op t env;
      match unbound_params t env with
      | [] -> ()
      | missing ->
          Fmt.invalid_arg
            "Codec.%s: codec %s has unbound input params [%s]; bind every one \
             before use."
            op t.name
            (String.concat ", " missing))

(* The record's extent as the buffer describes it. A parametric span resolves
   through [env]: without one every [Param.expr] reads 0, so a param-sized field
   measures as empty and the answer comes back quietly short. That is not a
   smaller record, it is a different verdict about the same bytes, so a codec
   with input params refuses here exactly as it does on [decode] rather than
   answering. *)
let wire_size_at ?env:e (t : _ t) buf off =
  require_env ~op:"wire_size_at" t e;
  match (compiled t).wire_size with
  | Fixed n -> n
  | Variable { compute; _ } ->
      compute (runtime ?env:e ()) (Input_end.of_bytes buf) buf off - off

(* A parametric field size resolves through [env], as it does for {!encode}.
   Without one the size stays symbolic and a field measured by it refuses
   rather than reporting 0, which a caller would allocate from. *)
let size_of_value ?env:e (t : _ t) v =
  (* An env that is given must be this codec's and fully bound, as for encode.
     One that is absent is not an error: a caller legitimately asks for the size
     before it has a budget to bind, and every extent the value itself settles
     still answers. *)
  (match e with None -> () | Some _ -> require_env ~op:"size_of_value" t e);
  (compiled t).size_of_value (runtime ?env:e ()) v

let raw_encode ?env:e t v buf off =
  require_env ~op:"encode" t e;
  (compiled t).encode v (runtime ?env:e ()) buf off

let embed_encode_ctx (t : 'r t) v runtime buf off =
  (compiled t).encode v runtime buf off

let embed_encode (t : 'r t) v buf off = embed_encode_ctx t v no_runtime buf off

let validate_runtime t c runtime input_end buf off =
  validate_runtime_of ~scratch:c.decode_scratch ~clear:c.clear_scratch
    ~param_base:c.param_base ~param_handles:t.param_handles
    ~validate_arr:c.validate_arr runtime input_end buf off

let embed_decode_ctx (t : 'r t) runtime input_end buf off =
  let c = compiled t in
  let v = c.decode runtime input_end buf off in
  validate_runtime t c runtime input_end buf off;
  v

let embed_decode (t : 'r t) buf off =
  embed_decode_ctx t no_runtime (Input_end.of_bytes buf) buf off

(* [embed_decode_ctx] with the record construction dropped: the same bounds
   check and the same field pass, and nothing built. *)
let embed_validate_ctx (t : 'r t) runtime input_end buf off =
  (compiled t).validate_ctx runtime input_end buf off

let wire_size_info_ctx (t : _ t) =
  match (compiled t).wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } ->
      `Variable
        (fun runtime input_end buf off ->
          compute runtime input_end buf off - off)

let wire_size_info (t : _ t) =
  match (compiled t).wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } ->
      `Variable
        (fun buf off ->
          compute no_runtime (Input_end.of_bytes buf) buf off - off)

let env (t : _ t) : Param.env =
  {
    Types.codec_id = t.id;
    names =
      Array.of_list
        (List.map (fun (Param.Pack p) -> p.Types.name) t.param_handles);
    slots = Array.make t.n_params 0;
    bound = Array.make t.n_params false;
  }

(* The params of a check pass, held in its slots as [validate] holds them:
   handle [i] at [param_base + i], so a sub-codec's check pass sets an output
   param in the slots that are written back, not in the env behind them. *)
let slots_runtime (t : _ t) (c : _ compiled) arr =
  if t.n_params = 0 then no_runtime
  else
    let find name =
      List.find_index
        (fun (Param.Pack p) -> String.equal p.Types.name name)
        t.param_handles
    in
    Types.eval_ctx
      ~set_param:(fun name value ->
        match find name with
        | Some i -> arr.ints.(c.param_base + i) <- value
        | None -> ())
      (fun name ->
        match find name with Some i -> arr.ints.(c.param_base + i) | None -> 0)

(* The decode of a codec with params, or of one with no plain decode: it checks
   the bounds and then runs the check pass with the params the env binds, as
   [validate] does, so it reports the same first failure; only then does it
   read, so a [map] decoder sees only a record that passed. Last, it writes the
   output params back: the read sees the env the caller bound. *)
let decode_params consume e t (c : _ compiled) buf off =
  let runtime = runtime ?env:e () in
  let input_end = Input_end.of_bytes buf in
  (* A codec with nothing to check and no parameters has only its bounds to
     check, and the read checks them. *)
  let arr =
    if (not c.has_checks) && t.n_params = 0 then None
    else begin
      let min_size =
        match c.wire_size with
        | Fixed n -> n
        | Variable { min_size; _ } -> min_size
      in
      check_decode_bounds c.wire_size min_size runtime input_end buf off;
      let arr = c.decode_scratch () in
      c.clear_scratch arr;
      (match e with
      | Some (e : Param.env) when t.n_params > 0 ->
          Array.blit e.slots 0 arr.ints c.param_base t.n_params
      | _ -> ());
      c.validate_arr arr (slots_runtime t c arr) input_end buf off;
      Some arr
    end
  in
  let v = c.decode runtime input_end buf off in
  let consumed =
    match c.wire_size with
    | Fixed n -> off + n
    | Variable { compute; _ } -> compute runtime input_end buf off
  in
  Types.check_consumption consume ~consumed
    ~available:(Input_end.to_int input_end);
  (match (e, arr) with
  | Some (e : Param.env), Some arr ->
      (* Array slot of [param_handles.(i)] is [param_base + i]; its env slot is
         [i]. Write decoded output params back into the environment. *)
      List.iteri
        (fun i (Param.Pack _p) ->
          let v = arr.ints.(c.param_base + i) in
          e.slots.(i) <- v)
        t.param_handles
  | _ -> ());
  v

let decode_slow consume e t (c : _ compiled) buf off =
  (* Require an env that binds every input param, as encode does: an unbound
     input param resolves a parametric size to 0, silently truncating the field
     and misaligning the rest of the record. This is a usage error, not malformed
     input, so it raises [Invalid_argument] rather than returning a parse error. *)
  require_env ~op:"decode" t e;
  match c.plain_decode with
  | Some decode ->
      (* No params: the plain decode checks before it reads, as [validate]
         does, and leaves only the consumption to check. *)
      let v = decode buf off in
      let consumed =
        match c.wire_size with
        | Fixed n -> off + n
        | Variable { compute; _ } ->
            compute no_runtime (Input_end.of_bytes buf) buf off
      in
      Types.check_consumption consume ~consumed ~available:(Bytes.length buf);
      v
  | None -> decode_params consume e t c buf off

(* The first decode of a codec compiles it. Kept out of [decode_exn] so nothing
   there is live across a call, and its arguments stay in registers. *)
let decode_first consume e t buf off =
  let c = compiled t in
  match (e, consume, c.plain_decode) with
  | None, `Prefix, Some decode -> decode buf off
  | _ -> decode_slow consume e t c buf off

let decode_exn ?(consume = `Prefix) ?env:e t buf off =
  match Atomic.get t.compiled with
  | None -> decode_first consume e t buf off
  | Some c -> (
      match (e, consume, c.plain_decode) with
      | None, `Prefix, Some decode -> decode buf off
      | _ -> decode_slow consume e t c buf off)

let decode ?consume ?env t buf off =
  try Ok (decode_exn ?consume ?env t buf off)
  with Types.Parse_error e -> Error e

let encode_slow e t v buf off =
  if off < 0 then
    Fmt.invalid_arg "Codec.encode %s: negative offset %d" t.name off;
  require_env ~op:"encode" t e;
  let c = compiled t in
  let rt = runtime ?env:e () in
  let expected =
    match c.wire_size with Fixed n -> n | Variable _ -> c.size_of_value rt v
  in
  let actual = c.encode v rt buf off - off in
  (* Underrun = silent data corruption: the trailing bytes the caller
     allocated stay uninitialised and the decoder reads them as part
     of the value. Overrun is loud already. *)
  if actual < expected then
    Fmt.invalid_arg
      "Codec.encode %s: size_of_value reported %d bytes but the encoded form \
       spans %d -- writer wrote fewer bytes than promised"
      t.name expected actual

let encode ?env:e t v buf off =
  match e with
  | None when t.n_params = 0 -> (
      match Atomic.get t.compiled with
      | Some { plain_encode = Some f; _ } -> f v buf off
      | _ -> encode_slow e t v buf off)
  | _ -> encode_slow e t v buf off

let collect_params name (fields : Types.field list) where =
  collect_param_handles name fields where
  |> List.map (fun (Param.Pack p) ->
      { param_name = p.name; param_typ = p.packed_typ; mutable_ = p.mutable_ })

let to_struct t =
  let formals = collect_params t.name t.struct_fields t.where in
  match (formals, t.where) with
  | [], None -> struct' t.name t.struct_fields
  | _ -> param_struct t.name formals ?where:t.where t.struct_fields

let validate_first (t : _ t) buf off = (compiled t).plain_validate buf off

let validate_fixed min_size pass buf off =
  let input_end = Input_end.of_bytes buf in
  if off < 0 || off > Input_end.to_int input_end - min_size then
    min_bounds_error min_size buf off
  else
    match pass with
    | No_checks -> ()
    | _ -> run_plain_pass pass input_end buf off

let validate ?env:e (t : _ t) buf off =
  match e with
  | None when t.n_params = 0 -> (
      match Atomic.get t.compiled with
      | Some c ->
          if c.fixed_frame >= 0 then
            validate_fixed c.fixed_frame c.plain_pass buf off
          else c.plain_validate buf off
      | None -> validate_first t buf off)
  | _ ->
      require_env ~op:"validate" t e;
      let env_slots = Option.map (fun (env : Param.env) -> env.slots) e in
      (compiled t).validate ?env_slots buf off

let encode_fresh ~op ?env t v =
  let buf = Bytes.create (size_of_value ?env t v) in
  encode ?env t v buf 0;
  (try validate ?env t buf 0
   with Types.Parse_error e ->
     Fmt.invalid_arg
       "Codec.%s %s: the encoded record is rejected by validation (%a)" op
       t.name Types.pp_parse_error e);
  buf

let to_bytes ?env t v = encode_fresh ~op:"to_bytes" ?env t v

let to_string ?env t v =
  Bytes.unsafe_to_string (encode_fresh ~op:"to_string" ?env t v)

(* Build a staged reader from field type and access info.
   For Fixed: use build_field_reader which handles Where/Enum/Map.
   For Bitfield: the GADT ensures 'a = int via Bits constructor.
   For Dynamic: compute offset at read time. *)
let rec build_staged_reader : type a.
    a typ -> field_access -> runtime -> Input_end.t -> bytes -> int -> a =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      let read = build_bf_accessor_reader base byte_off shift width in
      fun _runtime _input_end buf base -> read buf base
  | _, Fixed off ->
      let read = build_field_reader_ctx typ off in
      fun runtime input_end buf base -> read runtime input_end buf base
  | _, Dynamic fn ->
      let reader_at_0 = build_field_reader_ctx typ 0 in
      fun runtime input_end buf base ->
        let off = fn runtime input_end buf base in
        reader_at_0 runtime input_end buf (base + off)
  | Byte_slice _, Variable { off; size_fn } ->
      fun runtime input_end buf base ->
        let sz = size_fn runtime input_end buf base in
        Slice.make_or_eod buf ~first:(base + off) ~length:sz
  | Byte_array _, Variable { off; size_fn } ->
      fun runtime input_end buf base ->
        let sz = size_fn runtime input_end buf base in
        Bytes.sub_string buf (base + off) sz
  | Byte_array_where { elt_var; cond; _ }, Variable { off; size_fn } ->
      fun runtime input_end buf base ->
        let sz = size_fn runtime input_end buf base in
        refined_span_reader ~elt_var ~cond buf ~first:(base + off) ~len:sz
  | Byte_slice _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime input_end buf base ->
        let fo = off_fn runtime input_end buf base in
        let sz = size_fn runtime input_end buf base in
        Slice.make_or_eod buf ~first:(base + fo) ~length:sz
  | Byte_array _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime input_end buf base ->
        let fo = off_fn runtime input_end buf base in
        let sz = size_fn runtime input_end buf base in
        Bytes.sub_string buf (base + fo) sz
  | Byte_array_where { elt_var; cond; _ }, Variable_dynamic { off_fn; size_fn }
    ->
      fun runtime input_end buf base ->
        let fo = off_fn runtime input_end buf base in
        let sz = size_fn runtime input_end buf base in
        refined_span_reader ~elt_var ~cond buf ~first:(base + fo) ~len:sz
  | Where { inner; _ }, _ -> build_staged_reader inner access
  | Enum { base; cases; closed; _ }, _ ->
      let read = build_staged_reader base access in
      let check =
        enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
      in
      let at = access_at access in
      fun runtime input_end buf base ->
        check
          ~at:(at runtime input_end buf base)
          (read runtime input_end buf base)
  | Map { inner; decode; index_bound; _ }, _ -> (
      let read = build_staged_reader inner access in
      match index_bound with
      | None ->
          fun runtime input_end buf base ->
            decode (read runtime input_end buf base)
      | Some _ ->
          let decode = Types.map_decode ~index_bound decode in
          let at = access_at access in
          fun runtime input_end buf base ->
            decode
              ~at:(at runtime input_end buf base)
              (read runtime input_end buf base))
  | _, Bitfield _ ->
      invalid_arg "Codec.get: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.get: unsupported variable-size field type"

(* Every leaf encoder checks the value before it writes a byte, so it puts
   down the whole field or nothing. A composite one writes part by part: a
   sub-codec validates the record it has just laid down, a casetype writes its
   tag ahead of its body, an array walks its elements one at a time. Any of
   those can raise with bytes already in the buffer. *)
let rec encoder_writes_before_checking : type a. a typ -> bool = function
  | Codec _ | Casetype _ | Union _ | Array _ | Repeat _ -> true
  | Single_elem { elem; _ } -> encoder_writes_before_checking elem
  | Map { inner; _ } -> encoder_writes_before_checking inner
  | Where { inner; _ } -> encoder_writes_before_checking inner
  | Enum { base; _ } -> encoder_writes_before_checking base
  | Optional { inner; _ } -> encoder_writes_before_checking inner
  | Optional_or { inner; _ } -> encoder_writes_before_checking inner
  | Apply { typ; _ } -> encoder_writes_before_checking typ
  | _ -> false

(* [Codec.set] promises the buffer is untouched when it raises, so a writer
   that can raise mid-field keeps a copy of the bytes it is about to replace
   and puts them back. The width is the value-driven size the record encoder
   budgets the same field with, clipped to what the buffer holds so a short
   buffer still fails through the encoder's own message. *)
let field_writer : type a. a typ -> Types.eval_ctx -> bytes -> int -> a -> unit
    =
 fun typ ->
  let encode = build_field_encoder_ctx typ in
  if not (encoder_writes_before_checking typ) then fun runtime buf off value ->
    ignore (encode runtime buf off value : int)
  else fun runtime buf off value ->
    let room = Bytes.length buf - off in
    let saved =
      if off >= 0 && room > 0 then
        Bytes.sub buf off (min (Types.size_of_typ_value runtime typ value) room)
      else Bytes.empty
    in
    match encode runtime buf off value with
    | _ -> ()
    | exception e ->
        let n = Bytes.length saved in
        if n > 0 then Bytes.blit saved 0 buf off n;
        raise e

(* Build a staged writer from field type and access info. *)
let rec build_staged_writer : type a.
    a typ -> field_access -> runtime -> bytes -> int -> a -> unit =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      let write = build_bf_accessor_writer base byte_off shift width in
      fun _runtime buf base value -> write buf base value
  | _, Fixed off ->
      let encode = field_writer typ in
      fun runtime buf base value -> encode runtime buf (base + off) value
  | _, Dynamic fn ->
      let encode = field_writer typ in
      fun runtime buf base value ->
        let off = fn runtime (Input_end.of_bytes buf) buf base in
        encode runtime buf (base + off) value
  (* A field whose size is only known at run time still owns exactly that many
     bytes: writing the value's length instead would overwrite the fields that
     follow. Check before blitting, with the same error [Codec.encode] gives. *)
  | Byte_slice _, Variable { off; size_fn } ->
      fun runtime buf base value ->
        let src = (value : Slice.t) in
        let len = Slice.length src in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + off) len
  | Byte_array _, Variable { off; size_fn } ->
      fun runtime buf base value ->
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit_string s 0 buf (base + off) len
  | Byte_array_where _, Variable { off; size_fn } ->
      fun runtime buf base value ->
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit_string s 0 buf (base + off) len
  | Byte_slice _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime buf base value ->
        let fo = off_fn runtime (Input_end.of_bytes buf) buf base in
        let src = (value : Slice.t) in
        let len = Slice.length src in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + fo) len
  | Byte_array _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime buf base value ->
        let fo = off_fn runtime (Input_end.of_bytes buf) buf base in
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit_string s 0 buf (base + fo) len
  | Byte_array_where _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime buf base value ->
        let fo = off_fn runtime (Input_end.of_bytes buf) buf base in
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit_string s 0 buf (base + fo) len
  | Where { inner; _ }, _ -> build_staged_writer inner access
  | Enum { base; _ }, _ -> build_staged_writer base access
  | Map { inner; encode; _ }, _ ->
      let write = build_staged_writer inner access in
      fun runtime buf base value -> write runtime buf base (encode value)
  | _, Bitfield _ ->
      invalid_arg "Codec.set: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.set: unsupported variable-size field type"

(* Fast accessor variants never construct or inspect an evaluation context.
   Returning [None] keeps parameter-dependent and compound layouts on the
   context-threaded path above. *)
let rec build_immediate_staged_reader : type a.
    a typ -> field_access -> (bytes -> int -> a) option =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      Some (build_bf_accessor_reader base byte_off shift width)
  | _, Fixed off -> build_immediate_reader typ off
  | Where { inner; _ }, _ -> build_immediate_staged_reader inner access
  | Enum { base; cases; closed; _ }, _ -> (
      match build_immediate_staged_reader base access with
      | None -> None
      | Some read ->
          let check =
            enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
          in
          let off = immediate_access_off access in
          Some (fun buf base -> check ~at:(base + off) (read buf base)))
  | Map { inner; decode; index_bound; _ }, _ -> (
      match build_immediate_staged_reader inner access with
      | None -> None
      | Some read ->
          let decode = Types.map_decode ~index_bound decode in
          let off = immediate_access_off access in
          Some (fun buf base -> decode ~at:(base + off) (read buf base)))
  | _ -> None

let rec build_immediate_staged_writer : type a.
    a typ -> field_access -> (bytes -> int -> a -> unit) option =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      Some (build_bf_accessor_writer base byte_off shift width)
  | _, Fixed off -> (
      match build_immediate_encoder typ with
      | None -> None
      | Some write ->
          Some
            (fun buf base value -> ignore (write buf (base + off) value : int)))
  | Where { inner; _ }, _ -> build_immediate_staged_writer inner access
  | Enum { base; _ }, _ -> build_immediate_staged_writer base access
  | Map { inner; encode; _ }, _ -> (
      match build_immediate_staged_writer inner access with
      | None -> None
      | Some write -> Some (fun buf base value -> write buf base (encode value))
      )
  | _ -> None

let field_access ~op (codec : _ t) (f : (_, _) field) =
  match List.assoc_opt f.name (compiled codec).field_access with
  | Some (id, access) when id = f.id -> access
  | Some _ | None ->
      Fmt.invalid_arg "%s: field %S does not belong to codec %S" op f.name
        codec.name

let[@inline] get (type a r) ?env (codec : r t) (f : (a, r) field) :
    (bytes -> int -> a) Staged.t =
  (* A field's offset can be driven by an input param, and an unbound one reads
     0, which stages a reader onto the bytes in front of the field rather than
     onto the field. That is the same silent misread [decode] refuses, so it is
     refused here on the same terms. *)
  require_env ~op:"get" codec env;
  let access = field_access ~op:"Codec.get" codec f in
  let read = build_staged_reader f.typ access in
  (* The context no longer carries the buffer, so [env] resolves once here
     instead of per staged call. *)
  let rt = runtime ?env () in
  let c = compiled codec in
  match List.assoc_opt f.name c.field_actions with
  | None -> (
      match build_immediate_staged_reader f.typ access with
      | Some read -> Staged.stage read
      | None ->
          Staged.stage (fun buf off -> read rt (Input_end.of_bytes buf) buf off)
      )
  | Some act ->
      (* Reuse the codec's own domain-local scratch rather than minting another
         [Domain.DLS] key here: [get] returns a staged reader, but a caller that
         re-stages instead of reusing would leak a key per call. [decode_scratch]
         is sized [n_array_slots] and only ever touched by one operation at a
         time on a given domain, so sharing it with decode is safe. *)
      let get_arr = c.decode_scratch in
      let n = c.n_array_slots in
      let populate = c.populate in
      let n_params = codec.n_params in
      let sync, blit_input =
        match env with
        | None -> ((fun _arr -> ()), fun _arr -> ())
        | Some (e : Param.env) ->
            let param_handles = codec.param_handles in
            let param_base = c.param_base in
            ( (fun arr ->
                List.iteri
                  (fun i (Param.Pack _p) ->
                    let v = arr.ints.(param_base + i) in
                    e.slots.(i) <- v)
                  param_handles),
              fun arr ->
                if n_params > 0 then
                  Array.blit e.slots 0 arr.ints param_base n_params )
      in
      Staged.stage (fun buf off ->
          let input_end = Input_end.of_bytes buf in
          let v = read rt input_end buf off in
          let arr = get_arr () in
          clear_slots arr n;
          blit_input arr;
          populate arr rt input_end buf off;
          act arr;
          sync arr;
          v)

let[@inline] set (type a r) ?env (codec : r t) (f : (a, r) field) :
    (bytes -> int -> a -> unit) Staged.t =
  (* As for [get]: an unbound param places the write on the wrong bytes, which
     for a writer means overwriting a field the caller never named and leaving
     the one it did name as it was. *)
  require_env ~op:"set" codec env;
  let access = field_access ~op:"Codec.set" codec f in
  let write = build_staged_writer f.typ access in
  let rt = runtime ?env () in
  match build_immediate_staged_writer f.typ access with
  | Some write -> Staged.stage write
  | None -> Staged.stage (fun buf off value -> write rt buf off value)

type 'r valid = { buf : bytes; off : int; codec : int }

let valid_slow env (t : 'r t) buf off =
  validate ?env t buf off;
  { buf; off; codec = t.id }

let valid ?env (t : 'r t) buf off =
  match env with
  | None when t.n_params = 0 -> (
      match Atomic.get t.compiled with
      | Some c when c.fixed_frame >= 0 ->
          validate_fixed c.fixed_frame c.plain_pass buf off;
          { buf; off; codec = t.id }
      | _ -> valid_slow env t buf off)
  | _ -> valid_slow env t buf off

let wrong_valid name =
  Fmt.invalid_arg "Codec.read: the value was not validated by codec %S" name

(* One closure per kind, chosen when the reader is staged, so a read is the
   codec check and one load. *)
let valid_reader : type a r. id:int -> name:string -> a scalar -> r valid -> a =
 fun ~id ~name s ->
  match s with
  | Byte o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else UInt8.unsafe_get v.buf (v.off + o)
  | U16le o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else UInt16.unsafe_le v.buf (v.off + o)
  | U16be o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else UInt16.unsafe_be v.buf (v.off + o)
  | U32le o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else UInt32.unsafe_le v.buf (v.off + o)
  | U32be o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else UInt32.unsafe_be v.buf (v.off + o)
  | Sbyte o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else SInt8.unsafe_get v.buf (v.off + o)
  | I16le o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else SInt16.unsafe_le v.buf (v.off + o)
  | I16be o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else SInt16.unsafe_be v.buf (v.off + o)
  | I32le o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else SInt32.unsafe_le v.buf (v.off + o)
  | I32be o ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else SInt32.unsafe_be v.buf (v.off + o)
  | Bits8 { off; shift; mask } ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else (Unchecked.u8 v.buf (v.off + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else (Unchecked.u16le v.buf (v.off + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      fun v ->
        if v.codec <> id then wrong_valid name
        else (Unchecked.u16be v.buf (v.off + off) lsr shift) land mask

let read (type a r) ?env (codec : r t) (f : (a, r) field) :
    (r valid -> a) Staged.t =
  let access = field_access ~op:"Codec.read" codec f in
  let c = compiled codec in
  let frame =
    match c.wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size
  in
  let id = codec.id and name = codec.name in
  match
    if List.mem_assoc f.name c.field_actions then None
    else field_scalar ~min_wire_size:frame access f.typ
  with
  | Some s -> Staged.stage (valid_reader ~id ~name s)
  | None ->
      let get = Staged.unstage (get ?env codec f) in
      Staged.stage (fun v ->
          if v.codec <> id then wrong_valid name else get v.buf v.off)

let name (t : _ t) = t.name

let rename new_name (t : _ t) =
  { t with name = new_name; compiled = Atomic.make None }

let doc (t : _ t) = t.doc

let field_readers_ctx (t : _ t) =
  List.filter_map
    (fun (name, read) -> Option.map (fun name -> (name, read)) name)
    (compiled t).field_readers

let field_readers t =
  List.map
    (fun (name, read) ->
      (name, fun buf off -> read no_runtime (Input_end.of_bytes buf) buf off))
    (field_readers_ctx t)

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
  match field_access ~op:"Codec.slice_offset" codec f with
  | Fixed off -> Staged.stage (fun _buf base -> base + off)
  | Dynamic fn ->
      Staged.stage (fun buf base ->
          base + fn no_runtime (Input_end.of_bytes buf) buf base)
  | Variable { off; _ } -> Staged.stage (fun _buf base -> base + off)
  | Variable_dynamic { off_fn; _ } ->
      Staged.stage (fun buf base ->
          base + off_fn no_runtime (Input_end.of_bytes buf) buf base)
  | Bitfield _ ->
      Fmt.invalid_arg
        "Codec.slice_offset: field %S is a bitfield, not a byte slice" f.name

let[@inline] slice_length (type r) (codec : r t) (f : (Slice.t, r) field) :
    (bytes -> int -> int) Staged.t =
  match (f.typ, field_access ~op:"Codec.slice_length" codec f) with
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
      Staged.stage (fun buf base ->
          size_fn no_runtime (Input_end.of_bytes buf) buf base)
  | _, (Fixed _ | Dynamic _) ->
      Fmt.invalid_arg "Codec.slice_length: field %S is not a byte slice" f.name
  | _, Bitfield _ ->
      Fmt.invalid_arg
        "Codec.slice_length: field %S is a bitfield, not a byte slice" f.name

(* -- Bitfield batch access -- *)

type bitfield = bf_info

(* The word a bitfield sits in, read with one checked load. Where [Optint.t] is
   the native [int] the word is that [int], with no conversion. *)
let bf_word_reader base byte_off : bytes -> int -> Optint.t =
  let n = bf_base_byte_size base in
  match (Optint.is_immediate, base) with
  | True, U8 ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Unchecked.u8 buf p
  | True, U16 Little ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Unchecked.u16le buf p
  | True, U16 Big ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Unchecked.u16be buf p
  | True, U32 Little ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Int32.to_int (Unchecked.i32le buf p) land mask32
  | True, U32 Big ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Int32.to_int (Unchecked.i32be buf p) land mask32
  | False, U32 Little ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Optint.of_int32 (Unchecked.i32le buf p)
  | False, U32 Big ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Optint.of_int32 (Unchecked.i32be buf p)
  | False, (U8 | U16 _) ->
      fun buf off ->
        let p = off + byte_off in
        Bitfield.check_word buf p n;
        Optint.of_int (Bitfield.read_word base buf p)

let bitfield (type r) (codec : r t) (f : (int, r) field) : bitfield =
  match field_access ~op:"Codec.bitfield" codec f with
  | Bitfield { base; byte_off; shift; width } ->
      (* Dispatch on [base] once at construction so the resulting closure
         is a direct read of the right width with [byte_off] baked in.
         Avoids the partial-application + runtime [match] that
         [Bitfield.read_word base] would create. *)
      let word_reader = bf_word_reader base byte_off in
      let mask = (1 lsl width) - 1 in
      {
        word_reader;
        bf_shift = shift;
        bf_mask = Optint.of_int mask;
        bf_mask_int = mask;
      }
  | _ -> Fmt.invalid_arg "Codec.bitfield: field %S is not a bitfield" f.name

let load_word (bf : bitfield) : (bytes -> int -> Optint.t) Staged.t =
  Staged.stage bf.word_reader

let[@inline always] extract (bf : bitfield) (word : Optint.t) =
  match Optint.is_immediate with
  | True -> (word lsr bf.bf_shift) land bf.bf_mask_int
  | False ->
      Optint.to_int
        (Optint.logand (Optint.shift_right_logical word bf.bf_shift) bf.bf_mask)

(* -- Snapshot: batch bitfield access -- *)

(* The support generated code calls into: casts from an [int] it has read, and
   the cold paths, which hand the failing input to the codec itself so the error
   is the one [decode], [validate] or [encode] raises. *)
module Gen = struct
  let to_struct = to_struct

  (* An [int] as a type the proof says is [int]. *)
  let[@inline] cast : type a. (a, int) Type.eq -> int -> a =
   fun Type.Equal v -> v

  let uint8 x = cast UInt8.int_eq x
  let uint16 x = cast UInt16.int_eq x
  let int8 x = cast SInt8.int_eq x
  let int16 x = cast SInt16.int_eq x

  (* A description of a codec that changes whenever anything the generated
     code relies on changes: each integer's width, sign and byte order, each
     bitfield's width, base word and bit order, each enum's cases, each
     refinement and size, every expression spelled out to its last operator
     and reference. A [map]'s functions cannot be compared; the generated code
     checks each map it names by identity when it loads. [version] names the
     format and what the generator assumes beyond it, such as how bitfields
     pack and in which order checks run: bump it when either changes, so code
     generated by an older Wire refuses to load. *)
  let version = "wire-ocaml-1"
  let endian = function Types.Little -> "le" | Types.Big -> "be"

  let base = function
    | Types.U8 -> "8"
    | Types.U16 e -> "16" ^ endian e
    | Types.U32 e -> "32" ^ endian e

  let order = function Types.Msb_first -> "msb" | Types.Lsb_first -> "lsb"

  let cast = function
    | `U8 -> "u8"
    | `U16 -> "u16"
    | `U32 -> "u32"
    | `U64 -> "u64"

  let quoted = Fmt.(option ~none:(any "-") (fmt "%S"))

  let rec describe_expr : type a. a Types.expr Fmt.t =
   fun ppf e ->
    let op1 tag a = Fmt.pf ppf "%s(%a)" tag describe_expr a in
    let op2 : type x y. string -> x Types.expr -> y Types.expr -> unit =
     fun tag a b -> Fmt.pf ppf "%s(%a,%a)" tag describe_expr a describe_expr b
    in
    match e with
    | Types.Int n -> Fmt.pf ppf "%d" n
    | Types.Int64 n -> Fmt.pf ppf "%LdL" n
    | Types.Bool b -> Fmt.bool ppf b
    | Types.Ref (Types.I, n) -> Fmt.pf ppf "ref(%S)" n
    | Types.Ref (Types.I64, n) -> Fmt.pf ppf "ref64(%S)" n
    | Types.Param_ref p -> Fmt.pf ppf "param(%S)" p.name
    | Types.Sizeof t -> Fmt.pf ppf "sizeof(%a)" describe t
    | Types.Sizeof_this -> Fmt.string ppf "sizeof_this"
    | Types.Field_pos -> Fmt.string ppf "field_pos"
    | Types.Add (a, b) -> op2 "add" a b
    | Types.Sub (a, b) -> op2 "sub" a b
    | Types.Mul (a, b) -> op2 "mul" a b
    | Types.Div (a, b) -> op2 "div" a b
    | Types.Mod (a, b) -> op2 "mod" a b
    | Types.Land (a, b) -> op2 "land" a b
    | Types.Lor (a, b) -> op2 "lor" a b
    | Types.Lxor (a, b) -> op2 "lxor" a b
    | Types.Lnot a -> op1 "lnot" a
    | Types.Lsl (a, b) -> op2 "lsl" a b
    | Types.Lsr (a, b) -> op2 "lsr" a b
    | Types.Land64 (a, b) -> op2 "land64" a b
    | Types.Lsr64 (a, b) -> op2 "lsr64" a b
    | Types.Eq (a, b) -> op2 "eq" a b
    | Types.Ne (a, b) -> op2 "ne" a b
    | Types.Lt (a, b) -> op2 "lt" a b
    | Types.Le (a, b) -> op2 "le" a b
    | Types.Gt (a, b) -> op2 "gt" a b
    | Types.Ge (a, b) -> op2 "ge" a b
    | Types.And (a, b) -> op2 "and" a b
    | Types.Or (a, b) -> op2 "or" a b
    | Types.Not a -> op1 "not" a
    | Types.Cast (c, a) -> op1 ("cast_" ^ cast c) a
    | Types.If_then_else (c, a, b) ->
        Fmt.pf ppf "if(%a,%a,%a)" describe_expr c describe_expr a describe_expr
          b

  (* The types the generator specialises, spelled out; any other is refused
     before its code is written, so its tag alone is enough. *)
  and describe : type a. a Types.typ Fmt.t =
   fun ppf t ->
    let expr = describe_expr in
    match t with
    | Types.Uint8 -> Fmt.string ppf "u8"
    | Types.Int8 -> Fmt.string ppf "i8"
    | Types.Uint16 e -> Fmt.pf ppf "u16%s" (endian e)
    | Types.Int16 e -> Fmt.pf ppf "i16%s" (endian e)
    | Types.Uint32 e -> Fmt.pf ppf "u32%s" (endian e)
    | Types.Int32 e -> Fmt.pf ppf "i32%s" (endian e)
    | Types.Uint64 e -> Fmt.pf ppf "u64%s" (endian e)
    | Types.Int64 e -> Fmt.pf ppf "i64%s" (endian e)
    | Types.Float32 e -> Fmt.pf ppf "f32%s" (endian e)
    | Types.Float64 e -> Fmt.pf ppf "f64%s" (endian e)
    | Types.Uint_var { size; endian = e } ->
        Fmt.pf ppf "uvar%s(%a)" (endian e) expr size
    | Types.Bits { width; base = w; bit_order } ->
        Fmt.pf ppf "bits(%d,%s,%s)" width (base w) (order bit_order)
    | Types.Where { cond; inner } ->
        Fmt.pf ppf "where(%a,%a)" expr cond describe inner
    | Types.Byte_array { size } -> Fmt.pf ppf "bytes(%a)" expr size
    | Types.Byte_slice { size } -> Fmt.pf ppf "slice(%a)" expr size
    | Types.Byte_array_where { size; elt_var; cond } ->
        Fmt.pf ppf "bytes_where(%a,%S,%a)" expr size elt_var expr cond
    | Types.Enum { name; cases; base = inner; closed } ->
        Fmt.pf ppf "enum(%S,%b,[%a],%a)" name closed
          Fmt.(list ~sep:(any ";") (pair ~sep:(any "=") (fmt "%S") int))
          cases describe inner
    | Types.Map { inner; index_bound; _ } ->
        Fmt.pf ppf "map(%a,%a)"
          Fmt.(option ~none:(any "-") int)
          index_bound describe inner
    | Types.Optional { present; inner } ->
        Fmt.pf ppf "optional(%a,%a)" expr present describe inner
    | Types.Codec { codec_name; codec_struct; _ } ->
        Fmt.pf ppf "codec(%S,%a)" codec_name describe_struct codec_struct
    | _ -> Fmt.string ppf "other"

  and describe_struct ppf (s : Types.struct_) =
    Fmt.pf ppf "struct(%S,%d,%a)" s.name (List.length s.params)
      Fmt.(option ~none:(any "-") describe_expr)
      s.where;
    List.iter
      (fun (Types.Field f) ->
        Fmt.pf ppf "[%a:%a:%a:%b]" quoted f.field_name describe f.field_typ
          Fmt.(option ~none:(any "-") describe_expr)
          f.constraint_ (Option.is_some f.action))
      s.fields

  let digest t =
    Digest.to_hex
      (Fmt.kstr Digest.string "%s:%a" version describe_struct (to_struct t))

  let check t d =
    if not (String.equal (digest t) d) then
      Fmt.invalid_arg
        "Wire: the generated code for %s is out of date with its codec; \
         generate it again"
        t.name

  let disagrees t =
    Fmt.invalid_arg "Wire: the generated code for %s disagrees with its codec"
      t.name

  let fail t buf off =
    validate t buf off;
    disagrees t

  let encode_too_short t buf off = encode_too_short t.name (wire_size t) buf off

  let bits_too_wide ~mask v =
    bits_too_wide ~mask v;
    Fmt.invalid_arg "Wire: %d does not fit its bitfield" v

  let byte_length ~expected ~actual =
    Types.check_byte_field_size ~expected ~actual;
    Fmt.invalid_arg "Wire: byte field of length %d" actual

  let get_failed t name buf off =
    let typ =
      List.find_map
        (fun (Types.Field f) ->
          if f.field_name = Some name then Some (Types.Pack_typ f.field_typ)
          else None)
        (to_struct t).fields
    in
    (match (typ, List.assoc_opt name (compiled t).field_access) with
    | Some (Types.Pack_typ typ), Some (_, access) -> (
        match build_immediate_staged_reader typ access with
        | Some read -> ignore (read buf off)
        | None ->
            let read = build_staged_reader typ access in
            ignore (read (runtime ()) (Input_end.of_bytes buf) buf off))
    | _ -> ());
    disagrees t

  let encode_rejected t buf off =
    match validate t buf off with
    | () -> disagrees t
    | exception Types.Parse_error e -> encode_rejected t.name e

  (* A map's functions, typed through its inner type: the wire value is read
     as an [int] (or an [int32] for a 32-bit field), and [dec] decodes it as
     the codec does, the index bound of a lookup checked and a failure placed at
     [at]. *)
  type ('w, 'a) map = { dec : at:int -> 'w -> 'a; enc : 'a -> 'w }
  type 'w view = { of_raw : int -> 'w; to_raw : 'w -> int }
  type 'w view32 = { of_raw32 : int32 -> 'w; to_raw32 : 'w -> int32 }

  let rec int_view : type w. w Types.typ -> w view option = function
    | Types.Uint8 -> Some { of_raw = uint8; to_raw = UInt8.to_int }
    | Types.Int8 -> Some { of_raw = int8; to_raw = SInt8.to_int }
    | Types.Uint16 _ -> Some { of_raw = uint16; to_raw = UInt16.to_int }
    | Types.Int16 _ -> Some { of_raw = int16; to_raw = SInt16.to_int }
    | Types.Bits _ -> Some { of_raw = Fun.id; to_raw = Fun.id }
    | Types.Where { inner; _ } -> int_view inner
    | Types.Enum { base; _ } -> int_view base
    | _ -> None

  let rec int32_view : type w. w Types.typ -> w view32 option = function
    | Types.Uint32 _ ->
        Some { of_raw32 = UInt32.of_int32; to_raw32 = UInt32.to_int32 }
    | Types.Int32 _ ->
        Some { of_raw32 = SInt32.of_int32; to_raw32 = SInt32.to_int32 }
    | Types.Where { inner; _ } -> int32_view inner
    | Types.Enum { base; _ } -> int32_view base
    | _ -> None

  let not_a_map () = invalid_arg "Wire: not a map over an integer"

  let map_int : type a. a Types.typ -> (int, a) map = function
    | Types.Map { inner; decode; encode; index_bound; _ } -> (
        match int_view inner with
        | Some v ->
            let decode = Types.map_decode ~index_bound decode in
            {
              dec = (fun ~at x -> decode ~at (v.of_raw x));
              enc = (fun a -> v.to_raw (encode a));
            }
        | None -> not_a_map ())
    | _ -> not_a_map ()

  let map_int32 : type a. a Types.typ -> (int32, a) map = function
    | Types.Map { inner; decode; encode; index_bound; _ } -> (
        match int32_view inner with
        | Some v ->
            let decode = Types.map_decode ~index_bound decode in
            {
              dec = (fun ~at x -> decode ~at (v.of_raw32 x));
              enc = (fun a -> v.to_raw32 (encode a));
            }
        | None -> not_a_map ())
    | _ -> not_a_map ()

  let map_id : type a. a Types.typ -> int option = function
    | Types.Map { map_id; _ } -> Some map_id
    | _ -> None

  let check_map t name typ =
    let holds =
      List.exists
        (fun (Types.Field f) ->
          f.field_name = Some name
          && Option.is_some (map_id typ)
          && map_id f.field_typ = map_id typ)
        (to_struct t).fields
    in
    if not holds then
      Fmt.invalid_arg
        "Wire: the generated code for %s names a map its field %s no longer \
         has; generate it again"
        t.name name

  (* The bytes a cold path on a bigarray hands to the codec: the record's own
     region, at most [size] bytes from [off], and failures moved back by [off]. *)
  let on_region t buf off f =
    let len = Unchecked.Bigstring.length buf in
    if off < 0 || off > len then f (Unchecked.Bigstring.to_bytes buf) off
    else
      let n = min (min_wire_size t) (len - off) in
      let region =
        Bytes.init n (fun i -> Char.chr (Unchecked.Bigstring.u8 buf (off + i)))
      in
      try f region 0
      with Types.Parse_error e ->
        raise (Types.Parse_error { e with at = e.at + off })

  let fail_bigstring t buf off = on_region t buf off (fail t)

  let get_failed_bigstring t name buf off =
    on_region t buf off (get_failed t name)

  let encode_rejected_bigstring t buf off =
    match on_region t buf off (validate t) with
    | () -> disagrees t
    | exception Types.Parse_error e -> rejected_by_constraints t.name e

  let encode_too_short_bigstring t buf off =
    if off < 0 then
      Fmt.invalid_arg "Codec.encode %s: negative offset %d" t.name off;
    Fmt.invalid_arg "Codec.encode %s: buffer too short (need %d, got %d)" t.name
      (wire_size t)
      (Unchecked.Bigstring.length buf - off)
end

module Testing = struct
  (* Every fast path off: the compiled record of [t] with the plain entry
     points dropped, so each operation takes its general path. *)
  let general (t : 'r t) : 'r t =
    let compile name =
      let c = t.compile name in
      {
        c with
        plain_decode = None;
        plain_encode = None;
        fixed_frame = -1;
        plain_validate = (fun buf off -> c.validate buf off);
        paths = ([] : string list);
      }
    in
    { t with compiled = Atomic.make None; compile }

  let fresh (t : 'r t) : 'r t = { t with compiled = Atomic.make None }

  let read_paths (t : _ t) (c : _ compiled) : string list =
    let frame =
      match c.wire_size with
      | Fixed n -> n
      | Variable { min_size; _ } -> min_size
    in
    let scalar (Types.Field f) =
      match f.field_name with
      | None -> false
      | Some name -> (
          (not (List.mem_assoc name c.field_actions))
          &&
          match List.assoc_opt name c.field_access with
          | Some (_, access) ->
              Option.is_some
                (field_scalar ~min_wire_size:frame access f.field_typ)
          | None -> false)
    in
    if List.exists scalar t.struct_fields then [ "read.scalar" ] else []

  let paths t : string list =
    let c = compiled t in
    c.paths @ if t.n_params = 0 then read_paths t c else []

  let all_paths : string list =
    [ "decode.fixed"; "decode.variable"; "decode.layout" ]
    @ List.concat_map
        (fun p ->
          [ p ^ ".none"; p ^ ".steps"; p ^ ".interval"; p ^ ".byte_table" ])
        [ "pass"; "layout" ]
    @ List.init 4 (fun i -> Fmt.str "encode.staged.%d" (i + 1))
    @ ([ "encode.staged.rows"; "encode.plain"; "encode.blit"; "read.scalar" ]
        : string list)
end
