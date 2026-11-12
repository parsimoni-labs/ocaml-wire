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
    let base = (name, dummy_reader) :: acc.field_readers in
    List.fold_left
      (fun acc' vn -> (vn, dummy_reader) :: acc')
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
        cf.nested_readers @ ((name, cf.int_reader) :: acc.field_readers)
      in
      {
        validators_rev = (cf.validator_off, full) :: acc.validators_rev;
        checkers_rev = (cf.validator_off, check_only) :: acc.checkers_rev;
        field_readers = new_field_readers;
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
  let validate_arr, _populate, _validate =
    build_validators
      (List.rev acc.validators_rev)
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

let view : type f r.
    string -> ?where:bool expr -> ?doc:string -> f -> (f, r) fields -> r t =
 fun name ?where ?doc constructor flds ->
  let rec add : type g. (g, r) record -> (g, r) fields -> r t =
   fun r flds ->
    match flds with [] -> seal r | f :: rest -> add (add_field r f) rest
  in
  add (record_start ?where ?doc name constructor) flds

let v = view

let wire_size_opt (t : _ t) =
  match t.wire_size with Fixed n -> Some n | Variable _ -> None

let wire_size (t : _ t) =
  match t.wire_size with
  | Fixed n -> n
  | Variable _ ->
      invalid_arg
        "Codec.wire_size: variable-size codec (use wire_size_opt or \
         wire_size_at instead)"

let min_wire_size (t : _ t) =
  match t.wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size

let size_of_value_ctx (t : _ t) = t.size_of_value

let raw_decode (t : _ t) buf off =
  t.decode no_runtime (Input_end.of_bytes buf) buf off

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
  | None when not (has_input_params t) -> ()
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
  match t.wire_size with
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
  t.size_of_value (runtime ?env:e ()) v

let raw_encode ?env:e t v buf off =
  require_env ~op:"encode" t e;
  t.encode v (runtime ?env:e ()) buf off

let embed_encode_ctx (t : 'r t) v runtime buf off = t.encode v runtime buf off
let embed_encode (t : 'r t) v buf off = embed_encode_ctx t v no_runtime buf off

let validate_runtime t runtime input_end buf off =
  validate_runtime_of ~scratch:t.decode_scratch ~n_total:t.n_array_slots
    ~param_base:t.param_base ~param_handles:t.param_handles
    ~validate_arr:t.validate_arr runtime input_end buf off

let embed_decode_ctx (t : 'r t) runtime input_end buf off =
  let v = t.decode runtime input_end buf off in
  validate_runtime t runtime input_end buf off;
  v

let embed_decode (t : 'r t) buf off =
  embed_decode_ctx t no_runtime (Input_end.of_bytes buf) buf off

(* [embed_decode_ctx] with the record construction dropped: the same bounds
   check and the same field pass, and nothing built. *)
let embed_validate_ctx (t : 'r t) = t.validate_ctx

let wire_size_info_ctx (t : _ t) =
  match t.wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } ->
      `Variable
        (fun runtime input_end buf off ->
          compute runtime input_end buf off - off)

let wire_size_info (t : _ t) =
  match t.wire_size with
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

let decode_exn ?(consume = `Prefix) ?env:e t buf off =
  (* Require an env that binds every input param, as encode does: an unbound
     input param resolves a parametric size to 0, silently truncating the field
     and misaligning the rest of the record. This is a usage error, not malformed
     input, so it raises [Invalid_argument] rather than returning a parse error. *)
  require_env ~op:"decode" t e;
  let runtime = runtime ?env:e () in
  let input_end = Input_end.of_bytes buf in
  let v = t.decode runtime input_end buf off in
  let arr = t.decode_scratch () in
  clear_slots arr t.n_array_slots;
  (match e with
  | Some (e : Param.env) when t.n_params > 0 ->
      Array.blit e.slots 0 arr.ints t.param_base t.n_params
  | _ -> ());
  t.validate_arr arr runtime input_end buf off;
  let consumed =
    match t.wire_size with
    | Fixed n -> off + n
    | Variable { compute; _ } -> compute runtime input_end buf off
  in
  Types.check_consumption consume ~consumed
    ~available:(Input_end.to_int input_end);
  (match e with
  | Some (e : Param.env) ->
      (* Array slot of [param_handles.(i)] is [param_base + i]; its env slot is
         [i]. Write decoded output params back into the environment. *)
      List.iteri
        (fun i (Param.Pack _p) ->
          let v = arr.ints.(t.param_base + i) in
          e.slots.(i) <- v)
        t.param_handles
  | None -> ());
  v

let decode ?consume ?env t buf off =
  try Ok (decode_exn ?consume ?env t buf off)
  with Types.Parse_error e -> Error e

let encode ?env:e t v buf off =
  require_env ~op:"encode" t e;
  let rt = runtime ?env:e () in
  let expected = t.size_of_value rt v in
  let actual = t.encode v rt buf off - off in
  (* Underrun = silent data corruption: the trailing bytes the caller
     allocated stay uninitialised and the decoder reads them as part
     of the value. Overrun is loud already. *)
  if actual < expected then
    Fmt.invalid_arg
      "Codec.encode %s: size_of_value reported %d bytes but the encoded form \
       spans %d -- writer wrote fewer bytes than promised"
      t.name expected actual

let collect_params name (fields : Types.field list) where =
  collect_param_handles name fields where
  |> List.map (fun (Param.Pack p) ->
      { param_name = p.name; param_typ = p.packed_typ; mutable_ = p.mutable_ })

let to_struct t =
  let formals = collect_params t.name t.struct_fields t.where in
  match (formals, t.where) with
  | [], None -> struct' t.name t.struct_fields
  | _ -> param_struct t.name formals ?where:t.where t.struct_fields

let validate ?env:e (t : _ t) buf off =
  require_env ~op:"validate" t e;
  let env_slots = Option.map (fun (env : Param.env) -> env.slots) e in
  t.validate ?env_slots buf off

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
  | Codec _ | Casetype _ | Array _ | Repeat _ -> true
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
  match List.assoc_opt f.name codec.field_access with
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
  match List.assoc_opt f.name codec.field_actions with
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
      let get_arr = codec.decode_scratch in
      let n = codec.n_array_slots in
      let populate = codec.populate in
      let n_params = codec.n_params in
      let sync, blit_input =
        match env with
        | None -> ((fun _arr -> ()), fun _arr -> ())
        | Some (e : Param.env) ->
            let param_handles = codec.param_handles in
            let param_base = codec.param_base in
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

let name (t : _ t) = t.name
let rename new_name (t : _ t) = { t with name = new_name }
let doc (t : _ t) = t.doc
let field_readers_ctx (t : _ t) = t.field_readers

let field_readers (t : _ t) =
  List.map
    (fun (name, read) ->
      (name, fun buf off -> read no_runtime (Input_end.of_bytes buf) buf off))
    t.field_readers

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

let bitfield (type r) (codec : r t) (f : (int, r) field) : bitfield =
  match field_access ~op:"Codec.bitfield" codec f with
  | Bitfield { base; byte_off; shift; width } ->
      (* Dispatch on [base] once at construction so the resulting closure
         is a direct read of the right width with [byte_off] baked in.
         Avoids the partial-application + runtime [match] that
         [Bitfield.read_word base] would create. *)
      let read_word =
        match base with
        | U8 ->
            fun buf off -> Optint.of_int (Bytes.get_uint8 buf (off + byte_off))
        | U16 Little ->
            fun buf off -> Optint.of_int (Bitfield.u16_le buf (off + byte_off))
        | U16 Big ->
            fun buf off -> Optint.of_int (Bitfield.u16_be buf (off + byte_off))
        | U32 Little ->
            if Bitfield.int_holds_u32 then fun buf off ->
              Optint.of_int (Bitfield.u32_le buf (off + byte_off))
            else fun buf off ->
              Optint.of_int32 (Bytes.get_int32_le buf (off + byte_off))
        | U32 Big ->
            if Bitfield.int_holds_u32 then fun buf off ->
              Optint.of_int (Bitfield.u32_be buf (off + byte_off))
            else fun buf off ->
              Optint.of_int32 (Bytes.get_int32_be buf (off + byte_off))
      in
      (* Same caller-supplied offset as {!get}, so the same span check. *)
      let word = bf_base_byte_size base in
      let word_reader buf off =
        Bitfield.check_word buf (off + byte_off) word;
        read_word buf off
      in
      let mask = (1 lsl width) - 1 in
      { word_reader; bf_shift = shift; bf_mask = Optint.of_int mask }
  | _ -> Fmt.invalid_arg "Codec.bitfield: field %S is not a bitfield" f.name

let load_word (bf : bitfield) : (bytes -> int -> Optint.t) Staged.t =
  Staged.stage bf.word_reader

let[@inline always] extract (bf : bitfield) word =
  Optint.to_int
    (Optint.logand (Optint.shift_right_logical word bf.bf_shift) bf.bf_mask)

(* -- Snapshot: batch bitfield access -- *)
