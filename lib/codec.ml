open Types
module Slice = Bytesrw.Bytes.Slice
open Codec_leaf
open Codec_repr
open Codec_elem
open Codec_seal
open Codec_fault
open Codec_access

(* The codec is built in phases, one module each, in dependency order:
   [Codec_leaf] (one field type at a fixed offset), [Codec_repr] (scratch,
   field access, builder and sealed types), [Codec_elem] (bitfield words and
   repeat elements), [Codec_compile] (per-field plans) and [Codec_seal] (frame,
   checks, decode and encode). [Codec_struct] holds the struct validator. This
   module holds the operations of the interface. *)

type ('a, 'r) field = ('a, 'r) Codec_repr.field
type 'r t = 'r Codec_repr.t

type 'r components = 'r Codec_repr.components = {
  find : 'a. ('a, 'r) field -> (bytes -> int -> ('a, Error.t) result) option;
}

let ( $ ) = ( $ )
let field_name (f : _ field) = f.name
let field_id (f : _ field) = f.id
let field_typ (f : _ field) = f.typ
let field_proj (f : _ field) = f.get
let zeroterm_nul_pos = zeroterm_nul_pos
let blit_string_exact = blit_string_exact
let blit_slice_exact = blit_slice_exact
let pp = pp

type validator = Codec_struct.t

let validator_of_struct = Codec_struct.v

let validate_struct v buf off =
  match Codec_struct.validate v buf off with
  | () -> Ok ()
  | exception exn when Types.is_parse_error exn ->
      let e = Types.parse_error_of exn in
      Error e

let struct_size_of = Codec_struct.size_of
let struct_min_size = Codec_struct.min_size
let wire_size_info_of_validator = Codec_struct.wire_size_info

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

(* A scope, newest first: each name with the field of this codec that brings
   it, [None] for the codec's own field and [Some f] for a field of the
   sub-codec in [f]. A codec's own field shadows a sub-codec's field of the
   same name, and the first sub-codec to bring a name keeps it, as in
   [Codec_elem.extend_scope]. *)
let extend_names name typ scope =
  let taken n = String.equal n name || List.mem_assoc n scope in
  let nested =
    List.filter_map
      (fun n -> if taken n then None else Some (n, Some name))
      (nested_names typ)
  in
  nested @ ((name, None) :: List.remove_assoc name scope)

(* 3D does not see a sub-codec's fields from the struct that embeds it, so an
   expression of the codec naming one has no projection. *)
let check_own codec scope ~user name =
  match List.assoc_opt name scope with
  | Some (Some sub) ->
      Fmt.invalid_arg
        "Codec.v %S: %s refers to %S, a field of the sub-codec in field %S; 3D \
         does not see a sub-codec's fields from the struct that embeds it"
        codec user name sub
  | Some None | None -> ()

(* A constraint or an action may only name the fields before it, and the
   [where] clause may name them all; none may name a field of an embedded
   sub-codec. Checking that here, with the scope and the error the compile
   uses, makes a bad name fail where the codec is built. Returns the scope of
   the [where] clause. *)
let check_field_refs : type f r.
    string -> (f, r) fields -> (string * string option) list =
 fun codec flds ->
  let rec go : type g.
      (string * string option) list ->
      (g, r) fields ->
      (string * string option) list =
   fun scope -> function
     | [] -> scope
     | fld :: rest ->
         let user = Fmt.str "field %S" fld.name in
         let stmts : Types.action_stmt list =
           match fld.action with
           | None -> []
           | Some (Types.Success stmts | Types.Act stmts) -> stmts
         in
         iter_typ_field_refs (check_own codec scope ~user) fld.typ;
         let conds = Option.to_list fld.constraint_ @ typ_where_conds fld.typ in
         if not (List.is_empty conds && List.is_empty stmts) then (
           let vars = List.fold_left action_vars [] stmts in
           let scope = extend_names fld.name Unit scope in
           let check name =
             if not (List.mem name vars) then
               if List.mem_assoc name scope then
                 check_own codec scope ~user name
               else unbound_check_ref name
           in
           List.iter (iter_refs ~param:ignore ~field:check) conds;
           List.iter (iter_stmt_field_refs check) stmts);
         go (extend_names fld.name fld.typ scope) rest
  in
  go [] flds

let check_where_refs codec scope where =
  let check name =
    if List.mem_assoc name scope then
      check_own codec scope ~user:"the where clause" name
    else unbound_where_ref name
  in
  Option.iter (iter_refs ~param:ignore ~field:check) where

let v : type f r.
    string -> ?where:bool expr -> ?doc:string -> f -> (f, r) fields -> r t =
 fun name ?where ?doc constructor flds ->
  let rec struct_fields : type g. (g, r) fields -> Types.field list = function
    | [] -> []
    | f :: rest -> struct_field f :: struct_fields rest
  in
  (* Check the description now, so a malformed codec fails where it is built.
     The compile waits for the first operation that needs it. *)
  let scope = check_field_refs name flds in
  check_where_refs name scope where;
  let struct_fields = struct_fields flds in
  Shape.reject_invalid_codec_shape ?where name struct_fields;
  let param_handles = collect_param_handles name struct_fields where in
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
    param_use = param_use struct_fields;
    where;
    doc;
    compiled = Atomic.make None;
    compile;
    components = None;
  }

(* A codec whose bytes another module reads: its one field stands for the
   whole record, the check and the read are the functions given, and the
   record has no field a binary path places. Nothing is shared with a pass of
   [Codec_seal]: no slots, no fast paths, no walk. *)
let opaque_compiled ~name ~min_size ~check ~read ~extent ~write ~size_of_value =
  let validate_ctx _ input_end buf off = check input_end buf off in
  let checked _ input_end buf off =
    check input_end buf off;
    read input_end buf off
  in
  let plain_validate buf off =
    if off < 0 then Errors.negative_offset off
    else check (Input_end.of_bytes buf) buf off
  in
  let plain_decode buf off =
    if off < 0 then Errors.negative_offset off
    else checked no_runtime (Input_end.of_bytes buf) buf off
  in
  let write v buf off =
    if off < 0 then
      Fmt.invalid_arg "Codec.encode %s: negative offset %d" name off;
    write v buf off
  in
  {
    size_of_value = (fun _ v -> size_of_value v);
    field_access = [];
    field_readers = [];
    field_actions = [];
    decode = checked;
    read = (fun _ input_end buf off -> read input_end buf off);
    plain_decode = Some plain_decode;
    walk = None;
    places = [];
    encode = (fun v _ buf off -> write v buf off);
    wire_size =
      Variable
        {
          min_size;
          compute = (fun _ input_end buf off -> extent input_end buf off);
        };
    validate = (fun ?env_slots:_ buf off -> plain_validate buf off);
    plain_validate;
    plain_encode = Some (fun v buf off -> ignore (write v buf off : int));
    fixed_frame = -1;
    plain_pass = No_checks;
    faults = validate_ctx;
    validate_ctx;
    validate_sized =
      (fun rt input_end buf off _ -> validate_ctx rt input_end buf off);
    check_end = (fun _ _ _ -> -2);
    validate_arr =
      (fun _ rt input_end buf off -> validate_ctx rt input_end buf off);
    clear_scratch = ignore;
    populate = (fun _ _ _ _ _ -> ());
    n_array_slots = 0;
    has_checks = true;
    decode_scratch = (fun () -> { ints = [||]; int64s = [||] });
    param_base = 0;
    paths = [];
  }

let opaque ~name field ~min_size ~check ~read ~extent ~write ~size_of_value
    ~components =
  let compile _ =
    opaque_compiled ~name ~min_size ~check ~read ~extent ~write ~size_of_value
  in
  {
    id = Atomic.fetch_and_add id_counter 1;
    name;
    struct_fields = [ field ];
    n_params = 0;
    param_handles = [];
    param_use = Stable;
    where = None;
    doc = None;
    compiled = Atomic.make None;
    compile;
    components = Some components;
  }

(* Two domains may both compile a codec neither has used; the first to publish
   wins and the other's result is dropped. Compiling is pure, so the two are
   interchangeable. *)
let rec compile (t : 'r t) : 'r compiled =
  let c = t.compile t.name in
  if Atomic.compare_and_set t.compiled None (Some c) then c
  else match Atomic.get t.compiled with Some c -> c | None -> compile t

let[@inline] compiled (t : 'r t) : 'r compiled =
  match Atomic.get t.compiled with Some c -> c | None -> compile t

let size (t : _ t) =
  match (compiled t).wire_size with Fixed n -> Some n | Variable _ -> None

let min_size (t : _ t) =
  match (compiled t).wire_size with
  | Fixed n -> n
  | Variable { min_size; _ } -> min_size

let encoded_size_ctx (t : _ t) = (compiled t).size_of_value

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
   answering. Bytes that hold no size are input, answered with their error; a
   negative offset is misuse. *)
let size_at ?env:e (t : _ t) buf off =
  require_env ~op:"size_at" t e;
  if off < 0 then Errors.negative_offset off
  else
    match (compiled t).wire_size with
    | Fixed n -> Ok n
    | Variable { compute; _ } -> (
        match compute (runtime ?env:e ()) (Input_end.of_bytes buf) buf off with
        | stop -> Ok (stop - off)
        | exception exn when Types.is_parse_error exn ->
            Error (Types.parse_error_of exn))

(* A parametric field size resolves through [env], as it does for {!encode}.
   Without one the size stays symbolic and a field measured by it refuses
   rather than reporting 0, which a caller would allocate from. *)
let encoded_size ?env:e (t : _ t) v =
  (* An env that is given must be this codec's and fully bound, as for encode.
     One that is absent is not an error: a caller legitimately asks for the size
     before it has a budget to bind, and every extent the value itself settles
     still answers. *)
  (match e with None -> () | Some _ -> require_env ~op:"encoded_size" t e);
  (compiled t).size_of_value (runtime ?env:e ()) v

let raw_encode ?env:e t v buf off =
  require_env ~op:"encode" t e;
  (compiled t).encode v (runtime ?env:e ()) buf off

let embed_encode_ctx (t : 'r t) v runtime buf off =
  (compiled t).encode v runtime buf off

let embed_encode (t : 'r t) v buf off = embed_encode_ctx t v no_runtime buf off

(* A sub-codec read inside the read phase of an enclosing decode builds its
   record and nothing else: the enclosing check pass has run this codec's
   checks where it sits, so checking again would check a record once per level
   it is nested under. Read anywhere else, it checks first and reads with the
   context marked, so the records under it are checked by this pass alone. *)
let embed_decode_ctx (t : 'r t) runtime input_end buf off =
  let c = compiled t in
  if Types.is_checked runtime then c.read runtime input_end buf off
  else
    match t.param_use with
    | Stable ->
        c.validate_ctx runtime input_end buf off;
        c.read (Types.checked_eval_ctx runtime) input_end buf off
    | Settled | Unsettled ->
        (* The check pass below writes its output parameters to [runtime],
           and it read them from there: read first, with the same values. *)
        let v = c.decode runtime input_end buf off in
        c.validate_ctx runtime input_end buf off;
        v

let embed_decode (t : 'r t) buf off =
  embed_decode_ctx t no_runtime (Input_end.of_bytes buf) buf off

(* [embed_decode_ctx] with the record construction dropped: the same bounds
   check and the same field pass, and nothing built. *)
let embed_validate_ctx (t : 'r t) runtime input_end buf off =
  (compiled t).validate_ctx runtime input_end buf off

let embed_validate_sized_ctx (t : 'r t) runtime input_end buf off end_off =
  (compiled t).validate_sized runtime input_end buf off end_off

let embed_check_end (t : 'r t) input_end buf off =
  (compiled t).check_end input_end buf off

let embed_walk (t : 'r t) = (compiled t).walk

type _ Types.codec_ref += Codec_ref : 'r t -> 'r Types.codec_ref

let embed_ref t = Codec_ref t

let wire_size_info_ctx (t : _ t) =
  match (compiled t).wire_size with
  | Fixed n -> `Fixed n
  | Variable { min_size; compute } ->
      (* A record measured inside another is framed first, as its check frames
         it: one that does not fit before [input_end] fails on its frame, not
         on the first length field the measure reads past it. *)
      `Variable
        (fun runtime input_end buf off ->
          check_min_bounds min_size input_end off;
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
    wide = params_wide t.param_handles;
    wides = Array.make t.n_params 0L;
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
      ~param64:(fun name ->
        match find name with
        | Some i -> arr.int64s.(c.param_base + i)
        | None -> 0L)
      ~set_param64:(fun name value ->
        match find name with
        | Some i -> arr.int64s.(c.param_base + i) <- value
        | None -> ())
      (fun name ->
        match find name with Some i -> arr.ints.(c.param_base + i) | None -> 0)

(* The params a check pass left in its slots, for the read of a [Settled]
   codec: each field that reads one was checked with these values. *)
let settled_runtime (t : _ t) (c : _ compiled) arr =
  let slots = Array.sub arr.ints c.param_base t.n_params in
  Types.eval_ctx (fun name ->
      match
        List.find_index
          (fun (Param.Pack p) -> String.equal p.Types.name name)
          t.param_handles
      with
      | Some i -> slots.(i)
      | None -> 0)

(* The decode of a codec with params, or of one with no plain decode: it checks
   the bounds and then runs the check pass with the params the env binds, as
   [validate] does, so it reports the same first failure; only then does it
   read, so a [map] decoder sees only a record that passed. Last, it writes the
   output params back. *)
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
      check_frame ~faults:c.faults min_size runtime input_end buf off;
      let arr = c.decode_scratch () in
      c.clear_scratch arr;
      (match e with
      | Some (e : Param.env) when t.n_params > 0 ->
          Array.blit e.slots 0 arr.ints c.param_base t.n_params;
          Array.blit e.wides 0 arr.int64s c.param_base t.n_params
      | _ -> ());
      c.validate_arr arr (slots_runtime t c arr) input_end buf off;
      check_extent c.wire_size min_size runtime input_end buf off;
      Some arr
    end
  in
  let read_runtime =
    match (arr, t.param_use) with
    | Some _, Stable -> Types.checked_eval_ctx runtime
    | Some arr, Settled -> Types.checked_eval_ctx (settled_runtime t c arr)
    | Some _, Unsettled | None, _ -> runtime
  in
  let v =
    if Types.is_checked read_runtime then c.read read_runtime input_end buf off
    else c.decode read_runtime input_end buf off
  in
  let consumed =
    match c.wire_size with
    | Fixed n -> off + n
    | Variable { compute; _ } -> compute read_runtime input_end buf off
  in
  Types.check_consumption consume ~consumed
    ~available:(Input_end.to_int input_end);
  (match (e, arr) with
  | Some (e : Param.env), Some arr ->
      (* Array slot of [param_handles.(i)] is [param_base + i]; its env slot is
         [i]. Write decoded output params back into the environment. *)
      Array.blit arr.ints c.param_base e.slots 0 t.n_params;
      Array.blit arr.int64s c.param_base e.wides 0 t.n_params
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

let decode_general consume e t buf off =
  match Atomic.get t.compiled with
  | None -> decode_first consume e t buf off
  | Some c -> (
      match (e, consume, c.plain_decode) with
      | None, `Prefix, Some decode -> decode buf off
      | _ -> decode_slow consume e t c buf off)

(* The common call, no [consume] and no [env] on a compiled codec without
   params, is one test before its decoder runs. *)
let decode_exn ?consume ?env:e t buf off =
  match (consume, e, Atomic.get t.compiled) with
  | None, None, Some { plain_decode = Some decode; _ } -> decode buf off
  | _ -> decode_general (Option.value consume ~default:`Prefix) e t buf off

let decode ?consume ?env t buf off =
  match decode_exn ?consume ?env t buf off with
  | v -> Ok v
  | exception exn when Types.is_parse_error exn ->
      Error (Types.parse_error_of exn)

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
      "Codec.encode %s: encoded_size reported %d bytes but the encoded form \
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
  let fields = bound_regions ~params:formals t.where t.struct_fields in
  match (formals, t.where) with
  | [], None -> struct' t.name fields
  | _ -> param_struct t.name formals ?where:t.where fields

let validate_first (t : _ t) buf off = (compiled t).plain_validate buf off

(* [valid] inlines this, so the error stays a call. *)
let validate_fixed ~faults min_size pass buf off =
  let input_end = Input_end.of_bytes buf in
  if off < 0 || off > Input_end.to_int input_end - min_size then
    short ~faults min_size buf off
  else
    match pass with
    | No_checks -> ()
    | Word { q; l; r; m; lo; hi; _ }
      when Codec_intervals.within buf off q l r m lo hi ->
        ()
    | Intervals { words = Some f; _ } when f buf off -> ()
    | _ -> run_plain_pass pass input_end buf off

let validate_exn ?env:e (t : _ t) buf off =
  match e with
  | None when t.n_params = 0 -> (
      match Atomic.get t.compiled with
      | Some c ->
          if c.fixed_frame >= 0 then
            validate_fixed ~faults:c.faults c.fixed_frame c.plain_pass buf off
          else c.plain_validate buf off
      | None -> validate_first t buf off)
  | _ ->
      require_env ~op:"validate" t e;
      let env_slots = Option.map (fun (env : Param.env) -> env.slots) e in
      (compiled t).validate ?env_slots buf off

let validate ?env t buf off =
  match validate_exn ?env t buf off with
  | () -> Ok ()
  | exception exn when Types.is_parse_error exn ->
      let e = Types.parse_error_of exn in
      Error e

let encode_fresh ~op ?env t v =
  let buf = Bytes.create (encoded_size ?env t v) in
  encode ?env t v buf 0;
  (try validate_exn ?env t buf 0
   with exn when Types.is_parse_error exn ->
     let e = Types.parse_error_of exn in
     Fmt.invalid_arg
       "Codec.%s %s: the encoded record is rejected by validation (%a)" op
       t.name Error.pp e);
  buf

let to_bytes ?env t v = encode_fresh ~op:"to_bytes" ?env t v

let to_string ?env t v =
  Bytes.unsafe_to_string (encode_fresh ~op:"to_string" ?env t v)

let field_access ~op (codec : _ t) (f : (_, _) field) =
  match List.assoc_opt f.name (compiled codec).field_access with
  | Some (id, access) when id = f.id -> access
  | Some _ | None ->
      Fmt.invalid_arg "%s: field %S does not belong to codec %S" op f.name
        codec.name

(* What a read of the field [name] answers: its value, or the error of the
   bytes it read, under the field's name. A negative offset is misuse. *)
let[@inline never] field_error name (e : parse_error) =
  Error { e with field = name :: e.field }

let[@inline never] field_eof name ~at ~width len =
  Error
    {
      at;
      field = [ name ];
      kind =
        Unexpected_eof
          { expected = width; got = (if at > len then 0 else len - at) };
    }

(* The reader of a field with an action: the action runs on the record's
   slots, its error placed at the field as [decode] places it. *)
let action_reader ?env (codec : _ t) (c : _ compiled) rt access act read name =
  (* Reuse the codec's own domain-local scratch rather than minting another
         [Domain.DLS] key here: [get c f] returns a reader, but a caller that
         makes it again instead of reusing it would leak a key per call. [decode_scratch]
         is sized [n_array_slots] and only ever touched by one operation at a
         time on a given domain, so sharing it with decode is safe. *)
  let get_arr = c.decode_scratch in
  let n = c.n_array_slots in
  let populate = c.populate in
  let n_params = codec.n_params in
  let min_size =
    match c.wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size
  in
  let at = access_at access in
  let sync, blit_input =
    match env with
    | None -> ((fun _arr -> ()), fun _arr -> ())
    | Some (e : Param.env) ->
        let param_base = c.param_base in
        ( (fun arr ->
            Array.blit arr.ints param_base e.slots 0 n_params;
            Array.blit arr.int64s param_base e.wides 0 n_params),
          fun arr ->
            if n_params > 0 then begin
              Array.blit e.slots 0 arr.ints param_base n_params;
              Array.blit e.wides 0 arr.int64s param_base n_params
            end )
  in
  (* The action reads the record's other fields, so the record must fit;
         its error is the field's, as [decode] places it. *)
  let run buf off =
    let input_end = Input_end.of_bytes buf in
    let v = read rt input_end buf off in
    check_min_bounds min_size input_end off;
    let arr = get_arr () in
    clear_slots arr n;
    blit_input arr;
    populate arr rt input_end buf off;
    (try act arr
     with exn when Types.is_parse_error exn ->
       let e = Types.parse_error_of exn in
       relocate at rt input_end buf off e);
    sync arr;
    v
  in
  fun buf off ->
    if off < 0 then Errors.negative_offset off
    else
      match run buf off with
      | v -> Ok v
      | exception exn when Types.is_parse_error exn ->
          let e = Types.parse_error_of exn in
          field_error name e

(* A field of a codec whose bytes another module reads: that module's reader,
   which answers a negative offset as [get] does. *)
let component_get (codec : _ t) (c : _ components) (f : (_, _) field) =
  match c.find f with
  | None ->
      Fmt.invalid_arg "Codec.get: field %S does not belong to codec %S" f.name
        codec.name
  | Some read ->
      fun buf off ->
        if off < 0 then Errors.negative_offset off else read buf off

let[@inline] binary_get (type a r) ?env (codec : r t) (f : (a, r) field) :
    bytes -> int -> (a, parse_error) result =
  (* A field's offset can be driven by an input param, and an unbound one reads
     0, which places a reader on the bytes in front of the field rather than
     onto the field. That is the same silent misread [decode] refuses, so it is
     refused here on the same terms. *)
  require_env ~op:"get" codec env;
  let access = field_access ~op:"Codec.get" codec f in
  let read = build_staged_reader f.typ access in
  let name = f.name in
  (* The context no longer carries the buffer, so [env] resolves once here
     instead of per staged call. *)
  let rt = runtime ?env () in
  let c = compiled codec in
  match List.assoc_opt f.name c.field_actions with
  | None -> (
      match
        ( build_immediate_get ~name f.typ access,
          build_immediate_staged_reader f.typ access,
          immediate_extent f.typ access )
      with
      | Some get, _, _ -> get
      | None, Some read, Some (off, width) -> (
          fun buf base ->
            let len = Bytes.length buf in
            if base < 0 then Errors.negative_offset base
            else if base > len - off - width then
              field_eof name ~at:(base + off) ~width len
            else
              match read buf base with
              | v -> Ok v
              | exception exn when Types.is_parse_error exn ->
                  let e = Types.parse_error_of exn in
                  field_error name e)
      | _ -> (
          fun buf base ->
            if base < 0 then Errors.negative_offset base
            else
              match read rt (Input_end.of_bytes buf) buf base with
              | v -> Ok v
              | exception exn when Types.is_parse_error exn ->
                  let e = Types.parse_error_of exn in
                  field_error name e))
  | Some act -> action_reader ?env codec c rt access act read name

let[@inline] get ?env (codec : _ t) f =
  match codec.components with
  | Some c -> component_get codec c f
  | None -> binary_get ?env codec f

(* {1 Definitions, layers and paths} *)

(* A definition as the public API holds it: a field bound to a record, or a
   path to a field of a record a layer holds. *)
type ('a, 'r) def =
  | Def : ('a, 'r) field -> ('a, 'r) def
  | Path : ('s, 'r) layer * ('a, 's) def -> ('a option, 'r) def

and ('s, 'r) layer =
  | Layer : ('s, 'r) field -> ('s, 'r) layer
  | Opt_layer : ('s option, 'r) field -> ('s, 'r) layer
  | Under : ('s, 'r) layer * ('t, 's) layer -> ('t, 'r) layer

(* The codec a field of type [t] holds, when it holds one as a layer does. *)
let rec held : type s. s typ -> s t option = function
  | Codec { codec_ref = Codec_ref c; _ } -> Some c
  | Single_elem { elem; _ } -> held elem
  | Optional_or { inner; _ } -> held inner
  | _ -> None

let layer : type s r. (s, r) def -> (s, r) layer = function
  | Def f -> (
      match held f.typ with
      | Some _ -> Layer f
      | None ->
          Fmt.invalid_arg "Codec.layer: field %S does not hold a record" f.name)
  | Path _ -> invalid_arg "Codec.layer: a path is not a layer; use ( // )"

let opt_layer : type s r. (s option, r) def -> (s, r) layer = function
  | Def ({ typ = Optional { inner; _ }; _ } as f) -> (
      match held inner with
      | Some _ -> Opt_layer f
      | None ->
          Fmt.invalid_arg "Codec.opt_layer: field %S does not hold a record"
            f.name)
  | Def f ->
      Fmt.invalid_arg "Codec.opt_layer: field %S is not a Field.optional" f.name
  | Path _ -> invalid_arg "Codec.opt_layer: a path is not a layer; use ( // )"

let path l f = Path (l, f)
let under l l' = Under (l, l')

(* The definitions a fold hands its function, in order, as the constructor of
   [v] takes its fields: [[]] ends in the accumulator ['k]. *)
module Defs = struct
  type ('f, 'k, 'r) t =
    | [] : ('k, 'k, 'r) t
    | ( :: ) : ('a, 'r) def * ('f, 'k, 'r) t -> ('a -> 'f, 'k, 'r) t
end

(* The fields of [v] given as definitions; a path names no field of the
   record. *)
let rec fields_of_defs : type f r. (f, r, r) Defs.t -> (f, r) fields = function
  | Defs.[] -> []
  | Defs.(Def f :: rest) -> f :: fields_of_defs rest
  | Defs.(Path _ :: _) ->
      invalid_arg "Codec.v: a path names a field of a layer, not of the record"

(* {1 Checked views} *)

(* A pass made of more than interval tests, answered without its error. *)
let[@inline never] steps_pass pass input_end buf off =
  match run_plain_pass pass input_end buf off with
  | () -> true
  | exception exn when Types.is_parse_error exn -> false

let[@inline] passes pass input_end buf off =
  match pass with
  | No_checks -> true
  | Word { q; l; r; m; lo; hi; _ } ->
      Codec_intervals.within buf off q l r m lo hi
  | Intervals { words = Some f; _ } -> f buf off
  | Intervals { flat; tables; words = None; _ } ->
      Codec_intervals.scan_intervals flat tables buf off (Array.length flat) 0
      < 0
  | Steps _ -> steps_pass pass input_end buf off

module View = struct
  type 'r codec = 'r t
  type ('b, 'r) check = int
  type ('b, 'r) t = int

  (* A codec whose views read with no environment: its checks are the plain
     pass, or its walk, which no parameter changes. *)
  let[@inline] plain env (t : _ codec) =
    match env with None -> t.n_params = 0 | Some _ -> false

  let[@inline never] pass_slow pass buf off =
    passes pass (Input_end.of_bytes buf) buf off

  (* Whether the record at [off] passes [validate] in the bytes before [stop],
     without its error, [0 <= off <= stop <= Bytes.length buf]. A fixed-size
     codec runs its plain pass, a codec with a walk the walk, and any other
     [validate]'s checks. *)
  let accepts env (t : _ codec) buf off stop =
    let c = compiled t in
    let input_end = Input_end.narrow (Input_end.of_bytes buf) stop in
    if plain env t && c.fixed_frame >= 0 then
      off <= stop - c.fixed_frame && passes c.plain_pass input_end buf off
    else
      match c.walk with
      | Some w when plain env t -> Codec_walk.accepts w buf off input_end
      | _ -> (
          match
            if stop = Bytes.length buf then validate_exn ?env t buf off
            else c.validate_ctx (runtime ?env ()) input_end buf off
          with
          | () -> true
          | exception exn when Types.is_parse_error exn -> false)

  (* [stop] is the end of the window: [len] bytes from [off], or the end of
     the buffer. A window the buffer does not hold is refused. *)
  let[@inline never] check_slow env len (t : _ codec) buf off =
    require_env ~op:"View.check" t env;
    let length = Bytes.length buf in
    let stop =
      match len with
      | None -> length
      | Some len -> if len < 0 || len > length - off then -1 else off + len
    in
    if off < 0 || off > length || stop < 0 then -1
    else if accepts env t buf off stop then off
    else -1

  (* The frame and a pass of one word test inline, a pass of word tests one
     closure call, any other pass, a window and any codec [validate] checks
     out of line. *)
  let[@inline] check ?env ?len (t : 'r codec) b ~off =
    let buf = (b : _ Buf.t :> bytes) in
    match (env, len, Atomic.get t.compiled) with
    | None, None, Some c when c.fixed_frame >= 0 && t.n_params = 0 -> (
        if off < 0 || off > Bytes.length buf - c.fixed_frame then -1
        else
          match c.plain_pass with
          | No_checks -> off
          | Word { q; l; r; m; lo; hi; _ } ->
              if Codec_intervals.within buf off q l r m lo hi then off else -1
          | Intervals { words = Some f; _ } -> if f buf off then off else -1
          | pass -> if pass_slow pass buf off then off else -1)
    | _ -> check_slow env len t buf off

  let[@inline] is_ok k = k >= 0

  let[@inline never] refused () =
    invalid_arg "Codec.View.get: the check refused the record"

  let[@inline] get k = if k < 0 then refused () else k
  let offset r = r

  let frame_of (c : _ compiled) =
    match c.wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size

  (* {2 Zeros} *)

  (* What a read answers where the bytes written after the check move its
     field out of the buffer: [0] for an integer, empty bytes, an absent
     option. [None] for a type with no such value, such as a map whose decoder
     refuses its wire zero. *)
  let rec zero_of : type a. a typ -> a option = function
    | Uint8 -> Some UInt8.zero
    | Uint16 _ -> Some UInt16.zero
    | Uint32 _ -> Some UInt32.zero
    | Uint64 _ -> Some UInt64.zero
    | Int8 -> Some SInt8.zero
    | Int16 _ -> Some SInt16.zero
    | Int32 _ -> Some SInt32.zero
    | Int64 _ -> Some 0L
    | Float32 _ -> Some 0.
    | Float64 _ -> Some 0.
    | Uint_var _ -> Some Optint.Int63.zero
    | Bits _ -> Some 0
    | Unit -> Some ()
    | All_bytes -> Some ""
    | All_zeros -> Some ""
    | Zeroterm -> Some ""
    | Zeroterm_at_most _ -> Some ""
    | Byte_array _ -> Some ""
    | Byte_array_where _ -> Some ""
    | Byte_slice _ -> Some Slice.eod
    | Where { inner; _ } -> zero_of inner
    | Enum { base; _ } -> zero_of base
    | Single_elem { elem; _ } -> zero_of elem
    | Apply { typ; _ } -> zero_of typ
    | Optional _ -> Some None
    | Optional_or { default; _ } -> Some default
    | Array { seq = Seq_map { empty; finish; _ }; _ } -> Some (finish empty)
    | Repeat { seq = Seq_map { empty; finish; _ }; _ } -> Some (finish empty)
    | Map { inner; decode; _ } -> (
        match zero_of inner with
        | Some w -> (
            match decode w with
            | v -> Some v
            | exception (Invalid_argument _ | Failure _ | Not_found) -> None
            | exception exn when Types.is_parse_error exn -> None)
        | None -> None)
    | Casetype _ | Union _ | Struct _ | Type_ref _ | Qualified_ref _ | Codec _
    | Der _ ->
        None

  (* A read that answers [zero] where its field has left the buffer, or
     refuses with [Invalid_argument] for a type with no zero. *)
  let[@inline never] no_zero name =
    Fmt.invalid_arg
      "Codec.View.read: field %S no longer lies in the buffer, and its type \
       has no zero"
      name

  (* A staged read raises [Invalid_argument] on bytes outside the buffer. *)
  let left_the_buffer = function
    | Invalid_argument _ | Types.Unwalked -> true
    | exn -> Types.is_parse_error exn

  let or_zero name zero read buf r =
    match read buf r with
    | v -> v
    | exception exn when left_the_buffer exn -> (
        match zero with Some z -> z | None -> no_zero name)

  (* {2 Fields} *)

  type ('a, 'r) rfield = {
    rd : 'a Codec_view.read;
    wr : bytes -> int -> 'a -> unit;
  }

  (* The type a view reads a field as, under the [where] and [enum] the check
     made: a view reads what is there, as the module [Wire_ocaml] writes does. *)
  let rec read_as : type a. a typ -> a typ = function
    | Where { inner; _ } -> read_as inner
    | Enum { base; _ } -> read_as base
    | t -> t

  let[@inline never] out_of_place codec name =
    Fmt.invalid_arg
      "Codec.View.field: field %S of codec %S lies where an input parameter \
       places it; read it with Codec.decode"
      name codec

  (* A field that no direct read takes: read with its staged reader, or, for a
     shape with none, as the projection of the record decoded with no check
     again. *)
  let general (type a r) (codec : r codec) (c : r compiled)
      (f : (a, r) Codec_repr.field) access : a Codec_view.read =
    if has_input_params codec then out_of_place codec.name f.name;
    let zero = zero_of f.typ in
    let read =
      match build_staged_reader f.typ access with
      | read -> fun buf r -> read no_runtime (Input_end.of_bytes buf) buf r
      | exception Invalid_argument _ ->
          fun buf r ->
            f.get (c.read checked_runtime (Input_end.of_bytes buf) buf r)
    in
    Get (or_zero f.name zero read)

  let resolve_read (type a r) (codec : r codec) (f : (a, r) Codec_repr.field)
      (access : field_access) : a Codec_view.read =
    let c = compiled codec in
    let size = frame_of c in
    let typ = read_as f.typ in
    match field_scalar ~min_wire_size:size access typ with
    | Some s -> (
        match
          Option.bind
            (Codec_view.bits_of_scalar s)
            (Codec_view.word_field ~size)
        with
        | Some f -> f
        | None -> Codec_view.scalar s)
    | None -> (
        match (typ, access) with
        | Bits _, Bitfield { base = U32 Big; byte_off = o; shift; width }
          when o >= 0 && o <= size - 4 -> (
            match
              Codec_view.word_field ~size
                { o; n = 4; s = shift; w = width; eq = Equal }
            with
            | Some f -> f
            | None ->
                let read = build_bf_accessor_reader (U32 Big) o shift width in
                Get (or_zero f.name (Some 0) read))
        | Byte_slice { size = Int length }, Fixed off
          when off >= 0 && length >= 0 && off <= size - length ->
            Bytes { off; length }
        | Byte_array { size = Int length }, Fixed off
          when off >= 0 && length >= 0 && off <= size - length ->
            Get
              (fun buf r ->
                if r + off > Bytes.length buf - length then ""
                else Bytes.sub_string buf (r + off) length)
        | _ -> general codec c f access)

  let[@inline never] no_place codec name =
    Fmt.invalid_arg
      "Codec.View.set: field %S of codec %S has no constant place in the record"
      name codec

  let[@inline never] out_of_buffer name =
    Fmt.invalid_arg "Codec.View.set: field %S does not lie in the buffer" name

  (* A write in place of a field at a constant place, its bytes tested to lie
     in the buffer first. A write that reads the record to size the field
     answers bytes that size nothing with [Invalid_argument]. *)
  let resolve_write (type a r) (codec : r codec) (f : (a, r) Codec_repr.field)
      (access : field_access) : bytes -> int -> a -> unit =
    let none _ _ _ = no_place codec.name f.name in
    let parametric = has_input_params codec in
    match access with
    | Dynamic _ | Variable_dynamic _ -> none
    | Variable _ when parametric -> none
    | Fixed _ | Bitfield _ | Variable _ -> (
        let bounded write =
          match immediate_extent f.typ access with
          | Some (off, width) ->
              fun buf r x ->
                if r + off > Bytes.length buf - width then out_of_buffer f.name
                else write buf r x
          | None -> write
        in
        match build_immediate_staged_writer f.typ access with
        | Some write -> bounded write
        | None -> (
            match build_staged_writer f.typ access with
            | write ->
                bounded (fun buf r x ->
                    match write no_runtime buf r x with
                    | () -> ()
                    | exception exn when Types.is_parse_error exn ->
                        out_of_buffer f.name)
            | exception Invalid_argument _ -> none))

  let[@inline never] changed codec name =
    Fmt.invalid_arg
      "Codec.View.read: field %S of codec %S no longer decodes: its bytes \
       changed after the check"
      name codec

  (* A field of a codec whose bytes another module reads ([opaque]): that
     module's reader, which a check of the record let decode, and no write,
     since its lengths move every offset. *)
  let component (type a r) (codec : r codec) (c : r components)
      (f : (a, r) Codec_repr.field) : (a, r) rfield =
    match c.find f with
    | None ->
        Fmt.invalid_arg "Codec.View.field: field %S does not belong to codec %S"
          f.name codec.name
    | Some read ->
        {
          rd =
            Get
              (fun buf r ->
                match read buf r with
                | Ok x -> x
                | Error _ -> changed codec.name f.name);
          wr = (fun _ _ _ -> no_place codec.name f.name);
        }

  let resolve (type a r) (codec : r codec) (f : (a, r) Codec_repr.field) :
      (a, r) rfield =
    match codec.components with
    | Some c -> component codec c f
    | None ->
        let access = field_access ~op:"Codec.View.field" codec f in
        { rd = resolve_read codec f access; wr = resolve_write codec f access }

  let[@inline] read b r f = Codec_view.read f.rd (b : _ Buf.t :> bytes) r
  let[@inline] set b r f x = f.wr (b : _ Buf.t :> bytes) r x

  (* {2 Layers} *)

  (* A layer is data too: where its record starts in the record around it,
     and the frame its views read in. A field at a constant offset with no
     gate is [At] it; the walk of the record around it finds any other
     ([Walked], [-1] when it is absent); without a walk, [Locate] and the
     field's gate ([present]) evaluate it from the record's bytes. *)
  type place =
    | At of int
    | Walked of (Types.cursor -> int)
    | Locate of (runtime -> Input_end.t -> bytes -> int -> int)

  type sub = {
    place : place;
    present : (runtime -> Input_end.t -> bytes -> int -> bool) option;
    sub_frame : int;
  }

  type ('s, 'r) sub_layer = One of sub | Chain of sub list

  let[@inline never] parametric_layer codec name =
    Fmt.invalid_arg
      "Codec.View.layer: field %S of codec %S lies where an input parameter \
       places it"
      name codec

  (* The sub of the field [f] of [codec], which holds the records of [held],
     present where [gate] holds. *)
  let make_sub (type s r) (codec : r codec) (held : s codec)
      (f : (_, r) Codec_repr.field) gate =
    let op = "Codec.View.layer" in
    if held.n_params > 0 then
      Fmt.invalid_arg "%s: codec %S has input parameters" op held.name;
    let access = field_access ~op codec f in
    let c = compiled codec in
    let walk = if codec.n_params = 0 then c.walk else None in
    let place, present =
      match (access, gate, List.assoc_opt f.name c.places) with
      | (Fixed off | Variable { off; _ }), None, _ -> (At off, None)
      | _, _, Some at when Option.is_some walk -> (Walked at, None)
      | (Fixed off | Variable { off; _ }), present, _ -> (At off, present)
      | (Dynamic fn | Variable_dynamic { off_fn = fn; _ }), present, _ ->
          (Locate fn, present)
      | Bitfield _, _, _ ->
          Fmt.invalid_arg "%s: field %S does not hold a record" op f.name
    in
    (match (place, present) with
    | At _, None -> ()
    | _ -> if has_input_params codec then parametric_layer codec.name f.name);
    {
      place;
      present = Option.map (compile_bool_expr c.field_readers) present;
      sub_frame = frame_of (compiled held);
    }

  (* The gate of a field that holds a record: its own [Optional_or]'s. *)
  let rec gate : type s. s typ -> bool expr option = function
    | Optional_or { present; _ } -> Some present
    | Single_elem { elem; _ } -> gate elem
    | _ -> None

  let rec resolve_layer : type s r.
      r codec -> (s, r) layer -> s codec * sub list =
   fun codec -> function
    | Layer f -> (
        match held f.typ with
        | Some h -> (h, [ make_sub codec h f (gate f.typ) ])
        | None -> assert false)
    | Opt_layer ({ typ = Optional { present; inner }; _ } as f) -> (
        match held inner with
        | Some h -> (h, [ make_sub codec h f (Some present) ])
        | None -> assert false)
    | Opt_layer _ -> assert false
    | Under (l, l') ->
        let s, subs = resolve_layer codec l in
        let t, subs' = resolve_layer s l' in
        (t, subs @ subs')

  let layer codec l =
    match resolve_layer codec l with _, [ s ] -> One s | _, subs -> Chain subs

  (* The check of the enclosing record ran the layer's checks where it is
     present, so entering it tests only that its frame lies in the buffer: a
     view reads its fields there with no check of their own. A record whose
     bytes changed since may move the layer or raise in the reads that place
     it; it is then refused. *)
  let[@inline] walked at buf r =
    at
      {
        wbuf = buf;
        wbase = r;
        wpos = r;
        wend = Input_end.of_bytes buf;
        wgates = 0;
      }

  let[@inline never] enter_slow s buf r =
    let len = Bytes.length buf in
    let input_end = Input_end.of_bytes buf in
    match
      match (s.present, s.place) with
      | Some present, _ when not (present no_runtime input_end buf r) -> -1
      | _, At off -> r + off
      | _, Locate fn -> r + fn no_runtime input_end buf r
      | _, Walked at -> walked at buf r
    with
    | o -> if o >= 0 && o <= len - s.sub_frame then o else -1
    | exception Types.Unwalked -> -1
    | exception exn when Types.is_parse_error exn -> -1

  (* A layer the walk places, which has no gate of its own: one call with no
     dispatch. *)
  let[@inline never] enter_walked at buf r frame =
    match walked at buf r with
    | o -> if o >= 0 && o <= Bytes.length buf - frame then o else -1
    | exception Types.Unwalked -> -1
    | exception exn when Types.is_parse_error exn -> -1

  let[@inline] enter s buf r =
    match (s.place, s.present) with
    | At off, None ->
        let o = r + off in
        if o <= Bytes.length buf - s.sub_frame then o else -1
    | Walked at, None -> enter_walked at buf r s.sub_frame
    | _ -> enter_slow s buf r

  let rec enter_chain (subs : sub list) buf r =
    match subs with
    | [] -> r
    | s :: rest ->
        let o = enter s buf r in
        if o < 0 then -1 else enter_chain rest buf o

  let[@inline] sub_bytes l buf r =
    match l with One s -> enter s buf r | Chain subs -> enter_chain subs buf r

  let[@inline] sub b r l = sub_bytes l (b : _ Buf.t :> bytes) r

  (* A path reads its field in the record its layers hold, [None] where one
     is absent. *)
  let rec field : type a r. r codec -> (a, r) def -> (a, r) rfield =
   fun codec -> function
    | Def f -> resolve codec f
    | Path (l, d) ->
        let held, subs = resolve_layer codec l in
        let inner = field held d in
        let l = match subs with [ s ] -> One s | subs -> Chain subs in
        {
          rd =
            Get
              (fun buf r ->
                let o = sub_bytes l buf r in
                if o < 0 then None else Some (Codec_view.read inner.rd buf o));
          wr =
            (fun _ _ _ ->
              invalid_arg "Codec.View.set: a path has no constant place");
        }

  (* {2 Folds} *)

  (* The fields of a fold resolved, and handed to [f] in a saturated call for
     up to four of them. *)
  let rec apply : type f k r.
      r codec -> (f, k, r) Defs.t -> bytes -> int -> f -> k =
   fun codec defs ->
    match defs with
    | Defs.[] -> fun _ _ k -> k
    | Defs.[ a ] ->
        let a = (field codec a).rd in
        fun buf r k -> k (Codec_view.read a buf r)
    | Defs.[ a; b ] ->
        let a = (field codec a).rd and b = (field codec b).rd in
        fun buf r k -> k (Codec_view.read a buf r) (Codec_view.read b buf r)
    | Defs.[ a; b; c ] ->
        let a = (field codec a).rd
        and b = (field codec b).rd
        and c = (field codec c).rd in
        fun buf r k ->
          k (Codec_view.read a buf r) (Codec_view.read b buf r)
            (Codec_view.read c buf r)
    | Defs.[ a; b; c; d ] ->
        let a = (field codec a).rd
        and b = (field codec b).rd
        and c = (field codec c).rd
        and d = (field codec d).rd in
        fun buf r k ->
          k (Codec_view.read a buf r) (Codec_view.read b buf r)
            (Codec_view.read c buf r) (Codec_view.read d buf r)
    | Defs.(a :: rest) ->
        let a = (field codec a).rd and rest = apply codec rest in
        fun buf r k -> rest buf r (k (Codec_view.read a buf r))

  (* The error [validate] answers for the record at [off] in the bytes of its
     window that [buf] holds, placed in [buf]. *)
  let error env (t : _ codec) buf off len =
    let length = Bytes.length buf in
    let window =
      if off < 0 || off > length then Bytes.empty
      else Bytes.sub buf off (min (length - off) (max 0 len))
    in
    match validate ?env t window 0 with
    | Error e -> { e with at = e.at + off }
    | Ok () ->
        (* The bytes at [off] hold a record of no bytes: back to back, it is
           not followed by another. *)
        ({ at = off; field = []; kind = Trailing_bytes (length - off) }
          : Error.t)

  (* Where the record at [off] ends, when it passes its check in the bytes
     before [stop], [0 <= off <= stop <= Bytes.length buf]; [-1] otherwise. A
     codec with a walk checks and measures the record in one pass. *)
  (* Where the record [accepts] took ends, measured from its bytes. *)
  let measured_end env (c : _ compiled) input_end buf off stop =
    match c.wire_size with
    | Fixed n -> off + n
    | Variable { compute; _ } -> (
        match compute (runtime ?env ()) input_end buf off with
        | e -> if e >= off && e <= stop then e else -1
        | exception exn when Types.is_parse_error exn -> -1)

  let check_end env (t : _ codec) buf off stop =
    let c = compiled t in
    let input_end = Input_end.narrow (Input_end.of_bytes buf) stop in
    match c.walk with
    | _ when plain env t && c.fixed_frame >= 0 ->
        if off <= stop - c.fixed_frame && passes c.plain_pass input_end buf off
        then off + c.fixed_frame
        else -1
    | Some w when plain env t -> Codec_walk.end_of w buf off input_end
    | _ ->
        if accepts env t buf off stop then
          measured_end env c input_end buf off stop
        else -1

  (* The end of the window of [len] bytes at [off], or [-1] where [buf] does
     not hold it. *)
  let[@inline] window_end buf off len =
    if off < 0 || len < 0 || len > Bytes.length buf - off then -1 else off + len

  let fold ?env ?stride ?offs ?lens ?invalid (t : 'r codec) fields =
    let op = "Codec.View.fold" in
    require_env ~op t env;
    (match (stride, offs, lens, invalid) with
    | Some s, _, _, _ when s <= 0 ->
        Fmt.invalid_arg "%s: stride %d is not positive" op s
    | Some _, Some _, _, _ | Some _, _, Some _, _ ->
        Fmt.invalid_arg "%s: stride is given with offs" op
    | _, Some o, Some l, _ when Array.length o <> Array.length l ->
        Fmt.invalid_arg "%s: offs and lens differ in length" op
    | _, Some _, None, _ | _, None, Some _, _ ->
        Fmt.invalid_arg "%s: offs and lens are given together" op
    | None, None, None, Some _ ->
        Fmt.invalid_arg "%s: invalid is given with records back to back" op
    | _ -> ());
    let apply = apply t fields in
    fun buf ~init ~f ->
      let length = Bytes.length buf in
      match (stride, offs, lens) with
      | Some stride, _, _ ->
          let n = length / stride in
          let rec go acc i =
            if i = n then Ok acc
            else
              let off = i * stride in
              if check_end env t buf off (off + stride) >= 0 then
                go (apply buf off (f acc)) (i + 1)
              else
                match invalid with
                | Some invalid -> go (invalid acc) (i + 1)
                | None -> Error (error env t buf off stride, acc)
          in
          go init 0
      | None, Some offs, Some lens ->
          let n = Array.length offs in
          let rec go acc i =
            if i = n then Ok acc
            else
              let off = offs.(i) and len = lens.(i) in
              let stop = window_end buf off len in
              if stop >= 0 && check_end env t buf off stop >= 0 then
                go (apply buf off (f acc)) (i + 1)
              else
                match invalid with
                | Some invalid -> go (invalid acc) (i + 1)
                | None -> Error (error env t buf off len, acc)
          in
          go init 0
      | _ ->
          let rec go acc off =
            if off >= length then Ok acc
            else
              let e = check_end env t buf off length in
              if e <= off then Error (error env t buf off (length - off), acc)
              else go (apply buf off (f acc)) e
          in
          go init 0
end

let name (t : _ t) = t.name
let id (t : _ t) = t.id

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
      ( name,
        fun buf off ->
          match read no_runtime (Input_end.of_bytes buf) buf off with
          | v -> Ok v
          | exception exn when Types.is_parse_error exn ->
              let e = Types.parse_error_of exn in
              Error e ))
    (field_readers_ctx t)

let pp_value (t : _ t) ppf v =
  let ctx = Types.unbound_eval_ctx in
  let buf = Bytes.create (encoded_size_ctx t ctx v) in
  let _ : int = embed_encode_ctx t v ctx buf 0 in
  Fmt.pf ppf "@[<hv 2>%s {" t.name;
  List.iter
    (fun (name, reader) ->
      match reader buf 0 with
      | Ok n -> Fmt.pf ppf "@ %s = %d;" name n
      | Error e -> Fmt.pf ppf "@ %s = <%a>;" name Error.pp e)
    (field_readers t);
  Fmt.pf ppf "@ }@]"

(* -- Snapshot: batch bitfield access -- *)

(* The support generated code calls into: casts from an [int] it has read, and
   the cold paths, which hand the failing input to the codec itself so the error
   is the one [decode], [validate] or [encode] raises. *)
(* The field [name] of [c] of type [typ], found by its place rather than by
   its handle, or [None] when [c] has no field [name]. *)
let named_field (type a r) (c : r t) name (typ : a Types.typ) :
    (a, r) field option =
  match List.assoc_opt name (compiled c).field_access with
  | Some (id, _) ->
      Some
        {
          id;
          name;
          typ;
          constraint_ = None;
          action = None;
          doc = None;
          get =
            (fun _ ->
              Fmt.invalid_arg "%s.%s: a field found by place has no getter"
                c.name name);
        }
  | None -> None

module Gen_core = struct
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
    | Types.Byte_array_where { size; elt_var; cond; _ } ->
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

  type 'b field_k = {
    k : 'a. 'a Types.typ -> (bytes -> int -> ('a, Error.t) result) -> 'b;
  }

  (* The reader [Codec.get] makes for a field, found by its name. *)
  let with_field_reader t name { k } =
    List.find_map
      (fun (Types.Field f) ->
        if f.field_name <> Some name then None
        else
          let typ = f.field_typ in
          Option.map (fun field -> k typ (get t field)) (named_field t name typ))
      (to_struct t).fields

  (* A map's functions, typed through its inner type: the wire value is read
     as an [int] (or an [int32] for a 32-bit field), and [dec] decodes it as
     the codec does, the index bound of a lookup checked and a failure placed at
     [at]. *)
  type ('w, 'a) map = {
    dec : at:int -> 'w -> 'a;
    get : at:int -> field:string -> 'w -> ('a, Error.t) result;
    enc : 'a -> 'w;
  }

  let map_get decode ~at ~field x =
    match decode ~at x with
    | a -> Ok a
    | exception exn when Types.is_parse_error exn ->
        let e = Types.parse_error_of exn in
        Error { e with field = field :: e.field }

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
            let dec ~at x = decode ~at (v.of_raw x) in
            { dec; get = map_get dec; enc = (fun a -> v.to_raw (encode a)) }
        | None -> not_a_map ())
    | _ -> not_a_map ()

  let map_int32 : type a. a Types.typ -> (int32, a) map = function
    | Types.Map { inner; decode; encode; index_bound; _ } -> (
        match int32_view inner with
        | Some v ->
            let decode = Types.map_decode ~index_bound decode in
            let dec ~at x = decode ~at (v.of_raw32 x) in
            { dec; get = map_get dec; enc = (fun a -> v.to_raw32 (encode a)) }
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
        walk = None;
        places = [];
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

  let staged_intervals t buf off =
    match (compiled t).plain_pass with
    | Intervals { words = Some f; flat; tables; _ }
      when off >= 0 && off <= Bytes.length buf - min_size t ->
        let scan =
          Codec_intervals.scan_intervals flat tables buf off (Array.length flat)
            0
        in
        Some (f buf off, scan < 0)
    | _ -> None

  let all_paths : string list =
    [ "decode.fixed"; "decode.variable"; "decode.layout"; "decode.walk" ]
    @ List.concat_map
        (fun p ->
          [ p ^ ".none"; p ^ ".steps"; p ^ ".interval"; p ^ ".byte_table" ]
          (* The tests read words where an int holds one. *)
          @ if Sys.int_size >= 63 then [ p ^ ".words" ] else [])
        [ "pass"; "layout" ]
    @ List.init 4 (fun i -> Fmt.str "encode.staged.%d" (i + 1))
    @ ([ "encode.staged.rows"; "encode.plain"; "encode.blit"; "read.scalar" ]
        : string list)
end

module Gen = struct
  include Gen_core

  let scan c =
    let s = to_struct c in
    ignore (Scan_outputs.names s : (string * bool) list);
    fun buf off len ->
      if off < 0 || len < 0 || len > Bytes.length buf - off then None
      else
        let frame = Bytes.sub buf off len in
        match validate c frame 0 with
        | Ok () -> Some (Scan_outputs.values (Scan_outputs.read s frame 0))
        | Error _ -> None

  let frames ~name buf off len ~stride ~init f =
    if stride <= 0 || off < 0 || len < 0 || len > Bytes.length buf - off then
      invalid_arg (name ^ ": invalid frame bounds or stride");
    let rec go acc i =
      if i = len / stride then acc
      else go (f acc (off + (i * stride)) stride) (i + 1)
    in
    go init 0

  let index ~name ~offs ~lens ~init f =
    if Array.length offs <> Array.length lens then
      invalid_arg (name ^ ": offs and lens differ in length");
    let rec go acc i =
      if i = Array.length offs then acc else go (f acc offs.(i) lens.(i)) (i + 1)
    in
    go init 0
end
