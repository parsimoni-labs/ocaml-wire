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

let ( $ ) = ( $ )
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

(* The fields a [writer] takes, in the order its arguments come. Declared
   before [fields], so a list literal with no type to go by stays a [fields]. *)
type ('f, 'r) args =
  | [] : (unit, 'r) args
  | ( :: ) : ('a, 'r) field * ('f, 'r) args -> ('a -> 'f, 'r) args

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

let decoder t =
  let c = compiled t in
  let decode =
    match c.plain_decode with
    | Some decode -> decode
    | None -> fun buf off -> decode_slow `Prefix None t c buf off
  in
  Staged.stage (fun buf off ->
      match decode buf off with
      | v -> Ok v
      | exception exn when Types.is_parse_error exn ->
          let e = Types.parse_error_of exn in
          Error e)

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

let encoder t =
  let c = compiled t in
  match c.plain_encode with
  | Some f when t.n_params = 0 -> Staged.stage f
  | _ -> Staged.stage (fun v buf off -> encode_slow None t v buf off)

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
  let buf = Bytes.create (size_of_value ?env t v) in
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
         [Domain.DLS] key here: [get] returns a staged reader, but a caller that
         re-stages instead of reusing would leak a key per call. [decode_scratch]
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
  Staged.stage (fun buf off ->
      if off < 0 then Errors.negative_offset off
      else
        match run buf off with
        | v -> Ok v
        | exception exn when Types.is_parse_error exn ->
            let e = Types.parse_error_of exn in
            field_error name e)

let[@inline] get (type a r) ?env (codec : r t) (f : (a, r) field) :
    (bytes -> int -> (a, parse_error) result) Staged.t =
  (* A field's offset can be driven by an input param, and an unbound one reads
     0, which stages a reader onto the bytes in front of the field rather than
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
      | Some get, _, _ -> Staged.stage get
      | None, Some read, Some (off, width) ->
          Staged.stage (fun buf base ->
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
      | _ ->
          Staged.stage (fun buf base ->
              if base < 0 then Errors.negative_offset base
              else
                match read rt (Input_end.of_bytes buf) buf base with
                | v -> Ok v
                | exception exn when Types.is_parse_error exn ->
                    let e = Types.parse_error_of exn in
                    field_error name e))
  | Some act -> action_reader ?env codec c rt access act read name

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

(* {1 Writers} *)

open Codec_run

let slot_of : type a r. n:int -> r t -> (a, r) field -> a slot option =
 fun ~n codec f ->
  let s eq ~at ~nb ~sh ~w ~bits = Some (slot eq ~n ~at ~nb ~s:sh ~w ~bits) in
  match (field_access ~op:"Codec.writer" codec f, f.typ) with
  | Bitfield { base = U8; byte_off; shift; width }, Bits _ ->
      s Type.Equal ~at:byte_off ~nb:1 ~sh:shift ~w:width ~bits:true
  | Bitfield { base = U16 Big; byte_off; shift; width }, Bits _ ->
      s Type.Equal ~at:byte_off ~nb:2 ~sh:shift ~w:width ~bits:true
  | Bitfield { base = U32 Big; byte_off; shift; width }, Bits _ ->
      s Type.Equal ~at:byte_off ~nb:4 ~sh:shift ~w:width ~bits:true
  | Fixed at, Uint8 -> s UInt8.int_eq ~at ~nb:1 ~sh:0 ~w:8 ~bits:false
  | Fixed at, Int8 -> s SInt8.int_eq ~at ~nb:1 ~sh:0 ~w:8 ~bits:false
  | Fixed at, Uint16 Big -> s UInt16.int_eq ~at ~nb:2 ~sh:0 ~w:16 ~bits:false
  | Fixed at, Int16 Big -> s SInt16.int_eq ~at ~nb:2 ~sh:0 ~w:16 ~bits:false
  | Fixed at, Uint32 Big -> (
      match UInt32.int_eq with
      | Some eq -> s eq ~at ~nb:4 ~sh:0 ~w:32 ~bits:false
      | None -> None)
  | Fixed at, Int32 Big -> (
      match SInt32.int_eq with
      | Some eq -> s eq ~at ~nb:4 ~sh:0 ~w:32 ~bits:false
      | None -> None)
  | _ -> None

(* Each field of [args] once. *)
let rec wfields : type f r.
    fast:bool -> n:int -> r t -> (f, r) args -> int list -> (f, r) wlist =
 fun ~fast ~n codec args seen ->
  match args with
  | [] -> WNil
  | f :: rest ->
      if List.mem f.id seen then
        Fmt.invalid_arg "Codec.writer: field %S is written twice" f.name;
      let w =
        match if fast then slot_of ~n codec f else None with
        | Some s -> Slot s
        | None -> Setter (Staged.unstage (set codec f))
      in
      WCons (w, wfields ~fast ~n codec rest (f.id :: seen))

let writer (type f r) (codec : r t) (template : r) (args : (f, r) args) :
    (bytes -> int -> f) Staged.t =
  let n =
    match (compiled codec).wire_size with
    | Fixed n -> n
    | Variable _ ->
        Fmt.invalid_arg "Codec.writer: codec %S is not of a fixed size"
          codec.name
  in
  require_env ~op:"writer" codec None;
  let b = Bytes.create n in
  encode codec template b 0;
  let fast = n <= 7 && Sys.int_size >= 63 in
  let ws = wfields ~fast ~n codec args [] in
  if fast && all_slots ws then
    let t = clear_slots ws (run_of_bytes b n) in
    Staged.stage (run_writer ~name:codec.name ~n t ws)
  else
    let ws =
      if all_slots ws then wfields ~fast:false ~n codec args [] else ws
    in
    Staged.stage (set_writer ~name:codec.name ~n b ws)

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

let[@inline never] accepts_slow env (t : 'r t) buf off =
  off >= 0
  &&
  match validate_exn ?env t buf off with
  | () -> true
  | exception exn when Types.is_parse_error exn -> false

(* Whether the record at [off] passes [validate], without its error. Small
   enough to inline: the frame and the tests of a fixed-size codec, every
   other codec out of line. *)
let[@inline] accepts ?env (t : 'r t) buf off =
  match env with
  | None when t.n_params = 0 -> (
      match Atomic.get t.compiled with
      | Some c when c.fixed_frame >= 0 ->
          let input_end = Input_end.of_bytes buf in
          off >= 0
          && off <= Input_end.to_int input_end - c.fixed_frame
          && passes c.plain_pass input_end buf off
      | _ -> accepts_slow env t buf off)
  | _ -> accepts_slow env t buf off

module View = struct
  type 'r record_codec = 'r t
  type ('a, 'r) record_field = ('a, 'r) field

  let codec_get = get

  (* A named codec holds what [accepts] decides on each call: [frame] is the
     fixed frame of a codec checked by its plain pass alone, or [-1] for one
     its walk or [validate] checks. Its name ['c] is a phantom. *)
  type 'r named = {
    codec : 'r record_codec;
    env : Param.env option;
    c : 'r compiled;
    frame : int;
    pass : pass;
    walk : 'r Types.walk option;
  }

  type ('c, 'r) codec = 'r named

  module type CODEC = sig
    type c
    type r

    val codec : (c, r) codec
  end

  let named ?env (codec : _ record_codec) =
    require_env ~op:"View.codec" codec env;
    let compiled = compiled codec in
    let walk =
      match env with None when codec.n_params = 0 -> compiled.walk | _ -> None
    in
    let frame =
      match (env, compiled.plain_pass, walk) with
      | _, Steps _, Some _ -> -1
      | None, _, _ when codec.n_params = 0 -> compiled.fixed_frame
      | _ -> -1
    in
    { codec; env; c = compiled; frame; pass = compiled.plain_pass; walk }

  let codec (type r) ?env (c : r record_codec) : (module CODEC with type r = r)
      =
    let v = named ?env c in
    (module struct
      type c
      type nonrec r = r

      let codec = v
    end)

  type ('b, 'c) check = int

  (* A codec with a walk makes its checks with the walk, building nothing; any
     other is checked by [validate]. *)
  let[@inline never] check_slow v buf off =
    let ok =
      match v.walk with
      | Some w ->
          off >= 0 && Codec_walk.accepts w buf off (Input_end.of_bytes buf)
      | None -> accepts_slow v.env v.codec buf off
    in
    if ok then off else -1

  let[@inline never] pass_slow pass buf off =
    passes pass (Input_end.of_bytes buf) buf off

  (* The frame and a pass of one word test inline, a pass of word tests one
     closure call, any other pass and any codec [validate] checks out of
     line. *)
  let[@inline] check v b len off =
    let buf = (b : _ Buf.t :> bytes) and len = (len : _ Buf.length :> int) in
    if v.frame < 0 then check_slow v buf off
    else if off < 0 || off > len - v.frame then -1
    else
      match v.pass with
      | No_checks -> off
      | Word { q; l; r; m; lo; hi; _ } ->
          if Codec_intervals.within buf off q l r m lo hi then off else -1
      | Intervals { words = Some f; _ } -> if f buf off then off else -1
      | pass -> if pass_slow pass buf off then off else -1

  let[@inline] ok k = k >= 0

  type ('b, 'c) t = int

  let[@inline never] refused () =
    invalid_arg "Codec.View.get: the check refused the record"

  let[@inline] get k = if k < 0 then refused () else k

  include Codec_view

  let frame_of (c : _ compiled) =
    match c.wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size

  (* The type a view reads a field as, under the [where] and [enum] the check
     made: a view reads what is there, as the module [Wire_ocaml] writes does. *)
  let rec read_as : type a. a typ -> a typ = function
    | Where { inner; _ } -> read_as inner
    | Enum { base; _ } -> read_as base
    | t -> t

  let[@inline never] needs_a_check (v : _ codec) (f : _ record_field) =
    Fmt.invalid_arg
      "Codec.View.field: field %S of codec %S is read with a check: use \
       Codec.View.checked_field"
      f.name v.codec.name

  let field (type a r) (v : (_, r) codec) (f : (a, r) record_field) :
      (a, _) field =
    let access = field_access ~op:"Codec.View.field" v.codec f in
    let size = frame_of v.c in
    let plain = not (List.mem_assoc f.name v.c.field_actions) in
    let typ = read_as f.typ in
    let scalar =
      if plain then field_scalar ~min_wire_size:size access typ else None
    in
    match (scalar, typ, access) with
    | Some s, _, _ -> (
        match Option.bind (bits_of_scalar s) (word_field ~size) with
        | Some f -> f
        | None -> Scalar s)
    | None, Bits _, Bitfield { base = U32 Big; byte_off = o; shift; width }
      when plain && o >= 0 && o <= size - 4 -> (
        match
          word_field ~size { o; n = 4; s = shift; w = width; eq = Equal }
        with
        | Some f -> f
        | None ->
            let read = build_bf_accessor_reader (U32 Big) o shift width in
            Get read)
    | None, Byte_slice { size = Int length }, Fixed off
      when plain && off >= 0 && length >= 0 && off <= size - length ->
        Bytes { off; length }
    | None, Byte_array { size = Int length }, Fixed off
      when plain && off >= 0 && length >= 0 && off <= size - length ->
        Get (fun buf r -> Bytes.sub_string buf (r + off) length)
    | None, _, _ -> needs_a_check v f

  let checked_field (type a r) (v : (_, r) codec) (f : (a, r) record_field) :
      ((a, Error.t) result, _) field =
    ignore
      (field_access ~op:"Codec.View.checked_field" v.codec f : field_access);
    Get (Staged.unstage (codec_get ?env:v.env v.codec f))

  (* A sub-codec field is data too: where its record starts in the record
     around it, and the frame its views read in. A field at a constant offset
     with no gate is [At] it; the walk of the record around it finds any other
     ([Walked], [-1] when it is absent); without a walk, [Locate] and the
     field's gate ([present]) evaluate it from the record's bytes. *)
  type place =
    | At of int
    | Walked of (Types.cursor -> int)
    | Locate of (runtime -> Input_end.t -> bytes -> int -> int)

  type ('cs, 'c) sub = {
    place : place;
    present : (runtime -> Input_end.t -> bytes -> int -> bool) option;
    rt : runtime;
    sub_frame : int;
  }

  (* The id of the codec a sub-codec field holds, and its presence gate. *)
  let rec sub_codec : type a. a typ -> (int * bool expr option) option =
    function
    | Codec { codec_id; _ } -> Some (codec_id, None)
    | Single_elem { elem; _ } -> sub_codec elem
    | Optional_or { present; inner; _ } ->
        Option.map (fun (id, _) -> (id, Some present)) (sub_codec inner)
    | _ -> None

  let make_sub ~op v (cv : _ codec) (f : _ record_field) typ =
    if cv.codec.n_params > 0 then
      Fmt.invalid_arg "%s: codec %S has input parameters" op cv.codec.name;
    let access = field_access ~op v.codec f in
    let id, present =
      match typ with
      | Some (id, present) -> (id, present)
      | None ->
          Fmt.invalid_arg "%s: field %S does not hold a sub-codec" op f.name
    in
    if id <> cv.codec.id then
      Fmt.invalid_arg "%s: field %S does not hold codec %S" op f.name
        cv.codec.name;
    let place, present =
      match (access, present, List.assoc_opt f.name v.c.places) with
      | (Fixed off | Variable { off; _ }), None, _ -> (At off, None)
      | _, _, Some at when Option.is_some v.walk -> (Walked at, None)
      | (Fixed off | Variable { off; _ }), present, _ -> (At off, present)
      | (Dynamic fn | Variable_dynamic { off_fn = fn; _ }), present, _ ->
          (Locate fn, present)
      | Bitfield _, _, _ ->
          Fmt.invalid_arg "%s: field %S does not hold a sub-codec" op f.name
    in
    {
      place;
      present = Option.map (compile_bool_expr v.c.field_readers) present;
      rt = runtime ?env:v.env ();
      sub_frame = frame_of cv.c;
    }

  let sub v cv (f : _ record_field) =
    make_sub ~op:"Codec.View.sub" v cv f (sub_codec f.typ)

  let sub_option v cv (f : (_ option, _) record_field) =
    let typ =
      match f.typ with
      | Optional { present; inner } ->
          Option.map (fun (id, _) -> (id, Some present)) (sub_codec inner)
      | _ -> None
    in
    make_sub ~op:"Codec.View.sub_option" v cv f typ

  (* The check of the enclosing record ran the sub-record's checks where it is
     present, so entering it tests only that its frame lies in the buffer: a
     view reads its fields there with no check of their own. A record whose
     bytes changed since may move the sub-record or raise in the reads that
     place it; it is then refused. *)
  let[@inline] walked at buf r =
    at
      {
        wbuf = buf;
        wbase = r;
        wpos = r;
        wend = Input_end.of_bytes buf;
        wgates = 0;
      }

  let[@inline never] enter_slow s buf len r =
    let input_end = Input_end.of_bytes buf in
    match
      match (s.present, s.place) with
      | Some present, _ when not (present s.rt input_end buf r) -> -1
      | _, At off -> r + off
      | _, Locate fn -> r + fn s.rt input_end buf r
      | _, Walked at -> walked at buf r
    with
    | o -> if o >= 0 && o <= len - s.sub_frame then o else -1
    | exception Types.Unwalked -> -1
    | exception exn when Types.is_parse_error exn -> -1

  (* A sub-record the walk places, which has no gate of its own: one call
     with no dispatch. *)
  let[@inline never] enter_walked at buf len r frame =
    match walked at buf r with
    | o -> if o >= 0 && o <= len - frame then o else -1
    | exception Types.Unwalked -> -1
    | exception exn when Types.is_parse_error exn -> -1

  let[@inline] enter s b len r =
    let buf = (b : _ Buf.t :> bytes) and len = (len : _ Buf.length :> int) in
    match (s.place, s.present) with
    | At off, None ->
        let o = r + off in
        if o <= len - s.sub_frame then o else -1
    | Walked at, None -> enter_walked at buf len r s.sub_frame
    | _ -> enter_slow s buf len r
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

let field_ref (type a r) (f : (a, r) field) : int expr = Ref (I, f.name)

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

(* -- Record words -- *)

type 'r word = 'r Codec_run.word
type ('a, 'r) part = ('a, 'r) Codec_run.part

let word_size ~op (codec : _ t) =
  match (compiled codec).wire_size with
  | Fixed n when 8 * n < Sys.int_size -> n
  | Fixed n ->
      Fmt.invalid_arg "%s: codec %S has %d bytes, more than an int holds" op
        codec.name n
  | Variable _ ->
      Fmt.invalid_arg "%s: codec %S is not of a fixed size" op codec.name

type 'r word_reader = 'r Codec_run.reader

let word_reader (codec : 'r t) : 'r word_reader =
  Codec_run.reader (word_size ~op:"Codec.word_reader" codec)

let read_word = Codec_run.read_word

let word_part (type a r) (codec : r t) (f : (a, r) field) : (a, r) part =
  let n = word_size ~op:"Codec.word_part" codec in
  match slot_of ~n codec f with
  | None ->
      Fmt.invalid_arg
        "Codec.word_part: field %S of codec %S is not a big-endian integer or \
         a bitfield of a byte or a big-endian 16- or 32-bit word"
        f.name codec.name
  | Some s ->
      let signed =
        match f.typ with Int8 | Int16 _ | Int32 _ -> true | _ -> false
      in
      Codec_run.part s.eq ~pos:s.pos ~width:s.width ~signed

let of_word = Codec_run.of_word

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

  (* The reader [Codec.get] stages for a field, found by its name. *)
  let with_field_reader t name { k } =
    List.find_map
      (fun (Types.Field f) ->
        if f.field_name <> Some name then None
        else
          let typ = f.field_typ in
          Option.map
            (fun field -> k typ (Staged.unstage (get t field)))
            (named_field t name typ))
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
      when off >= 0 && off <= Bytes.length buf - min_wire_size t ->
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

type _ field_kind =
  | Int : int field_kind
  | UInt8 : UInt8.t field_kind
  | UInt16 : UInt16.t field_kind
  | UInt32 : UInt32.t field_kind
  | SInt8 : SInt8.t field_kind
  | SInt16 : SInt16.t field_kind
  | SInt32 : SInt32.t field_kind
  | String : string field_kind
  | Slice : Bytesrw.Bytes.Slice.t field_kind

let rec typ_eq : type a b. a Types.typ -> b field_kind -> (a, b) Type.eq option
    =
 fun t k ->
  match (t, k) with
  | Types.Bits _, Int -> Some Type.Equal
  | Types.Uint8, UInt8 -> Some Type.Equal
  | Types.Uint16 _, UInt16 -> Some Type.Equal
  | Types.Uint32 _, UInt32 -> Some Type.Equal
  | Types.Int8, SInt8 -> Some Type.Equal
  | Types.Int16 _, SInt16 -> Some Type.Equal
  | Types.Int32 _, SInt32 -> Some Type.Equal
  | Types.Byte_array _, String -> Some Type.Equal
  | Types.Byte_slice _, Slice -> Some Type.Equal
  | Types.Single_elem { elem; _ }, k -> typ_eq elem k
  | Types.Where { inner; _ }, k -> typ_eq inner k
  | Types.Enum { base; _ }, k -> typ_eq base k
  | _ -> None

module Gen = struct
  include Gen_core

  type 'a kind = 'a field_kind =
    | Int : int kind
    | UInt8 : UInt8.t kind
    | UInt16 : UInt16.t kind
    | UInt32 : UInt32.t kind
    | SInt8 : SInt8.t kind
    | SInt16 : SInt16.t kind
    | SInt32 : SInt32.t kind
    | String : string kind
    | Slice : Bytesrw.Bytes.Slice.t kind

  module Outputs = Scan_outputs

  (* {2 The interpreted twin} *)

  type record = Record : 'r View.named -> record

  let root c = Record (View.named c)

  (* The field [i] of the record [v], as a field the views read. *)
  let nth (type r) (v : r View.named) i =
    match List.nth_opt (to_struct v.codec).fields i with
    | Some (Types.Field { field_name = Some name; field_typ; _ }) ->
        (name, Types.Pack_typ field_typ)
    | Some _ | None -> Fmt.invalid_arg "%s: no named field at %d" v.codec.name i

  let record_field (type a r) (c : r t) name (typ : a Types.typ) : (a, r) field
      =
    match named_field c name typ with
    | Some f -> f
    | None -> Fmt.invalid_arg "%s: no field %s" c.name name

  let zero : type a. a kind -> a = function
    | Int -> 0
    | UInt8 -> UInt8.zero
    | UInt16 -> UInt16.zero
    | UInt32 -> UInt32.zero
    | SInt8 -> SInt8.zero
    | SInt16 -> SInt16.zero
    | SInt32 -> SInt32.zero
    | String -> ""
    | Slice -> Bytesrw.Bytes.Slice.eod

  let other_type c name =
    Fmt.invalid_arg "%s: field %s has another type" c.name name

  (* A field's read is a field of the views: one the views read with no
     check, or [Get] of a read of any other. *)
  type 'a reader = ('a, unit) View.field

  let[@inline] read (h : _ reader) b r = View.read h b r

  (* An integer output as a value of the kind [k]. *)
  let of_output : type a. a kind -> int64 -> a =
   fun k x ->
    let i = Int64.to_int x in
    let cast : type t. (t, int) Type.eq -> t = fun Type.Equal -> i in
    match k with
    | Int -> i
    | UInt8 -> cast UInt8.int_eq
    | UInt16 -> cast UInt16.int_eq
    | SInt8 -> cast SInt8.int_eq
    | SInt16 -> cast SInt16.int_eq
    | UInt32 -> UInt32.of_int i
    | SInt32 -> SInt32.of_int i
    | String | Slice -> invalid_arg "Gen: bytes are no output"

  (* A field the views read with no reader of their own, such as an optional
     field behind a gate the record's bytes decide, is read with the record's
     outputs, which name it by its label. *)
  let output (v : _ View.named) name f : _ reader =
    let s = to_struct v.codec and label = Names.ocaml_label name in
    View.Get (fun buf r -> f (Scan_outputs.read s buf r) label)

  (* A field a view reads with no check is one direct read; any other is read
     with its check, answering what [fail] makes of the error, as the module
     [Wire_ocaml] writes answers a field the bytes written after the check
     move out of the buffer. *)
  let view_field (v : _ View.named) name typ ~fail : _ reader option =
    let f = record_field v.codec name typ in
    match View.field v f with
    | f -> Some f
    | exception Invalid_argument _ -> (
        match View.checked_field v f with
        | f ->
            Some
              (View.Get
                 (fun buf r ->
                   match View.read_other f buf r with
                   | Ok x -> x
                   | Error _ -> fail))
        | exception Invalid_argument _ -> None)

  let field_at (type a) (Record v) i (k : a kind) : a reader =
    let name, Types.Pack_typ typ = nth v i in
    let by_output () =
      output v name (fun o l -> of_output k (Scan_outputs.int o l))
    in
    let optional f =
      View.Get
        (fun buf r -> Option.value ~default:(zero k) (View.read_other f buf r))
    in
    match typ with
    | Types.Optional { inner; _ } -> (
        match typ_eq inner k with
        | Some Type.Equal ->
            Option.fold ~none:(by_output ()) ~some:optional
              (view_field v name typ ~fail:None)
        | None -> other_type v.codec name)
    | Types.Optional_or { inner; _ } -> (
        match typ_eq inner k with
        | Some Type.Equal ->
            Option.value ~default:(by_output ())
              (view_field v name typ ~fail:(zero k))
        | None -> other_type v.codec name)
    | _ -> (
        match typ_eq typ k with
        | Some Type.Equal ->
            Option.value ~default:(by_output ())
              (view_field v name typ ~fail:(zero k))
        | None -> other_type v.codec name)

  let present_at (Record v) i =
    let name, Types.Pack_typ typ = nth v i in
    match typ with
    | Types.Optional _ -> (
        match view_field v name typ ~fail:None with
        | Some h ->
            View.Get (fun buf r -> Option.is_some (View.read_other h buf r))
        | None ->
            output v name (fun o l -> Scan_outputs.bool o (l ^ "_present")))
    | _ -> Fmt.invalid_arg "%s: field %s is not optional" v.codec.name name

  type 'a writer = { write : 'b. 'b Wire_runtime.Buf.t -> int -> 'a -> unit }

  let set_at (type a) (Record v) i (k : a kind) : a writer =
    let name, Types.Pack_typ typ = nth v i in
    match typ_eq typ k with
    | Some Type.Equal ->
        let write =
          Staged.unstage (set v.codec (record_field v.codec name typ))
        in
        { write = (fun b r x -> write (b : _ Wire_runtime.Buf.t :> bytes) r x) }
    | None -> other_type v.codec name

  (* The record a field of type [typ] holds, and the field as a sub-record. *)
  let held : type a r.
      r View.named ->
      (a, r) field ->
      a Types.typ ->
      (record * (unit, unit) View.sub) option =
   fun v f -> function
    | Types.Codec { codec_ref = Codec_ref c; _ } ->
        let w = View.named c in
        Some (Record w, View.sub v w f)
    | Types.Single_elem { elem = Types.Codec { codec_ref = Codec_ref c; _ }; _ }
      ->
        let w = View.named c in
        Some (Record w, View.sub v w f)
    | Types.Optional_or
        { inner = Types.Codec { codec_ref = Codec_ref c; _ }; _ } ->
        let w = View.named c in
        Some (Record w, View.sub v w f)
    | Types.Optional { inner = Types.Codec { codec_ref = Codec_ref c; _ }; _ }
      ->
        let w = View.named c in
        Some (Record w, View.sub_option v w f)
    | Types.Optional
        {
          inner =
            Types.Single_elem
              { elem = Types.Codec { codec_ref = Codec_ref c; _ }; _ };
          _;
        } ->
        let w = View.named c in
        Some (Record w, View.sub_option v w f)
    | _ -> None

  let held_at (Record v) i =
    let name, Types.Pack_typ typ = nth v i in
    match held v (record_field v.codec name typ) typ with
    | Some h -> h
    | None -> Fmt.invalid_arg "%s: field %s holds no record" v.codec.name name

  let sub_at r i = fst (held_at r i)
  let enter_at r i = snd (held_at r i)

  let[@inline] enter (s : (unit, unit) View.sub) b len r : int =
    View.enter s b len r

  let[@inline] check_at (Record v) b length off : int =
    View.check v b length off

  let check_window (Record v) (b : _ Wire_runtime.Buf.t) length ~off ~len : int
      =
    let length = (length : _ Wire_runtime.Buf.length :> int) in
    if off < 0 || len < 0 || off > length - len then -1
    else if
      accepts v.codec (Bytes.sub (b : _ Wire_runtime.Buf.t :> bytes) off len) 0
    then off
    else -1

  let scan c =
    let s = to_struct c in
    ignore (Scan_outputs.names s : (string * bool) list);
    fun buf off len ->
      if off < 0 || len < 0 || len > Bytes.length buf - off then None
      else
        let frame = Bytes.sub buf off len in
        match validate c frame 0 with
        | Ok () -> Some (Scan_outputs.read s frame 0)
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
