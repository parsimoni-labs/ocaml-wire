type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

let pp ppf (p : (_, _) t) = Fmt.string ppf p.Types.name

(* Per-typ converter from the OCaml representation to [int] and back. One
   match dispatches both directions; [to_int] and [of_int] just project
   the relevant field. Avoids drift between the parallel cases. *)
type 'a int_cvt = { fwd : 'a -> int; bwd : int -> 'a }

let rec int_cvt : type a. a Types.typ -> a int_cvt =
 fun typ ->
  let id : 'a int_cvt = { fwd = (fun v -> v); bwd = (fun v -> v) } in
  match typ with
  | Uint8 -> id
  | Uint16 _ -> id
  | Uint_var _ -> { fwd = UInt63.to_int; bwd = UInt63.of_int }
  | Uint32 _ -> { fwd = UInt32.to_int; bwd = UInt32.of_int }
  | Uint63 _ -> { fwd = UInt63.to_int; bwd = UInt63.of_int }
  | Uint64 _ ->
      {
        fwd =
          (fun v -> Int64.unsigned_to_int v |> Option.value ~default:max_int);
        bwd = Int64.of_int;
      }
  | Int8 -> id
  | Int16 _ -> id
  | Int32 _ -> id
  | Int64 _ -> { fwd = Int64.to_int; bwd = Int64.of_int }
  | Float32 _ -> invalid_arg "Param: floats are not integer-representable"
  | Float64 _ -> invalid_arg "Param: floats are not integer-representable"
  | Bits _ -> id
  | Enum { base; _ } -> int_cvt base
  | Where { inner; _ } -> int_cvt inner
  | Single_elem { elem; _ } -> int_cvt elem
  | Map { inner; encode; decode; _ } ->
      let c = int_cvt inner in
      { fwd = (fun v -> c.fwd (encode v)); bwd = (fun v -> decode (c.bwd v)) }
  | Apply { typ; _ } -> int_cvt typ
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Struct _
  | Type_ref _ | Qualified_ref _ | Codec _ | Optional _ | Optional_or _
  | Repeat _ ->
      invalid_arg "Param: unsupported parameter type"

let to_int typ v = (int_cvt typ).fwd v
let of_int typ v = (int_cvt typ).bwd v

let rec is_int_representable : type a. a Types.typ -> bool = function
  | Types.Uint8 | Types.Uint16 _ | Types.Uint_var _ | Types.Uint32 _
  | Types.Uint63 _ | Types.Uint64 _ | Types.Int8 | Types.Int16 _ | Types.Int32 _
  | Types.Int64 _ | Types.Bits _ ->
      true
  | Types.Enum { base; _ } -> is_int_representable base
  | Types.Map { inner; _ } -> is_int_representable inner
  | Types.Where { inner; _ } -> is_int_representable inner
  | _ -> false

let check_typ name typ =
  if not (is_int_representable typ) then
    Fmt.invalid_arg "Param.%s: only integer-representable types are supported"
      name

(* A per-domain backing ref for a handle's [cell]. The handle is created once
   and shared across domains, so a plain [ref 0] would let concurrent
   encode/decode of the same parametric codec overwrite each other's value;
   [Domain.DLS] gives each domain its own ref, still reused within the domain. *)
let domain_local_cell () =
  let key = Domain.DLS.new_key (fun () -> ref 0) in
  fun () -> Domain.DLS.get key

let input name typ =
  check_typ "input" typ;
  {
    Types.name;
    typ;
    packed_typ = Types.Pack_typ typ;
    mutable_ = false;
    cell = domain_local_cell ();
  }

let output name typ =
  check_typ "output" typ;
  {
    Types.name;
    typ;
    packed_typ = Types.Pack_typ typ;
    mutable_ = true;
    cell = domain_local_cell ();
  }

let decl (t : ('a, 'k) t) : Types.param =
  { param_name = t.name; param_typ = t.packed_typ; mutable_ = t.mutable_ }

let name (t : (_, _) t) = t.Types.name
let expr t : int Types.expr = Types.Param_ref t

(* -- Param.env -- *)

type env = Types.param_env

(* Slot of a handle within an env, by name. [-1] when the env's codec does not
   reference the param (e.g. binding a param the codec does not use). *)
let env_idx (env : env) name =
  let rec find i =
    if i >= Array.length env.Types.names then -1
    else if env.names.(i) = name then i
    else find (i + 1)
  in
  find 0

let bind (p : ('a, input) t) (v : 'a) (env : env) : env =
  let iv = to_int p.Types.typ v in
  let slots = Array.copy env.slots in
  let bound = Array.copy env.bound in
  let i = env_idx env p.Types.name in
  if i >= 0 then begin
    slots.(i) <- iv;
    bound.(i) <- true
  end;
  p.cell () := iv;
  { env with Types.slots; bound }

let bind_by_name name (iv : int) (env : env) : env =
  let i = env_idx env name in
  if i < 0 then env
  else begin
    let slots = Array.copy env.Types.slots in
    let bound = Array.copy env.bound in
    slots.(i) <- iv;
    bound.(i) <- true;
    { env with Types.slots; bound }
  end

let get (env : env) (p : ('a, 'k) t) : 'a =
  let i = env_idx env p.Types.name in
  if i < 0 then of_int p.typ !(p.cell ()) else of_int p.typ env.slots.(i)

type packed = Pack : ('a, 'k) t -> packed
