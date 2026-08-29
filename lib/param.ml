type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

let pp ppf (p : (_, _) t) = Fmt.string ppf p.Types.name

(* Reading a bound value as the [int] an environment slot holds. The other
   direction is [Types.of_int], shared with every other integer view of a type;
   this one stays separate because a parameter that does not fit the native int
   is a caller error [bind] reports as [Invalid_argument], not the malformed
   input the decode-path conversion raises a parse error on. *)
exception Unfittable_native_int

let id_counter = Atomic.make 0

let optint_to_int to_int value =
  match to_int value with
  | value -> value
  | exception (Failure _ | Invalid_argument _) -> raise Unfittable_native_int

let rec to_int : type a. a Types.typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> UInt8.to_int v
  | Uint16 _ -> UInt16.to_int v
  | Uint_var _ -> optint_to_int UInt63.to_int v
  | Uint32 _ -> optint_to_int UInt32.to_int v
  | Uint64 _ -> UInt64.to_int_opt v |> Option.value ~default:max_int
  | Int8 -> SInt8.to_int v
  | Int16 _ -> SInt16.to_int v
  | Int32 _ -> SInt32.to_int v
  | Int64 _ -> Int64.to_int v
  | Float32 _ -> invalid_arg "Param: floats are not integer-representable"
  | Float64 _ -> invalid_arg "Param: floats are not integer-representable"
  | Bits _ -> v
  | Enum { base; _ } -> to_int base v
  | Where { inner; _ } -> to_int inner v
  | Single_elem { elem; _ } -> to_int elem v
  | Map { inner; encode; _ } -> to_int inner (encode v)
  | Apply { typ; _ } -> to_int typ v
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Struct _
  | Type_ref _ | Qualified_ref _ | Codec _ | Optional _ | Optional_or _
  | Repeat _ ->
      invalid_arg "Param: unsupported parameter type"

let of_int = Types.of_int

(* A [uint ~size] renders as [UINTBE(n)], which is an array suffix rather than a
   type, so it cannot name a 3D formal. Rejected here for the same reason
   [Wire.casetype] rejects it as a tag: the position takes less than a field
   does. *)
let rec is_uint_var : type a. a Types.typ -> bool = function
  | Types.Uint_var _ -> true
  | Types.Enum { base; _ } -> is_uint_var base
  | Types.Map { inner; _ } -> is_uint_var inner
  | Types.Where { inner; _ } -> is_uint_var inner
  | _ -> false

let check_typ name typ =
  if not (Types.is_int_representable typ) then
    Fmt.invalid_arg "Param.%s: only integer-representable types are supported"
      name;
  if is_uint_var typ then
    Fmt.invalid_arg
      "Param.%s: a [uint ~size] has no 3D parameter type; use a fixed-width \
       integer"
      name

let input name typ =
  check_typ "input" typ;
  let id = Atomic.fetch_and_add id_counter 1 in
  { Types.id; name; typ; packed_typ = Types.Pack_typ typ; mutable_ = false }

let output name typ =
  check_typ "output" typ;
  let id = Atomic.fetch_and_add id_counter 1 in
  { Types.id; name; typ; packed_typ = Types.Pack_typ typ; mutable_ = true }

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
  let iv =
    match to_int p.Types.typ v with
    | value -> value
    | exception Unfittable_native_int ->
        Fmt.invalid_arg
          "Param.bind %S: value does not fit this platform's native int"
          p.Types.name
  in
  let slots = Array.copy env.slots in
  let bound = Array.copy env.bound in
  let i = env_idx env p.Types.name in
  if i >= 0 then begin
    slots.(i) <- iv;
    bound.(i) <- true
  end;
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
  if i < 0 then
    Fmt.invalid_arg
      "Param.get: parameter %S does not belong to this environment" p.Types.name
  else of_int p.typ env.slots.(i)

type packed = Pack : ('a, 'k) t -> packed
