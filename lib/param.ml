type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

let rec to_int : type a. a Types.typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint_var _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ -> Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Int8 -> v
  | Int16 _ -> v
  | Int32 _ -> v
  | Int64 _ -> Int64.to_int v
  | Float32 _ -> invalid_arg "Param: floats are not integer-representable"
  | Float64 _ -> invalid_arg "Param: floats are not integer-representable"
  | Bits _ -> v
  | Enum { base; _ } -> to_int base v
  | Where { inner; _ } -> to_int inner v
  | Single_elem { elem; _ } -> to_int elem v
  | Map { inner; encode; _ } -> to_int inner (encode v)
  | Apply { typ; _ } -> to_int typ v
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_array_where _
  | Byte_slice _ | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _
  | Codec _ | Optional _ | Optional_or _ | Repeat _ ->
      invalid_arg "Param: unsupported parameter type"

let rec of_int : type a. a Types.typ -> int -> a =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint_var _ -> v
  | Uint32 _ -> UInt32.of_int v
  | Uint63 _ -> UInt63.of_int v
  | Uint64 _ -> Int64.of_int v
  | Int8 -> v
  | Int16 _ -> v
  | Int32 _ -> v
  | Int64 _ -> Int64.of_int v
  | Float32 _ -> invalid_arg "Param: floats are not integer-representable"
  | Float64 _ -> invalid_arg "Param: floats are not integer-representable"
  | Bits _ -> v
  | Enum { base; _ } -> of_int base v
  | Where { inner; _ } -> of_int inner v
  | Single_elem { elem; _ } -> of_int elem v
  | Map { inner; decode; _ } -> decode (of_int inner v)
  | Apply { typ; _ } -> of_int typ v
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_array_where _
  | Byte_slice _ | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _
  | Codec _ | Optional _ | Optional_or _ | Repeat _ ->
      invalid_arg "Param: unsupported parameter type"

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

let input name typ =
  check_typ "input" typ;
  {
    Types.ph_name = name;
    ph_typ = typ;
    ph_packed_typ = Types.Pack_typ typ;
    ph_mutable = false;
    ph_cell = ref 0;
    ph_slot = -1;
    ph_env_idx = -1;
  }

let output name typ =
  check_typ "output" typ;
  {
    Types.ph_name = name;
    ph_typ = typ;
    ph_packed_typ = Types.Pack_typ typ;
    ph_mutable = true;
    ph_cell = ref 0;
    ph_slot = -1;
    ph_env_idx = -1;
  }

let decl (t : ('a, 'k) t) : Types.param =
  {
    param_name = t.ph_name;
    param_typ = t.ph_packed_typ;
    mutable_ = t.ph_mutable;
  }

let name t = t.Types.ph_name
let expr t : int Types.expr = Types.Param_ref t

(* -- Param.env -- *)

type env = Types.param_env

let bind (p : ('a, input) t) (v : 'a) (env : env) : env =
  let iv = to_int p.Types.ph_typ v in
  let slots = Array.copy env.pe_slots in
  if p.ph_env_idx >= 0 then slots.(p.ph_env_idx) <- iv;
  p.ph_cell := iv;
  { Types.pe_codec_id = env.pe_codec_id; pe_slots = slots }

let get (env : env) (p : ('a, 'k) t) : 'a =
  if p.Types.ph_env_idx < 0 then of_int p.ph_typ !(p.ph_cell)
  else of_int p.ph_typ env.pe_slots.(p.ph_env_idx)

type packed = Pack : ('a, 'k) t -> packed
