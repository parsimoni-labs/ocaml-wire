type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

let rec to_int : type a. a Types.typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ -> Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Bits _ -> v
  | Enum { base; _ } -> to_int base v
  | Where { inner; _ } -> to_int inner v
  | Single_elem { elem; _ } -> to_int elem v
  | Map { inner; encode; _ } -> to_int inner (encode v)
  | Apply { typ; _ } -> to_int typ v
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      invalid_arg "Param: unsupported parameter type"

let rec of_int : type a. a Types.typ -> int -> a =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.of_int v
  | Uint63 _ -> UInt63.of_int v
  | Uint64 _ -> Int64.of_int v
  | Bits _ -> v
  | Enum { base; _ } -> of_int base v
  | Where { inner; _ } -> of_int inner v
  | Single_elem { elem; _ } -> of_int elem v
  | Map { inner; decode; _ } -> decode (of_int inner v)
  | Apply { typ; _ } -> of_int typ v
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      invalid_arg "Param: unsupported parameter type"

let input name typ =
  {
    Types.ph_name = name;
    ph_typ = typ;
    ph_packed_typ = Types.Pack_typ typ;
    ph_mutable = false;
    ph_cell = ref 0;
  }

let output name typ =
  {
    Types.ph_name = name;
    ph_typ = typ;
    ph_packed_typ = Types.Pack_typ typ;
    ph_mutable = true;
    ph_cell = ref 0;
  }

let v (t : ('a, 'k) t) : Types.param =
  {
    param_name = t.ph_name;
    param_typ = t.ph_packed_typ;
    mutable_ = t.ph_mutable;
  }

let name t = t.Types.ph_name
let get t = of_int t.Types.ph_typ !(t.Types.ph_cell)
let set t v = t.Types.ph_cell := to_int t.Types.ph_typ v

let init (t : ('a, input) t) (v : 'a) : int Types.expr =
  set t v;
  Types.Param_ref t

let expr t : int Types.expr = Types.Param_ref t

type packed = Pack : ('a, 'k) t -> packed
