type input
type output
type ('a, 'k) t = { id : int; spec : Types.param; typ : 'a Types.typ }
type packed = Pack : ('a, 'k) t * int ref -> packed
type env = packed list

let next_id =
  let r = ref 0 in
  fun () ->
    let id = !r in
    incr r;
    id

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

let input name typ = { id = next_id (); spec = Types.param name typ; typ }

let output name typ =
  { id = next_id (); spec = Types.mutable_param name typ; typ }

let v t = t.spec
let name t = t.spec.param_name
let same_handle t (Pack (u, _)) = t.id = u.id
let same_name name (Pack (u, _)) = String.equal name u.spec.param_name
let empty = []
let is_empty env = env = []

let add env t v =
  let env = List.filter (fun p -> not (same_handle t p)) env in
  if List.exists (same_name t.spec.param_name) env then
    invalid_arg ("Param.bind: duplicate parameter name " ^ t.spec.param_name)
  else Pack (t, ref (to_int t.typ v)) :: env

let bind env t v = add env t v
let init env t v = add env t v

let get env t =
  let rec loop = function
    | [] -> invalid_arg ("Param.get: unbound parameter " ^ t.spec.param_name)
    | Pack (u, cell) :: rest ->
        if t.id = u.id then of_int t.typ !cell else loop rest
  in
  loop env

let to_ctx env =
  List.rev_map (fun (Pack (t, cell)) -> (t.spec.param_name, !cell)) env

let store_name env name v =
  List.iter
    (function
      | Pack (t, cell) when String.equal t.spec.param_name name -> cell := v
      | _ -> ())
    env
