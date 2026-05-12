type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

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
  | Uint_var _ -> id
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
  | Map { inner; encode; decode } ->
      let c = int_cvt inner in
      { fwd = (fun v -> c.fwd (encode v)); bwd = (fun v -> decode (c.bwd v)) }
  | Apply { typ; _ } -> int_cvt typ
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_array_where _
  | Byte_slice _ | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _
  | Codec _ | Optional _ | Optional_or _ | Repeat _ ->
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
