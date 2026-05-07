(* Top-level expression evaluator and value-to-int conversion.

   The full struct-internal expression machinery (with [Ref]/[Sizeof_this]/
   [Field_pos] resolution against bound fields) lives in [Codec] as the
   [compile_int_arr] family, which compiles expressions to [int array]
   accessors at codec construction. This module is the residual evaluator
   for the [Wire.of_string]/[Wire.encode] paths, which only ever
   evaluate expressions in [empty]: no field references, no cross-field
   dependencies. *)

open Types

type ctx = unit

let empty : ctx = ()

(* Convert a typed value to [int]. Returns [None] for types that don't
   fit in OCaml int (uint64 over 2^63, non-numeric). *)
let rec int_of : type a. a typ -> a -> int option =
 fun typ v ->
  match typ with
  | Uint8 -> Some v
  | Uint16 _ -> Some v
  | Uint_var _ -> Some v
  | Uint32 _ -> Some (UInt32.to_int v)
  | Uint63 _ -> Some (UInt63.to_int v)
  | Uint64 _ -> Int64.unsigned_to_int v
  | Bits _ -> Some v
  | Enum { base; _ } -> int_of base v
  | Where { inner; _ } -> int_of inner v
  | Single_elem { elem; _ } -> int_of elem v
  | Apply { typ; _ } -> int_of typ v
  | Map { inner; encode; _ } -> int_of inner (encode v)
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ | Codec _ | Optional _
  | Optional_or _ | Repeat _ ->
      None

let rec expr : type a. ctx -> a expr -> a =
 fun ctx e ->
  match e with
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref name ->
      failwith
        ("Eval.expr: unbound field " ^ name
       ^ " (cross-field references are only valid inside a struct)")
  | Param_ref p -> !(p.ph_cell)
  | Sizeof t -> field_wire_size t |> Option.value ~default:0
  | Sizeof_this -> 0
  | Field_pos -> 0
  | Add (a, b) -> expr ctx a + expr ctx b
  | Sub (a, b) -> expr ctx a - expr ctx b
  | Mul (a, b) -> expr ctx a * expr ctx b
  | Div (a, b) -> expr ctx a / expr ctx b
  | Mod (a, b) -> expr ctx a mod expr ctx b
  | Land (a, b) -> expr ctx a land expr ctx b
  | Lor (a, b) -> expr ctx a lor expr ctx b
  | Lxor (a, b) -> expr ctx a lxor expr ctx b
  | Lnot a -> lnot (expr ctx a)
  | Lsl (a, b) -> expr ctx a lsl expr ctx b
  | Lsr (a, b) -> expr ctx a lsr expr ctx b
  | Eq (a, b) -> expr ctx a = expr ctx b
  | Ne (a, b) -> expr ctx a <> expr ctx b
  | Lt (a, b) -> expr ctx a < expr ctx b
  | Le (a, b) -> expr ctx a <= expr ctx b
  | Gt (a, b) -> expr ctx a > expr ctx b
  | Ge (a, b) -> expr ctx a >= expr ctx b
  | And (a, b) -> expr ctx a && expr ctx b
  | Or (a, b) -> expr ctx a || expr ctx b
  | Not a -> not (expr ctx a)
  | Cast (width, e) -> (
      let v = expr ctx e in
      match width with
      | `U8 -> v land 0xFF
      | `U16 -> v land 0xFFFF
      | `U32 -> v land 0xFFFF_FFFF
      | `U64 -> v)
  | If_then_else (c, t, e) -> if expr ctx c then expr ctx t else expr ctx e
