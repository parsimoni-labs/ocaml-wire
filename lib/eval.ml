(* Top-level expression evaluator and value-to-int conversion.

   The full struct-internal expression machinery (with [Ref]/[Sizeof_this]/
   [Field_pos] resolution against bound fields) lives in [Codec] as the
   [compile_int_arr] family, which compiles expressions to [int array]
   accessors at codec construction. This module is the residual evaluator
   for the [Wire.of_string]/[Wire.encode] paths, which only ever
   evaluate expressions in [empty]: no field references, no cross-field
   dependencies. *)

open Types

type ctx = (string * int) list

let empty : ctx = []
let bind name v ctx = (name, v) :: ctx

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
  | Int8 -> Some v
  | Int16 _ -> Some v
  | Int32 _ -> Some v
  | Int64 _ -> Int64.unsigned_to_int v
  | Float32 _ -> None
  | Float64 _ -> None
  | Bits _ -> Some v
  | Enum { base; _ } -> int_of base v
  | Where { inner; _ } -> int_of inner v
  | Single_elem { elem; _ } -> int_of elem v
  | Apply { typ; _ } -> int_of typ v
  | Map { inner; encode; _ } -> int_of inner (encode v)
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Struct _
  | Type_ref _ | Qualified_ref _ | Codec _ | Optional _ | Optional_or _
  | Repeat _ ->
      None

(* Hot-path variant of [int_of] for the cross-field size/offset/present
   readers, which need a plain [int]. Returns it directly (no [Some] box on the
   numeric path) and raises [Parse_error] when the value is not a usable int: a
   [uint64]/[int64] beyond the native int range (adversarial input), or a
   non-integer field referenced where an integer is required (a schema error). *)
let int_overflow () =
  raise
    (Parse_error
       (Constraint_failed "integer field value exceeds the native int range"))

let not_an_integer () =
  invalid_arg "Wire: non-integer field referenced where an integer is required"

let rec int_of_exn : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint_var _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ -> (
      match Int64.unsigned_to_int v with Some n -> n | None -> int_overflow ())
  | Int8 -> v
  | Int16 _ -> v
  | Int32 _ -> v
  | Int64 _ -> (
      match Int64.unsigned_to_int v with Some n -> n | None -> int_overflow ())
  | Float32 _ -> not_an_integer ()
  | Float64 _ -> not_an_integer ()
  | Bits _ -> v
  | Enum { base; _ } -> int_of_exn base v
  | Where { inner; _ } -> int_of_exn inner v
  | Single_elem { elem; _ } -> int_of_exn elem v
  | Apply { typ; _ } -> int_of_exn typ v
  | Map { inner; encode; _ } -> int_of_exn inner (encode v)
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Struct _
  | Type_ref _ | Qualified_ref _ | Codec _ | Optional _ | Optional_or _
  | Repeat _ ->
      not_an_integer ()

let rec expr : type a. ctx -> a expr -> a =
 fun ctx e ->
  match e with
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref (I, name) -> (
      match List.assoc_opt name ctx with
      | Some v -> v
      | None ->
          failwith
            ("Eval.expr: unbound field " ^ name
           ^ " (cross-field references are only valid inside a struct)"))
  | Ref (I64, name) ->
      failwith
        ("Eval.expr: unbound int64 field " ^ name
       ^ " (cross-field references are only valid inside a struct)")
  | Param_ref p -> !(p.cell)
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
  | Lt (a, b) -> compare_expr ctx a b < 0
  | Le (a, b) -> compare_expr ctx a b <= 0
  | Gt (a, b) -> compare_expr ctx a b > 0
  | Ge (a, b) -> compare_expr ctx a b >= 0
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

and compare_expr : type a. ctx -> a expr -> a expr -> int =
 fun ctx a b ->
  match a with
  | Int64 _ -> compare_int64_expr ctx a b
  | Ref (I64, _) -> compare_int64_expr ctx a b
  | _ -> Stdlib.compare (expr ctx a) (expr ctx b)

and compare_int64_expr ctx (a : int64 expr) (b : int64 expr) =
  Int64.unsigned_compare (expr ctx a) (expr ctx b)
