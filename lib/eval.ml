(* Expression evaluator, action interpreter, and parsing context. *)

open Types
module Fields = Map.Make (String)

type ctx = { fields : int Fields.t; sizeof_this : int; field_pos : int }

let empty = { fields = Fields.empty; sizeof_this = 0; field_pos = 0 }

let of_params params =
  List.fold_left
    (fun ctx (name, v) -> { ctx with fields = Fields.add name v ctx.fields })
    empty (Param.to_ctx params)

let commit ctx params =
  Fields.iter (fun name v -> Param.store_name params name v) ctx.fields

let bind ctx name v = { ctx with fields = Fields.add name v ctx.fields }
let set_pos ctx ~sizeof_this ~field_pos = { ctx with sizeof_this; field_pos }

(* Convert a typed value to [int] for context storage. All types that
   appear in constraint expressions are numeric, so this conversion is
   lossless for practical schemas. Non-numeric types store 0. *)
let rec int_of : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ -> Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Bits _ -> v
  | Enum { base; _ } -> int_of base v
  | Where { inner; _ } -> int_of inner v
  | Single_elem { elem; _ } -> int_of elem v
  | Apply { typ; _ } -> int_of typ v
  | Map { inner; encode; _ } -> int_of inner (encode v)
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      0

let get ctx name =
  match Fields.find_opt name ctx.fields with
  | Some v -> v
  | None -> failwith ("unbound field: " ^ name)

let rec expr : type a. ctx -> a expr -> a =
 fun ctx e ->
  match e with
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref name -> get ctx name
  | Sizeof t -> field_wire_size t |> Option.value ~default:0
  | Sizeof_this -> ctx.sizeof_this
  | Field_pos -> ctx.field_pos
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

type action_outcome = Continue of ctx | Return of bool * ctx | Abort

let rec exec_stmt ctx = function
  | Assign (name, e) -> Continue (bind ctx name (expr ctx e))
  | Return e -> Return (expr ctx e, ctx)
  | Types.Abort -> Abort
  | If (cond, then_, else_) ->
      exec_stmts ctx
        (if expr ctx cond then then_ else Option.value else_ ~default:[])
  | Var (name, e) -> Continue (bind ctx name (expr ctx e))

and exec_stmts ctx = function
  | [] -> Continue ctx
  | stmt :: rest -> (
      match exec_stmt ctx stmt with
      | Continue ctx' -> exec_stmts ctx' rest
      | Return _ as r -> r
      | Abort -> Abort)

let action ctx = function
  | None -> ctx
  | Some (On_success stmts | On_act stmts) -> (
      match exec_stmts ctx stmts with
      | Continue ctx' -> ctx'
      | Return (true, ctx') -> ctx'
      | Return (false, _) ->
          raise (Parse_error (Constraint_failed "field action"))
      | Abort -> raise (Parse_error (Constraint_failed "field action")))
