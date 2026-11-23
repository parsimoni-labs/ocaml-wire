(* Top-level expression evaluator.

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

let rec lookup name = function
  | [] ->
      (* A description referencing another field, evaluated where there is no
         record to read it from, cannot be decoded whatever the input: the
         entry points document that as [Invalid_argument], and the [Param_ref]
         arm below already raises it. *)
      Fmt.invalid_arg
        "Eval.expr: unbound field %s (cross-field references are only valid \
         inside a struct)"
        name
  | (n, v) :: tl -> if String.equal n name then v else lookup name tl

(* Value-to-int conversion is a fold over the type description, so it is
   decided next to the type definitions; re-exported here because the
   evaluator's callers reach for it alongside [expr]. *)
let int_of = Types.int_of
let int_of_exn = Types.int_of_exn

let rec expr : type a. ctx -> a expr -> a =
 fun ctx e ->
  match e with
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref (I, name) -> lookup name ctx
  | Ref (I64, name) ->
      Fmt.invalid_arg
        "Eval.expr: unbound int64 field %s (cross-field references are only \
         valid inside a struct)"
        name
  | Param_ref p ->
      Fmt.invalid_arg
        "Eval.expr: parameter %S requires a codec evaluation context" p.name
  | Sizeof t -> field_wire_size t |> Option.value ~default:0
  | Sizeof_this -> 0
  | Field_pos -> 0
  | Add (a, b) -> checked_add (expr ctx a) (expr ctx b)
  | Sub (a, b) -> checked_sub (expr ctx a) (expr ctx b)
  | Mul (a, b) -> checked_mul (expr ctx a) (expr ctx b)
  | Div (a, b) -> checked_div (expr ctx a) (expr ctx b)
  | Mod (a, b) -> checked_mod (expr ctx a) (expr ctx b)
  | Land (a, b) -> expr ctx a land expr ctx b
  | Land64 (a, b) -> Int64.logand (expr ctx a) (expr ctx b)
  | Lsr64 (a, b) -> Int64.shift_right_logical (expr ctx a) (expr ctx b)
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
      | `U32 -> v land UInt32.mask32
      | `U64 -> v)
  | If_then_else (c, t, e) -> if expr ctx c then expr ctx t else expr ctx e

and compare_expr : type a. ctx -> a expr -> a expr -> int =
 fun ctx a b ->
  match a with
  | Int64 _ -> (compare_int64_expr ctx a b : int)
  | Ref (I64, _) -> (compare_int64_expr ctx a b : int)
  | Land64 _ -> (compare_int64_expr ctx a b : int)
  | Lsr64 _ -> (compare_int64_expr ctx a b : int)
  | _ -> Stdlib.compare (expr ctx a) (expr ctx b)

and compare_int64_expr : ctx -> int64 expr -> int64 expr -> int =
 fun ctx a b -> Int64.unsigned_compare (expr ctx a) (expr ctx b)

(* A per-byte condition reads the byte [elt_var] alone, and is computed at the
   width 3D gives it: in the native int, or on 64 bits, unsigned, where its
   value can leave the native int ({!Types.wide_expr}). *)
let byte_test ~elt_var cond =
  let unbound name _ () () () = lookup name [] in
  let leaves : (int, unit, unit, unit) Expr_compiler.leaves =
    {
      ref_ =
        (fun name ->
          if String.equal name elt_var then fun b () () () -> b
          else unbound name);
      i64 =
        (fun name _ () () () ->
          Fmt.invalid_arg
            "Eval.expr: unbound int64 field %s (cross-field references are \
             only valid inside a struct)"
            name);
      wide = (fun _ -> None);
      wide_expr =
        Types.wide_expr (fun name ->
            if String.equal name elt_var then Some (Pack_typ Uint8) else None);
      param_ref =
        (fun (Expr_compiler.Pack_param p) _ () () () ->
          Fmt.invalid_arg
            "Eval.expr: parameter %S requires a codec evaluation context" p.name);
      sizeof_typ =
        (fun (Pack_typ t) ->
          let n = Option.value ~default:0 (field_wire_size t) in
          fun _ () () () -> n);
      (* The one-byte element struct the condition projects to. *)
      sizeof_this = (fun _ () () () -> 1);
      field_pos = (fun _ () () () -> 0);
    }
  in
  let test = Expr_compiler.compile_bool leaves cond in
  fun b -> test b () () ()

(* [byte_array_where] refines every byte of the span, and every path that reads
   or writes one decides it here: the compiled codec, the direct parser, both
   encoders and the EverParse validator built from the same schema all admit
   exactly the same bytes. *)
let bad_byte ~elt_var ~cond =
  let test = byte_test ~elt_var cond in
  fun buf ~first ~len ->
    let rec scan i =
      if i >= len then -1
      else if test (Bytes.get_uint8 buf (first + i)) then scan (i + 1)
      else i
    in
    scan 0

let check_byte_refinement ~elt_var ~cond =
  let bad = bad_byte ~elt_var ~cond in
  fun s ->
    let len = String.length s in
    (* [bad] only reads, so the aliased bytes never escape as mutable. *)
    let i = bad (Bytes.unsafe_of_string s) ~first:0 ~len in
    if i >= 0 then
      Fmt.invalid_arg
        "Wire.encode: byte_array_where byte %d = 0x%02x violates its per-byte \
         constraint %a"
        i
        (Char.code s.[i])
        Types.pp_expr cond
