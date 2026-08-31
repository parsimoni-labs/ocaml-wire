(* Top-level expression evaluator.

   The full struct-internal expression machinery (with [Ref]/[Sizeof_this]/
   [Field_pos] resolution against bound fields) lives in [Codec] as the
   [compile_int_arr] family, which compiles expressions to [int array]
   accessors at codec construction. This module is the residual evaluator
   for the [Wire.of_string]/[Wire.encode] paths, which only ever
   evaluate expressions in [empty]: no field references, no cross-field
   dependencies. *)

open Types

(* The binding is mutable so a per-byte refinement can be decided in place: a
   fresh association per byte would make scanning a [byte_array_where] allocate
   in proportion to the span it is protecting. *)
type binding = { name : string; mutable value : int }
type ctx = binding list

let empty : ctx = []
let bind name value ctx = { name; value } :: ctx

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
  | b :: tl -> if String.equal b.name name then b.value else lookup name tl

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
  | Int64 _ -> compare_int64_expr ctx a b
  | Ref (I64, _) -> compare_int64_expr ctx a b
  | _ -> Stdlib.compare (expr ctx a) (expr ctx b)

and compare_int64_expr ctx (a : int64 expr) (b : int64 expr) =
  Int64.unsigned_compare (expr ctx a) (expr ctx b)

(* [byte_array_where] refines every byte of the span, and every path that reads
   or writes one decides it here: the compiled codec, the direct parser, both
   encoders and the EverParse validator built from the same schema all admit
   exactly the same bytes. The loop stays at top level rather than closing over
   the span, so a scan allocates only its single binding. *)
let rec scan_bytes ctx elt cond buf ~first ~len i =
  if i >= len then -1
  else begin
    elt.value <- Bytes.get_uint8 buf (first + i);
    if expr ctx cond then scan_bytes ctx elt cond buf ~first ~len (i + 1) else i
  end

let bad_byte ~elt_var ~cond buf ~first ~len =
  let elt = { name = elt_var; value = 0 } in
  scan_bytes [ elt ] elt cond buf ~first ~len 0

let check_byte_refinement ~elt_var ~cond s =
  let len = String.length s in
  (* [bad_byte] only reads, so the aliased bytes never escape as mutable. *)
  let i = bad_byte ~elt_var ~cond (Bytes.unsafe_of_string s) ~first:0 ~len in
  if i >= 0 then
    Fmt.invalid_arg
      "Wire.encode: byte_array_where byte %d = 0x%02x violates its per-byte \
       constraint %a"
      i
      (Char.code s.[i])
      Types.pp_expr cond
