(* How generated code holds integers, and how it writes a literal, an
   operation and a comparison on them, as OCaml or as F*. *)

open Planner

(* How the code holds integers. [Tagged] is [int]. [Boxed] is [int64] for
   values and [int] for positions; the stock compiler keeps such an [int64]
   unboxed in a register. [Unboxed] is OxCaml's [int64_u]. [Fstar] is
   [Tagged] written as F*, [Fstar64] is [Boxed] and [Fstar_u] is [Unboxed],
   an [int64_u] as its 64-bit pattern: the walk takes the same decisions,
   and writes each operation and load as a call into the module [P] of the
   F* primitives. *)
type t = Tagged | Boxed | Unboxed | Fstar | Fstar64 | Fstar_u

let pp ppf t =
  Fmt.string ppf
    (match t with
    | Tagged -> "Tagged"
    | Boxed -> "Boxed"
    | Unboxed -> "Unboxed"
    | Fstar -> "Fstar"
    | Fstar64 -> "Fstar64"
    | Fstar_u -> "Fstar_u")

(* How [num] holds values, whether written as OCaml or as F*. *)
let rep = function
  | Fstar -> Tagged
  | Fstar64 -> Boxed
  | Fstar_u -> Unboxed
  | num -> num

(* Whether [num] is written as F*. *)
let is_fstar = function
  | Fstar | Fstar64 | Fstar_u -> true
  | Tagged | Boxed | Unboxed -> false

(* How the code holds positions, sizes and bounds: a load takes an [int]. *)
let pos_num = function Boxed -> Tagged | Fstar64 -> Fstar | num -> num

(* The module of the operations on values held as [num], other than [int]. *)
let i64_ops = function
  | Unboxed -> "U"
  | Boxed | Tagged -> "Int64"
  | Fstar | Fstar64 | Fstar_u -> "P"

(* A 64-bit literal written as [digits], an OCaml literal without its
   suffix: an F* [int64] is its pattern. *)
let lit64 num digits =
  match num with
  | Unboxed -> "#" ^ digits ^ "L"
  | Fstar | Fstar64 | Fstar_u -> digits
  | Tagged | Boxed -> digits ^ "L"

(* The conversion to the [int64] (or [int64_u]) of [num] of a value of
   [from] ([int] or [int32]). *)
let int64_of num from =
  match num with
  | Unboxed -> if from = "int32" then "U.of_int32_u" else "U.of_int"
  | Fstar | Fstar64 | Fstar_u -> "P.int64_of_" ^ from
  | Tagged | Boxed -> "Int64.of_" ^ from

(* The [int] variable [x] as an [int64]. *)
let widen_int num x = Fmt.str "(%s %s)" (int64_of num "int") x

(* The [w] bits from bit [lo] up of a word, as an F* walk states a bitfield:
   of an [int], or of an [int64] pattern. *)
let bits_of num = if num = Fstar then "P.bits" else "P.bits64"

(* How a walk of [num] holds integer values. *)
let values : t -> Decision.values = function
  | Tagged | Fstar -> `Int
  | Boxed | Fstar64 -> `Int64
  | Unboxed | Fstar_u -> `Int64_u

let lit num n =
  match num with
  | Tagged -> int_lit n
  | Fstar ->
      (* As [Tagged] writes it, a literal past 31 bits through [int64]. *)
      let wide = String.starts_with ~prefix:"(Int64" (int_lit n) in
      if wide && n < 0 then
        fail_unsupported "the F* rendering of %d, a negative int64 literal" n
      else if wide then Fmt.str "(P.to_int %d)" n
      else if n < 0 then Fmt.str "(%d)" n
      else string_of_int n
  | Boxed -> if n < 0 then Fmt.str "(%dL)" n else Fmt.str "%dL" n
  | Fstar64 | Fstar_u ->
      (* An [int64] is its 64-bit pattern. *)
      Fmt.str "%Lu" (Int64.of_int n)
  | Unboxed ->
      if n < 0 then Fmt.str "(-#%LdL)" (Int64.neg (Int64.of_int n))
      else Fmt.str "#%dL" n

let unboxed_op = function
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "mod" -> "rem"
  | "land" -> "logand"
  | "lor" -> "logor"
  | "lxor" -> "logxor"
  | op -> invalid_arg op

(* The primitive of [P] an F* walk calls for the OCaml operator [op]. *)
let fstar_op = function
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | ("land" | "lsr" | "lsl") as op -> op
  | "<" -> "lt"
  | "<=" -> "le"
  | ">" -> "gt"
  | ">=" -> "ge"
  | "=" -> "eq"
  | "<>" -> "ne"
  | op -> fail_unsupported "the F* rendering of %s" op

(* The primitive of [P] an F* walk calls for the [int64] operation of the
   OCaml operator [op]: that of [Int64], prefixed. *)
let fstar64_op = function
  | ("+" | "-" | "*" | "land" | "lor" | "lxor") as op ->
      "int64_" ^ unboxed_op op
  | op -> "int64_" ^ fstar_op op

let binop num op a b =
  match num with
  | Tagged -> Fmt.str "(%s %s %s)" a op b
  | Fstar -> Fmt.str "(P.%s %s %s)" (fstar_op op) a b
  | Fstar64 | Fstar_u -> Fmt.str "(P.%s %s %s)" (fstar64_op op) a b
  | Boxed | Unboxed -> Fmt.str "(%s.%s %s %s)" (i64_ops num) (unboxed_op op) a b

(* The type annotation lets the stock compiler compare two [int64] inline. *)
let relation num op a b =
  match num with
  | Tagged -> Fmt.str "((%s : int) %s %s)" a op b
  | Fstar -> Fmt.str "(P.%s %s %s)" (fstar_op op) a b
  | Fstar64 | Fstar_u -> Fmt.str "(P.%s %s %s)" (fstar64_op op) a b
  | Boxed -> Fmt.str "((%s : int64) %s %s)" a op b
  | Unboxed -> Fmt.str "(U.compare %s %s %s 0)" a b op

(* The test that [v] lies outside [lo, hi]. An [int64_u] is tested as one
   unsigned comparison of [v - lo] with [hi - lo], which wraps a [v] below
   [lo] past [hi - lo]. *)
let outside_range num v ~lo ~hi =
  match num with
  | Unboxed | Fstar_u ->
      let d = if lo = 0 then v else binop num "-" v (lit num lo) in
      if num = Fstar_u then
        Fmt.str "(P.int64_unsigned_gt %s %s)" d (lit num (hi - lo))
      else Fmt.str "(B.unsigned_compare %s %s > 0)" d (lit num (hi - lo))
  | Tagged | Boxed | Fstar | Fstar64 ->
      Fmt.str "%s || %s"
        (relation num "<" v (lit num lo))
        (relation num ">" v (lit num hi))

(* An [int64] or [int64_u] holds the value of a 63-bit [int]
   sign-extended. *)
let normalise63 num v =
  if is_fstar num then
    fail_unsupported "the F* rendering of an int64 normalised to 63 bits";
  let u = i64_ops num in
  Fmt.str "(%s.shift_right (%s.shift_left %s 1) 1)" u u v
