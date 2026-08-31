(* Tests for Expr_compiler, the walk from the [expr] GADT to a closure.

   The module is parametric in leaf resolution, so the whole operator table is
   reachable from a [leaves] record built here: no buffer, no int slots and no
   sealed codec. The four curried context arguments carry an int environment, a
   64-bit environment, the size of the value being read and a field position,
   which also pins that a leaf reads the context it was given rather than a
   captured one.

   What is left to check is the arithmetic itself, the [Eq]/[Ne] refinement
   that picks a compiler from the operand type, and the 64-bit fallback that
   exists precisely because some expressions do not fit a native int. *)

open Wire.Private

type ints = (string * int) list
type i64s = (string * int64) list

let param name : (Wire.UInt8.t, Types.param_input) Types.param_handle =
  {
    Types.id = 1;
    name;
    typ = Types.uint8;
    packed_typ = Types.Pack_typ Types.uint8;
    mutable_ = false;
  }

let leaves : (ints, i64s, int, int) Expr_compiler.leaves =
  {
    ref_ = (fun name ints _ _ _ -> List.assoc name ints);
    i64 = (fun name _ i64s _ _ -> List.assoc name i64s);
    param_ref =
      (fun (Expr_compiler.Pack_param p) ints _ _ _ ->
        List.assoc p.Types.name ints);
    sizeof_typ =
      (fun (Types.Pack_typ t) _ _ _ _ ->
        Option.value ~default:0 (Types.field_wire_size t));
    sizeof_this = (fun _ _ size _ -> size);
    field_pos = (fun _ _ _ pos -> pos);
  }

let ints : ints = [ ("a", 10); ("b", 3); ("limit", 7) ]
let i64s : i64s = [ ("wide", 0xFFFF_FFFF_FFFF_FFFFL); ("small", 5L) ]
let size = 16
let pos = 12
let eval_int e = Expr_compiler.compile_int leaves e ints i64s size pos
let eval_bool e = Expr_compiler.compile_bool leaves e ints i64s size pos
let int n : int Types.expr = Types.Int n
let int64 n : int64 Types.expr = Types.Int64 n
let bool b : bool Types.expr = Types.Bool b
let a = Types.ref "a"
let b = Types.ref "b"
let wide = Types.Ref (Types.I64, "wide")
let small = Types.Ref (Types.I64, "small")

(* Every leaf form goes through the record, so a caller that supplies one
   strategy gets all five without the compiler knowing what a buffer is. *)
let test_leaves () =
  Alcotest.(check int) "constant" 42 (eval_int (int 42));
  Alcotest.(check int) "Ref" 10 (eval_int a);
  Alcotest.(check int)
    "Param_ref" 7
    (eval_int (Types.Param_ref (param "limit")));
  Alcotest.(check int) "Sizeof" 2 (eval_int (Types.Sizeof Types.uint16be));
  Alcotest.(check int) "Sizeof_this" size (eval_int Types.Sizeof_this);
  Alcotest.(check int) "Field_pos" pos (eval_int Types.Field_pos)

let test_arithmetic () =
  Alcotest.(check int) "Add" 15 (eval_int (Types.Add (a, int 5)));
  Alcotest.(check int) "Sub" 7 (eval_int (Types.Sub (a, b)));
  Alcotest.(check int) "Mul" 30 (eval_int (Types.Mul (a, b)));
  Alcotest.(check int)
    "nested" 13
    (eval_int (Types.Add (a, Types.Sub (b, int 0))))

let test_arithmetic_overflow () =
  let check name e =
    match eval_int e with
    | n -> Alcotest.failf "%s: overflow produced %d" name n
    | exception Types.Parse_error { kind = Types.Value_out_of_range _; _ } -> ()
    | exception exn ->
        Alcotest.failf "%s: expected Value_out_of_range, got %s" name
          (Printexc.to_string exn)
  in
  (* These are not merely just past the native-int boundary: unchecked
     arithmetic wraps each mathematical result to zero or one, exactly the
     small sizes that used to pass a byte-span bounds check. *)
  check "Add" (Types.Add (Types.Add (int max_int, int max_int), int 2));
  check "Sub" (Types.Add (Types.Sub (int max_int, int min_int), int 1));
  check "Mul" (Types.Mul (int max_int, int max_int))

(* [Div] and [Mod] are OCaml's, which truncate towards zero rather than
   flooring: a size expression over a negative intermediate must round the way
   the generated C rounds. *)
let test_div_mod () =
  Alcotest.(check int) "Div" 3 (eval_int (Types.Div (a, b)));
  Alcotest.(check int)
    "Div truncates towards zero" (-3)
    (eval_int (Types.Div (int (-10), b)));
  Alcotest.(check int) "Mod" 1 (eval_int (Types.Mod (a, b)));
  Alcotest.(check int)
    "Mod keeps the dividend's sign" (-1)
    (eval_int (Types.Mod (int (-10), b)))

(* Compilation is staged: a divisor read from the input is only known when the
   returned closure runs. It is still bad wire data, not a host-language fault
   that may escape the decoder. *)
let test_zero_divisor_is_parse_error () =
  let zero = [ ("a", 10); ("b", 0) ] in
  let check name e =
    let f = Expr_compiler.compile_int leaves e in
    match f zero i64s size pos with
    | n -> Alcotest.failf "%s by zero produced %d" name n
    | exception Types.Parse_error _ -> ()
    | exception Division_by_zero ->
        Alcotest.failf "%s leaked Division_by_zero" name
    | exception exn ->
        Alcotest.failf "%s raised %s" name (Printexc.to_string exn)
  in
  check "division" (Types.Div (a, b));
  check "modulo" (Types.Mod (a, b))

let test_bitwise () =
  Alcotest.(check int) "Land" 0x30 (eval_int (Types.Land (int 0xF0, int 0x3C)));
  Alcotest.(check int) "Lor" 0xFC (eval_int (Types.Lor (int 0xF0, int 0x3C)));
  Alcotest.(check int) "Lxor" 0xCC (eval_int (Types.Lxor (int 0xF0, int 0x3C)));
  Alcotest.(check int) "Lnot" (-1) (eval_int (Types.Lnot (int 0)));
  Alcotest.(check int) "Lsl" 256 (eval_int (Types.Lsl (int 1, int 8)));
  Alcotest.(check int) "Lsr" 0x10 (eval_int (Types.Lsr (int 0x100, int 4)));
  (* [Lsr] is logical, so the all-ones word shifts in zeros; an arithmetic
     shift would answer -1 here on every int width. *)
  Alcotest.(check int)
    "Lsr is logical" 0xF
    (eval_int (Types.Lsr (Types.Lnot (int 0), int (Sys.int_size - 4))))

let test_cast () =
  Alcotest.(check int) "U8" 0x34 (eval_int (Types.Cast (`U8, int 0x1234)));
  Alcotest.(check int) "U8 of -1" 0xFF (eval_int (Types.Cast (`U8, int (-1))));
  Alcotest.(check int)
    "U16" 0x5678
    (eval_int (Types.Cast (`U16, int 0x12345678)));
  (* The operand does not fit a narrow int, so it is built at run time and the
     expectation derives from the same mask the cast applies. *)
  let big = Int64.to_int 0x1_2345_6789L in
  Alcotest.(check int)
    "U32" (big land UInt32.mask32)
    (eval_int (Types.Cast (`U32, int big)));
  Alcotest.(check int)
    "U64 is the identity" 42
    (eval_int (Types.Cast (`U64, int 42)))

(* Only the taken branch runs, so a size expression may guard a division that
   would fault on the other side of the test. *)
let test_if_then_else () =
  Alcotest.(check int)
    "then" 1
    (eval_int (Types.If_then_else (bool true, int 1, int 2)));
  Alcotest.(check int)
    "else" 2
    (eval_int (Types.If_then_else (bool false, int 1, int 2)));
  Alcotest.(check int)
    "untaken branch is not evaluated" 1
    (eval_int (Types.If_then_else (bool true, int 1, Types.Div (int 1, int 0))))

let test_comparisons () =
  Alcotest.(check bool) "Lt" true (eval_bool (Types.Lt (b, a)));
  Alcotest.(check bool) "Lt false" false (eval_bool (Types.Lt (a, b)));
  Alcotest.(check bool) "Le on equals" true (eval_bool (Types.Le (a, int 10)));
  Alcotest.(check bool) "Gt" true (eval_bool (Types.Gt (a, b)));
  Alcotest.(check bool) "Ge on equals" true (eval_bool (Types.Ge (a, int 10)));
  Alcotest.(check bool) "Ge false" false (eval_bool (Types.Ge (b, a)))

let test_bool_ops () =
  Alcotest.(check bool) "Bool" true (eval_bool (bool true));
  Alcotest.(check bool)
    "And" false
    (eval_bool (Types.And (bool true, bool false)));
  Alcotest.(check bool) "Or" true (eval_bool (Types.Or (bool false, bool true)));
  Alcotest.(check bool) "Not" true (eval_bool (Types.Not (bool false)));
  (* [&&] and [||] keep OCaml's short-circuit, so a guard may protect the
     operand that follows it. *)
  Alcotest.(check bool)
    "And short-circuits" false
    (eval_bool (Types.And (bool false, Types.Eq (Types.Div (a, int 0), int 0))));
  Alcotest.(check bool)
    "Or short-circuits" true
    (eval_bool (Types.Or (bool true, Types.Eq (Types.Div (a, int 0), int 0))))

(* [Eq]/[Ne] are polymorphic in the operand type, so the compiler refines the
   operands to pick between the native-int, the 64-bit and the boolean
   comparison. All three must be reachable from the same constructor. *)
let test_eq_ne_refines_to_int () =
  Alcotest.(check bool) "Eq" true (eval_bool (Types.Eq (a, int 10)));
  Alcotest.(check bool) "Eq false" false (eval_bool (Types.Eq (a, b)));
  Alcotest.(check bool) "Ne" true (eval_bool (Types.Ne (a, b)));
  Alcotest.(check bool) "Ne false" false (eval_bool (Types.Ne (a, int 10)));
  (* A non-[Ref] int leaf refines the same way. *)
  Alcotest.(check bool)
    "Eq on Sizeof_this" true
    (eval_bool (Types.Eq (Types.Sizeof_this, int 16)))

let test_eq_ne_refines_to_int64 () =
  Alcotest.(check bool)
    "Eq" true
    (eval_bool (Types.Eq (wide, int64 0xFFFF_FFFF_FFFF_FFFFL)));
  Alcotest.(check bool) "Eq false" false (eval_bool (Types.Eq (wide, small)));
  Alcotest.(check bool) "Ne" true (eval_bool (Types.Ne (wide, small)));
  Alcotest.(check bool)
    "Ne false" false
    (eval_bool (Types.Ne (small, int64 5L)))

let test_eq_ne_refines_to_bool () =
  Alcotest.(check bool)
    "Eq" true
    (eval_bool (Types.Eq (Types.Not (bool false), bool true)));
  Alcotest.(check bool)
    "Eq false" false
    (eval_bool (Types.Eq (bool true, bool false)));
  Alcotest.(check bool) "Ne" true (eval_bool (Types.Ne (bool true, bool false)));
  (* A comparison is itself a boolean operand, so it refines too. *)
  Alcotest.(check bool)
    "Eq on two comparisons" true
    (eval_bool (Types.Eq (Types.Lt (b, a), Types.Gt (a, b))))

(* A 64-bit field carries an unsigned value, so an ordering on it is unsigned:
   an all-ones word is the largest value, not -1. *)
let test_int64_comparisons_are_unsigned () =
  Alcotest.(check bool) "Lt" true (eval_bool (Types.Lt (small, wide)));
  Alcotest.(check bool) "Lt false" false (eval_bool (Types.Lt (wide, small)));
  Alcotest.(check bool)
    "Le on equals" true
    (eval_bool (Types.Le (wide, int64 (-1L))));
  Alcotest.(check bool) "Gt" true (eval_bool (Types.Gt (wide, small)));
  Alcotest.(check bool) "Ge" true (eval_bool (Types.Ge (wide, wide)));
  Alcotest.(check bool) "Ge false" false (eval_bool (Types.Ge (small, wide)))

(* The 64-bit fallback exists for values a native int cannot hold: the mask and
   the shift stay exact on a platform whose int is narrower than 64 bits. *)
let test_int64_fallback () =
  Alcotest.(check bool)
    "Land64 keeps the low byte" true
    (eval_bool (Types.Eq (Types.Land64 (wide, int64 0xFFL), int64 0xFFL)));
  Alcotest.(check bool)
    "Land64 clears" true
    (eval_bool (Types.Eq (Types.Land64 (small, int64 0x2L), int64 0L)));
  (* [Lsr64] is logical: the top nibble of an all-ones word shifts down to
     0xF, where an arithmetic shift would keep the sign and answer -1. *)
  Alcotest.(check bool)
    "Lsr64 is logical" true
    (eval_bool (Types.Eq (Types.Lsr64 (wide, int 60), int64 0xFL)));
  (* The float64 exponent shape the predicates in [Wire] build. *)
  Alcotest.(check bool)
    "exponent bits" true
    (eval_bool
       (Types.Eq
          ( Types.Land64
              (Types.Lsr64 (int64 0x7FF0_0000_0000_0000L, int 52), int64 0x7FFL),
            int64 0x7FFL )))

(* Only a constant shift projects to 3D, so a computed one is refused when the
   codec is built rather than silently compiled to something 3D cannot say. *)
let test_lsr64_shift_must_be_constant () =
  Alcotest.check_raises "computed Lsr64 shift"
    (Invalid_argument "Wire: Lsr64 shift amount must be a constant") (fun () ->
      (* Binding the closure rather than calling it shows the refusal lands on
         the walk over the expression, before any context is supplied. *)
      let _ : ints -> i64s -> int -> int -> bool =
        Expr_compiler.compile_bool leaves
          (Types.Eq (Types.Lsr64 (wide, a), int64 0L))
      in
      ())

(* The walk over the expression happens in the partial application, so a leaf
   resolves once when the codec is sealed and not again per decode. *)
let test_leaf_resolution_is_staged () =
  let resolutions = ref 0 in
  let counting : (ints, i64s, int, int) Expr_compiler.leaves =
    {
      leaves with
      ref_ =
        (fun name ->
          incr resolutions;
          leaves.ref_ name);
    }
  in
  let f =
    Expr_compiler.compile_int counting (Types.Add (a, Types.Mul (b, a)))
  in
  Alcotest.(check int) "one resolution per Ref occurrence" 3 !resolutions;
  for _ = 1 to 5 do
    Alcotest.(check int) "value" 40 (f ints i64s size pos)
  done;
  Alcotest.(check int) "no resolution per call" 3 !resolutions

(* The context is four curried arguments, so a call reads the arguments it was
   handed: the same compiled closure answers differently for two environments. *)
let test_context_is_per_call () =
  let f = Expr_compiler.compile_int leaves (Types.Add (a, Types.Field_pos)) in
  Alcotest.(check int) "first context" 22 (f ints i64s size pos);
  Alcotest.(check int) "second context" 105 (f [ ("a", 100) ] [] 0 5)

let suite =
  ( "expr_compiler",
    [
      Alcotest.test_case "leaves" `Quick test_leaves;
      Alcotest.test_case "arithmetic" `Quick test_arithmetic;
      Alcotest.test_case "arithmetic overflow" `Quick test_arithmetic_overflow;
      Alcotest.test_case "div and mod" `Quick test_div_mod;
      Alcotest.test_case "zero divisor is a parse error" `Quick
        test_zero_divisor_is_parse_error;
      Alcotest.test_case "bitwise" `Quick test_bitwise;
      Alcotest.test_case "cast widths" `Quick test_cast;
      Alcotest.test_case "if_then_else" `Quick test_if_then_else;
      Alcotest.test_case "comparisons" `Quick test_comparisons;
      Alcotest.test_case "boolean operators" `Quick test_bool_ops;
      Alcotest.test_case "Eq/Ne at int" `Quick test_eq_ne_refines_to_int;
      Alcotest.test_case "Eq/Ne at int64" `Quick test_eq_ne_refines_to_int64;
      Alcotest.test_case "Eq/Ne at bool" `Quick test_eq_ne_refines_to_bool;
      Alcotest.test_case "int64 ordering is unsigned" `Quick
        test_int64_comparisons_are_unsigned;
      Alcotest.test_case "int64 fallback" `Quick test_int64_fallback;
      Alcotest.test_case "Lsr64 shift must be constant" `Quick
        test_lsr64_shift_must_be_constant;
      Alcotest.test_case "leaf resolution is staged" `Quick
        test_leaf_resolution_is_staged;
      Alcotest.test_case "context is per call" `Quick test_context_is_per_call;
    ] )
