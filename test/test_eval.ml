(* Tests for Eval: expression evaluator and value-to-int conversion.

   The historical [bind]/[get]/[action]/[set_pos] tests are gone -- the
   String-Map context machinery they exercised was retired when struct
   decoding moved to the [Codec.validator_of_struct] int-array kernel. *)

open Wire.Private

let test_int_of () =
  Alcotest.(check (option int)) "uint8" (Some 7) (Eval.int_of Types.uint8 7);
  Alcotest.(check (option int))
    "uint16be" (Some 300)
    (Eval.int_of Types.uint16be 300);
  Alcotest.(check (option int))
    "uint64 small" (Some 42)
    (Eval.int_of Types.uint64be 42L);
  Alcotest.(check (option int))
    "uint64 overflow" None
    (Eval.int_of Types.uint64be 0xFFFF_FFFF_FFFF_FFFFL);
  Alcotest.(check (option int)) "unit" None (Eval.int_of Types.Unit ())

let test_expr_const () =
  let ctx = Eval.empty in
  Alcotest.(check int) "int" 42 (Eval.expr ctx (Types.Int 42));
  Alcotest.(check int)
    "add" 30
    (Eval.expr ctx (Types.Add (Types.Int 10, Types.Int 20)));
  Alcotest.(check bool)
    "lt" true
    (Eval.expr ctx (Types.Lt (Types.Int 1, Types.Int 2)))

let test_expr_ref_fails () =
  Alcotest.check_raises "Ref at top level should fail"
    (Failure
       "Eval.expr: unbound field x (cross-field references are only valid \
        inside a struct)") (fun () ->
      ignore (Eval.expr Eval.empty (Types.Ref "x")))

let test_cast_u8 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U8, Types.Int 0x1234)) in
  Alcotest.(check int) "cast U8" 0x34 v

let test_cast_u16 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U16, Types.Int 0x12345678)) in
  Alcotest.(check int) "cast U16" 0x5678 v

let test_cast_u32 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U32, Types.Int 0x123456789)) in
  Alcotest.(check int) "cast U32" 0x23456789 v

let test_cast_u64 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U64, Types.Int 42)) in
  Alcotest.(check int) "cast U64 (identity)" 42 v

let test_cast_negative () =
  let v = Eval.expr Eval.empty (Types.Cast (`U8, Types.Int (-1))) in
  Alcotest.(check int) "cast U8 of -1" 0xFF v

let suite =
  ( "eval",
    [
      Alcotest.test_case "int_of" `Quick test_int_of;
      Alcotest.test_case "expr constants" `Quick test_expr_const;
      Alcotest.test_case "expr Ref fails" `Quick test_expr_ref_fails;
      Alcotest.test_case "cast U8" `Quick test_cast_u8;
      Alcotest.test_case "cast U16" `Quick test_cast_u16;
      Alcotest.test_case "cast U32" `Quick test_cast_u32;
      Alcotest.test_case "cast U64" `Quick test_cast_u64;
      Alcotest.test_case "cast negative" `Quick test_cast_negative;
    ] )
