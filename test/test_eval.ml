(* Tests for Eval: expression evaluator and action interpreter. *)

open Wire.Private

let test_empty_ctx () =
  let ctx = Eval.empty in
  let ctx' = Eval.bind ctx "x" 42 in
  let v = Eval.expr ctx' (Types.Ref "x") in
  Alcotest.(check int) "bind + expr" 42 v

let test_int_of () =
  Alcotest.(check (option int)) "uint8" (Some 7) (Eval.int_of Types.uint8 7);
  Alcotest.(check (option int))
    "uint16be" (Some 300)
    (Eval.int_of Types.uint16be 300);
  (* uint64 within range *)
  Alcotest.(check (option int))
    "uint64 small" (Some 42)
    (Eval.int_of Types.uint64be 42L);
  (* uint64 exceeding 63-bit range -> None *)
  Alcotest.(check (option int))
    "uint64 overflow" None
    (Eval.int_of Types.uint64be 0xFFFF_FFFF_FFFF_FFFFL);
  (* non-numeric -> None *)
  Alcotest.(check (option int)) "unit" None (Eval.int_of Types.Unit ())

let test_set_pos () =
  let ctx = Eval.empty in
  let ctx = Eval.set_pos ctx ~sizeof_this:10 ~field_pos:2 in
  Alcotest.(check int) "sizeof_this" 10 (Eval.expr ctx Types.Sizeof_this);
  Alcotest.(check int) "field_pos" 2 (Eval.expr ctx Types.Field_pos)

let test_action_none () =
  let ctx = Eval.empty in
  let ctx' = Eval.action ctx None in
  ignore ctx'

let test_cast_u8 () =
  let ctx = Eval.bind Eval.empty "x" 0x1234 in
  let v = Eval.expr ctx (Types.Cast (`U8, Types.Ref "x")) in
  Alcotest.(check int) "cast U8" 0x34 v

let test_cast_u16 () =
  let ctx = Eval.bind Eval.empty "x" 0x12345678 in
  let v = Eval.expr ctx (Types.Cast (`U16, Types.Ref "x")) in
  Alcotest.(check int) "cast U16" 0x5678 v

let test_cast_u32 () =
  let ctx = Eval.bind Eval.empty "x" 0x123456789 in
  let v = Eval.expr ctx (Types.Cast (`U32, Types.Ref "x")) in
  Alcotest.(check int) "cast U32" 0x23456789 v

let test_cast_u64 () =
  let ctx = Eval.bind Eval.empty "x" 42 in
  let v = Eval.expr ctx (Types.Cast (`U64, Types.Ref "x")) in
  Alcotest.(check int) "cast U64 (identity)" 42 v

let test_cast_negative () =
  let ctx = Eval.bind Eval.empty "x" (-1) in
  let v = Eval.expr ctx (Types.Cast (`U8, Types.Ref "x")) in
  Alcotest.(check int) "cast U8 of -1" 0xFF v

let suite =
  ( "eval",
    [
      Alcotest.test_case "bind and expr" `Quick test_empty_ctx;
      Alcotest.test_case "int_of" `Quick test_int_of;
      Alcotest.test_case "set_pos" `Quick test_set_pos;
      Alcotest.test_case "action none" `Quick test_action_none;
      Alcotest.test_case "cast U8" `Quick test_cast_u8;
      Alcotest.test_case "cast U16" `Quick test_cast_u16;
      Alcotest.test_case "cast U32" `Quick test_cast_u32;
      Alcotest.test_case "cast U64" `Quick test_cast_u64;
      Alcotest.test_case "cast negative" `Quick test_cast_negative;
    ] )
