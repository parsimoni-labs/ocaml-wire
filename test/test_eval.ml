(* Tests for Eval: expression evaluator and action interpreter. *)

open Wire.Private

let test_empty_ctx () =
  let ctx = Eval.empty in
  let ctx' = Eval.bind ctx "x" 42 in
  let v = Eval.expr ctx' (Types.Ref "x") in
  Alcotest.(check int) "bind + expr" 42 v

let test_int_of () =
  Alcotest.(check int) "uint8" 7 (Eval.int_of Types.uint8 7);
  Alcotest.(check int) "uint16be" 300 (Eval.int_of Types.uint16be 300)

let test_set_pos () =
  let ctx = Eval.empty in
  let ctx = Eval.set_pos ctx ~sizeof_this:10 ~field_pos:2 in
  Alcotest.(check int) "sizeof_this" 10 (Eval.expr ctx Types.Sizeof_this);
  Alcotest.(check int) "field_pos" 2 (Eval.expr ctx Types.Field_pos)

let test_action_none () =
  let ctx = Eval.empty in
  let ctx' = Eval.action ctx None in
  ignore ctx'

let suite =
  ( "eval",
    [
      Alcotest.test_case "bind and expr" `Quick test_empty_ctx;
      Alcotest.test_case "int_of" `Quick test_int_of;
      Alcotest.test_case "set_pos" `Quick test_set_pos;
      Alcotest.test_case "action none" `Quick test_action_none;
    ] )
