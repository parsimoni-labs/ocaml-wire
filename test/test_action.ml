(* Tests for Action module: runtime execution during struct parsing.

   Actions execute after a field is successfully parsed. The semantics follow
   EverParse 3D:
   - Assign: bind a name in the context (visible to later fields/actions)
   - Return true: validation succeeds, parsing continues
   - Return false: validation fails (Constraint_failed)
   - Abort: validation fails (Constraint_failed)
   - Var: local variable binding (same as Assign for OCaml-side execution)
   - If: conditional execution *)

open Wire
open Wire.C

(* ── Assign ── *)

let test_assign_propagates () =
  (* Field "x" is parsed, action assigns x to "out", field "y" has a
     constraint that references "out". If assign propagates correctly,
     the constraint on y (out + y <= 255) is evaluated with out = x. *)
  let s =
    struct_ "AssignProp"
      [
        field "x"
          ~action:(Action.on_success [ Action.assign "out" (field_ref "x") ])
          uint8;
        field "y"
          ~constraint_:Expr.(field_ref "out" + field_ref "y" <= int 255)
          uint8;
      ]
  in
  (* x=10, y=20 => out=10, 10+20=30 <= 255: OK *)
  let input = "\x0A\x14" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_assign_propagates_fail () =
  let s =
    struct_ "AssignPropFail"
      [
        field "x"
          ~action:(Action.on_success [ Action.assign "out" (field_ref "x") ])
          uint8;
        field "y"
          ~constraint_:Expr.(field_ref "out" + field_ref "y" <= int 10)
          uint8;
      ]
  in
  (* x=100, y=200 => out=100, 100+200=300 > 10: FAIL *)
  let input = "\x64\xC8" in
  match decode_string (struct_typ s) input with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_assign_expr () =
  (* Action assigns computed expression: out = x + 1 *)
  let s =
    struct_ "AssignExpr"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.assign "out" Expr.(field_ref "x" + int 1) ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 43) uint8;
      ]
  in
  (* x=42 => out=43, constraint out=43: OK *)
  let input = "\x2A\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Return ── *)

let test_return_true () =
  let s =
    struct_ "RetTrue"
      [
        field "x"
          ~action:(Action.on_success [ Action.return_bool Expr.true_ ])
          uint8;
      ]
  in
  let input = "\x42" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_return_false () =
  let s =
    struct_ "RetFalse"
      [
        field "x"
          ~action:(Action.on_success [ Action.return_bool Expr.false_ ])
          uint8;
      ]
  in
  let input = "\x42" in
  match decode_string (struct_typ s) input with
  | Ok _ -> Alcotest.fail "expected failure from return false"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_return_bool_expr () =
  (* return (x > 10): succeeds when x=42 *)
  let s =
    struct_ "RetExprOk"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(field_ref "x" > int 10) ])
          uint8;
      ]
  in
  let input = "\x2A" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_return_bool_expr_fail () =
  (* return (x > 10): fails when x=5 *)
  let s =
    struct_ "RetExprFail"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(field_ref "x" > int 10) ])
          uint8;
      ]
  in
  let input = "\x05" in
  match decode_string (struct_typ s) input with
  | Ok _ -> Alcotest.fail "expected failure from return false"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Abort ── *)

let test_abort () =
  let s =
    struct_ "AbortTest"
      [ field "x" ~action:(Action.on_success [ Action.abort ]) uint8 ]
  in
  let input = "\x42" in
  match decode_string (struct_typ s) input with
  | Ok _ -> Alcotest.fail "expected abort"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Var ── *)

let test_var_local () =
  (* var tmp = x * 2; assign out = tmp; later field checks out *)
  let s =
    struct_ "VarLocal"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.var "tmp" Expr.(field_ref "x" * int 2);
                 Action.assign "out" (field_ref "tmp");
               ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 84) uint8;
      ]
  in
  (* x=42 => tmp=84, out=84, constraint out=84: OK *)
  let input = "\x2A\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── If ── *)

let test_if_true_branch () =
  (* if (x > 0) { out = 1 } => out should be 1 when x=42 *)
  let s =
    struct_ "IfTrue"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.assign "out" (int 0);
                 Action.if_
                   Expr.(field_ref "x" > int 0)
                   [ Action.assign "out" (int 1) ]
                   None;
               ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 1) uint8;
      ]
  in
  let input = "\x2A\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_if_false_branch () =
  (* if (x > 100) { out = 1 } => out stays 0 when x=5 *)
  let s =
    struct_ "IfFalse"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.assign "out" (int 0);
                 Action.if_
                   Expr.(field_ref "x" > int 100)
                   [ Action.assign "out" (int 1) ]
                   None;
               ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 0) uint8;
      ]
  in
  let input = "\x05\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_if_else () =
  (* if (x = 0) { out = 10 } else { out = 20 } *)
  let s =
    struct_ "IfElse"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.if_
                   Expr.(field_ref "x" = int 0)
                   [ Action.assign "out" (int 10) ]
                   (Some [ Action.assign "out" (int 20) ]);
               ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 20) uint8;
      ]
  in
  (* x=1 => else branch => out=20 *)
  let input = "\x01\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_if_abort_in_else () =
  (* if (x > 0) { out = x } else { abort } *)
  let s =
    struct_ "IfAbort"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.if_
                   Expr.(field_ref "x" > int 0)
                   [ Action.assign "out" (field_ref "x") ]
                   (Some [ Action.abort ]);
               ])
          uint8;
      ]
  in
  (* x=0 => else branch => abort *)
  let input = "\x00" in
  match decode_string (struct_typ s) input with
  | Ok _ -> Alcotest.fail "expected abort from else branch"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_if_nested () =
  (* if (x > 0) { if (x < 100) { out = 1 } else { out = 2 } } *)
  let s =
    struct_ "IfNested"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.assign "out" (int 0);
                 Action.if_
                   Expr.(field_ref "x" > int 0)
                   [
                     Action.if_
                       Expr.(field_ref "x" < int 100)
                       [ Action.assign "out" (int 1) ]
                       (Some [ Action.assign "out" (int 2) ]);
                   ]
                   None;
               ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 1) uint8;
      ]
  in
  (* x=50: 50>0 => true, 50<100 => true, out=1 *)
  let input = "\x32\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── On_act vs On_success ── *)

let test_on_act () =
  (* on_act should execute identically to on_success *)
  let s =
    struct_ "OnAct"
      [
        field "x"
          ~action:
            (Action.on_act [ Action.return_bool Expr.(field_ref "x" = int 42) ])
          uint8;
      ]
  in
  let input = "\x2A" in
  (match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "on_act x=42: %a" pp_parse_error e);
  let input_bad = "\x00" in
  match decode_string (struct_typ s) input_bad with
  | Ok _ -> Alcotest.fail "expected on_act failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Multiple statements sequencing ── *)

let test_stmt_sequencing () =
  (* var a = x; var b = a + 1; assign out = b * 2 *)
  let s =
    struct_ "Sequence"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.var "a" (field_ref "x");
                 Action.var "b" Expr.(field_ref "a" + int 1);
                 Action.assign "out" Expr.(field_ref "b" * int 2);
               ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 86) uint8;
      ]
  in
  (* x=42 => a=42, b=43, out=86 *)
  let input = "\x2A\x00" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_return_short_circuits () =
  (* return true; abort => should succeed (abort never reached) *)
  let s =
    struct_ "ShortCircuit"
      [
        field "x"
          ~action:
            (Action.on_success [ Action.return_bool Expr.true_; Action.abort ])
          uint8;
      ]
  in
  let input = "\x42" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_abort_short_circuits () =
  (* abort; assign out = 1 => should fail (assign never reached) *)
  let s =
    struct_ "AbortShort"
      [
        field "x"
          ~action:
            (Action.on_success [ Action.abort; Action.assign "out" (int 1) ])
          uint8;
        field "y" ~constraint_:Expr.(field_ref "out" = int 1) uint8;
      ]
  in
  let input = "\x42\x00" in
  match decode_string (struct_typ s) input with
  | Ok _ -> Alcotest.fail "expected abort"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Empty action ── *)

let test_empty_action () =
  let s = struct_ "Empty" [ field "x" ~action:(Action.on_success []) uint8 ] in
  let input = "\x42" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Action on bitfield ── *)

let test_action_on_bitfield () =
  let s =
    struct_ "BFAction"
      [
        field "x"
          ~action:(Action.on_success [ Action.assign "out" (field_ref "x") ])
          (bits ~width:4 U8);
        field "y"
          ~constraint_:Expr.(field_ref "out" <= int 15)
          (bits ~width:4 U8);
      ]
  in
  let input = "\xAB" in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Suite ── *)

let suite =
  ( "action",
    [
      (* assign *)
      Alcotest.test_case "assign propagates to later field" `Quick
        test_assign_propagates;
      Alcotest.test_case "assign propagates (fail)" `Quick
        test_assign_propagates_fail;
      Alcotest.test_case "assign with expression" `Quick test_assign_expr;
      (* return *)
      Alcotest.test_case "return true" `Quick test_return_true;
      Alcotest.test_case "return false" `Quick test_return_false;
      Alcotest.test_case "return bool expr (pass)" `Quick test_return_bool_expr;
      Alcotest.test_case "return bool expr (fail)" `Quick
        test_return_bool_expr_fail;
      (* abort *)
      Alcotest.test_case "abort" `Quick test_abort;
      (* var *)
      Alcotest.test_case "var local binding" `Quick test_var_local;
      (* if *)
      Alcotest.test_case "if true branch" `Quick test_if_true_branch;
      Alcotest.test_case "if false (no else)" `Quick test_if_false_branch;
      Alcotest.test_case "if/else" `Quick test_if_else;
      Alcotest.test_case "if/else abort" `Quick test_if_abort_in_else;
      Alcotest.test_case "if nested" `Quick test_if_nested;
      (* on_act *)
      Alcotest.test_case "on_act" `Quick test_on_act;
      (* sequencing *)
      Alcotest.test_case "stmt sequencing" `Quick test_stmt_sequencing;
      Alcotest.test_case "return short-circuits" `Quick
        test_return_short_circuits;
      Alcotest.test_case "abort short-circuits" `Quick test_abort_short_circuits;
      (* edge cases *)
      Alcotest.test_case "empty action" `Quick test_empty_action;
      Alcotest.test_case "action on bitfield" `Quick test_action_on_bitfield;
    ] )
