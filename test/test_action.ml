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

(* Record type for two-field codecs *)
type xy = { x : int; y : int }

(* -- Assign -- *)

let test_assign_propagates () =
  (* Field "x" is parsed, action assigns x to "out". After decode, out = x. *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "AssignProp"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  (* x=10, y=20 => out=10 *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x0A\x14" in
  match Codec.decode ~env codec buf 0 with
  | Ok r ->
      Alcotest.(check int) "x" 10 r.x;
      Alcotest.(check int) "out" 10 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_assign_propagates_fail () =
  (* Assign out = x, then constraint out + y <= 10 fails. *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "AssignPropFail"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
            uint8
        $ fun r -> r.x );
        ( Field.v "y"
            ~constraint_:Expr.(Param.expr out + Field.ref f_y <= int 10)
            uint8
        $ fun r -> r.y );
      ]
  in
  (* x=100, y=200 => out=100, 100+200=300 > 10: FAIL *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x64\xC8" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_assign_expr () =
  (* Action assigns computed expression: out = x + 1 *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "AssignExpr"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [ Action.assign out Expr.(Field.ref f_x + int 1) ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  (* x=42 => out=43 *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x2A\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 43 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Return -- *)

let test_return_true () =
  let codec =
    let open Codec in
    v "RetTrue" Fun.id
      [
        Field.v "x"
          ~action:(Action.on_success [ Action.return_bool Expr.true_ ])
          uint8
        $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_return_false () =
  let codec =
    let open Codec in
    v "RetFalse" Fun.id
      [
        Field.v "x"
          ~action:(Action.on_success [ Action.return_bool Expr.false_ ])
          uint8
        $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> Alcotest.fail "expected failure from return false"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_return_bool_expr () =
  (* return (x > 10): succeeds when x=42 *)
  let f_x = Field.v "x" uint8 in
  let codec =
    let open Codec in
    v "RetExprOk" Fun.id
      [
        Field.v "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(Field.ref f_x > int 10) ])
          uint8
        $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x2A" in
  match Codec.decode codec buf 0 with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_return_bool_expr_fail () =
  (* return (x > 10): fails when x=5 *)
  let f_x = Field.v "x" uint8 in
  let codec =
    let open Codec in
    v "RetExprFail" Fun.id
      [
        Field.v "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(Field.ref f_x > int 10) ])
          uint8
        $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x05" in
  match Codec.decode codec buf 0 with
  | Ok _ -> Alcotest.fail "expected failure from return false"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Abort -- *)

let test_abort () =
  let codec =
    let open Codec in
    v "AbortTest" Fun.id
      [
        Field.v "x" ~action:(Action.on_success [ Action.abort ]) uint8 $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> Alcotest.fail "expected abort"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Var -- *)

let test_var_local () =
  (* var tmp = x * 2; assign out = tmp; check out after decode *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_tmp = Field.v "tmp" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "VarLocal"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [
                   Action.var "tmp" Expr.(Field.ref f_x * int 2);
                   Action.assign out (Field.ref f_tmp);
                 ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  (* x=42 => tmp=84, out=84 *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x2A\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 84 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- If -- *)

let test_if_true_branch () =
  (* if (x > 0) { out = 1 } => out should be 1 when x=42 *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "IfTrue"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [
                   Action.assign out (int 0);
                   Action.if_
                     Expr.(Field.ref f_x > int 0)
                     [ Action.assign out (int 1) ]
                     None;
                 ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x2A\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 1 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_if_false_branch () =
  (* if (x > 100) { out = 1 } => out stays 0 when x=5 *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "IfFalse"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [
                   Action.assign out (int 0);
                   Action.if_
                     Expr.(Field.ref f_x > int 100)
                     [ Action.assign out (int 1) ]
                     None;
                 ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x05\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 0 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_if_else () =
  (* if (x = 0) { out = 10 } else { out = 20 } *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "IfElse"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [
                   Action.if_
                     Expr.(Field.ref f_x = int 0)
                     [ Action.assign out (int 10) ]
                     (Some [ Action.assign out (int 20) ]);
                 ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  (* x=1 => else branch => out=20 *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x01\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 20 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_if_abort_in_else () =
  (* if (x > 0) { out = x } else { abort } *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let codec =
    let open Codec in
    v "IfAbort" Fun.id
      [
        Field.v "x"
          ~action:
            (Action.on_success
               [
                 Action.if_
                   Expr.(Field.ref f_x > int 0)
                   [ Action.assign out (Field.ref f_x) ]
                   (Some [ Action.abort ]);
               ])
          uint8
        $ Fun.id;
      ]
  in
  (* x=0 => else branch => abort *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.fail "expected abort from else branch"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_if_nested () =
  (* if (x > 0) { if (x < 100) { out = 1 } else { out = 2 } } *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "IfNested"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [
                   Action.assign out (int 0);
                   Action.if_
                     Expr.(Field.ref f_x > int 0)
                     [
                       Action.if_
                         Expr.(Field.ref f_x < int 100)
                         [ Action.assign out (int 1) ]
                         (Some [ Action.assign out (int 2) ]);
                     ]
                     None;
                 ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  (* x=50: 50>0 => true, 50<100 => true, out=1 *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x32\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 1 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- On_act vs On_success -- *)

let test_on_act () =
  (* on_act should execute identically to on_success *)
  let f_x = Field.v "x" uint8 in
  let codec =
    let open Codec in
    v "OnAct" Fun.id
      [
        Field.v "x"
          ~action:
            (Action.on_act [ Action.return_bool Expr.(Field.ref f_x = int 42) ])
          uint8
        $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x2A" in
  (match Codec.decode codec buf 0 with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "on_act x=42: %a" pp_parse_error e);
  let buf_bad = Bytes.of_string "\x00" in
  match Codec.decode codec buf_bad 0 with
  | Ok _ -> Alcotest.fail "expected on_act failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Multiple statements sequencing -- *)

let test_stmt_sequencing () =
  (* var a = x; var b = a + 1; assign out = b * 2 *)
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "Sequence"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success
                 [
                   Action.var "a" (Field.ref f_x);
                   Action.var "b" Expr.(Field.ref f_a + int 1);
                   Action.assign out Expr.(Field.ref f_b * int 2);
                 ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  (* x=42 => a=42, b=43, out=86 *)
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x2A\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 86 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_return_short_circuits () =
  (* return true; abort => should succeed (abort never reached) *)
  let codec =
    let open Codec in
    v "ShortCircuit" Fun.id
      [
        Field.v "x"
          ~action:
            (Action.on_success [ Action.return_bool Expr.true_; Action.abort ])
          uint8
        $ Fun.id;
      ]
  in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_abort_short_circuits () =
  (* abort; assign out = 1 => should fail (assign never reached) *)
  let out = Param.output "out" uint8 in
  let f_y = Field.v "y" uint8 in
  let codec =
    let open Codec in
    v "AbortShort"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:
              (Action.on_success [ Action.abort; Action.assign out (int 1) ])
            uint8
        $ fun r -> r.x );
        (f_y $ fun r -> r.y);
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42\x00" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ -> Alcotest.fail "expected abort"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Empty action -- *)

let test_empty_action () =
  let codec =
    let open Codec in
    v "Empty" Fun.id
      [ Field.v "x" ~action:(Action.on_success []) uint8 $ Fun.id ]
  in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Action on bitfield -- *)

let test_action_on_bitfield () =
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let codec =
    let open Codec in
    v "BFAction"
      (fun x y -> { x; y })
      [
        ( Field.v "x"
            ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
            (bits ~width:4 U8)
        $ fun r -> r.x );
        (Field.v "y" (bits ~width:4 U8) $ fun r -> r.y);
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\xAB" in
  match Codec.decode ~env codec buf 0 with
  | Ok _ ->
      let out_val = Param.get env out in
      Alcotest.(check bool) "out <= 15" true (out_val <= 15)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Suite -- *)

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
