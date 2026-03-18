(* Tests for Param module: typed parameter handles, runtime environments, and
   decode-time semantics. *)

open Wire
open Wire.C

(* ── Param.input / Param.output / Param.v ── *)

let test_input_spec () =
  let p = Param.input "limit" uint8 in
  let spec = Param.v p in
  let s = param_struct "T" [ spec ] [ field "x" uint8 ] in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT8 limit" true
    (Re.execp (Re.compile (Re.str "UINT8 limit")) output);
  Alcotest.(check bool)
    "not mutable" false
    (Re.execp (Re.compile (Re.str "mutable")) output)

let test_output_spec () =
  let p = Param.output "out" uint16be in
  let spec = Param.v p in
  let s = param_struct "T" [ spec ] [ field "x" uint8 ] in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains mutable" true
    (Re.execp (Re.compile (Re.str "mutable")) output);
  Alcotest.(check bool)
    "contains out" true
    (Re.execp (Re.compile (Re.str "out")) output)

(* ── Param.bind / Param.get ── *)

let test_input_binding () =
  let p = Param.input "limit" uint8 in
  let env = Param.bind Param.empty p 42 in
  Alcotest.(check int) "value" 42 (Param.get env p)

let test_output_binding () =
  let p = Param.output "out" uint8 in
  let env = Param.init Param.empty p 7 in
  Alcotest.(check int) "initial value" 7 (Param.get env p)

(* ── Input param visible to constraints ── *)

let test_input_param_constraint () =
  let limit = Param.input "limit" uint8 in
  let s =
    param_struct "Bounded"
      [ Param.v limit ]
      [
        field "x"
          ~constraint_:Expr.(Wire.field_ref "x" <= Wire.field_ref "limit")
          uint8;
      ]
  in
  let params = Param.bind Param.empty limit 10 in
  (* limit=10, x=5: passes *)
  (match decode_string ~env:params (struct_typ s) "\x05" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "pass: %a" pp_parse_error e);
  (* limit=3, x=5: fails *)
  let params = Param.bind Param.empty limit 3 in
  match decode_string ~env:params (struct_typ s) "\x05" with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Output param written by action ── *)

let test_output_param_action () =
  let out = Param.output "out" uint8 in
  let s =
    param_struct "Writer"
      [ Param.v out ]
      [
        field "x"
          ~action:(Action.on_success [ Action.assign out (Wire.field_ref "x") ])
          uint8;
      ]
  in
  let params = Param.init Param.empty out 0 in
  match decode_string ~env:params (struct_typ s) "\x2A" with
  | Ok () -> Alcotest.(check int) "out" 42 (Param.get params out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_output_param_computed () =
  let out = Param.output "out" uint16be in
  let s =
    param_struct "Computed"
      [ Param.v out ]
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.assign out Expr.(Wire.field_ref "x" * int 2) ])
          uint8;
      ]
  in
  let params = Param.init Param.empty out 0 in
  match decode_string ~env:params (struct_typ s) "\x15" with
  | Ok () -> Alcotest.(check int) "out" 42 (Param.get params out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Where clause with params ── *)

let test_where_clause_pass () =
  let max_len = Param.input "max_len" uint16be in
  let s =
    param_struct "Bounded"
      [ Param.v max_len ]
      ~where:Expr.(Wire.field_ref "len" <= Wire.field_ref "max_len")
      [
        field "len" uint16be;
        field "data" (byte_array ~size:(Wire.field_ref "len"));
      ]
  in
  (* max_len=5, len=3, data="abc" *)
  let params = Param.bind Param.empty max_len 5 in
  match decode_string ~env:params (struct_typ s) "\x00\x03abc" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_where_clause_fail () =
  let max_len = Param.input "max_len" uint16be in
  let s =
    param_struct "Bounded"
      [ Param.v max_len ]
      ~where:Expr.(Wire.field_ref "len" <= Wire.field_ref "max_len")
      [
        field "len" uint16be;
        field "data" (byte_array ~size:(Wire.field_ref "len"));
      ]
  in
  (* max_len=2, len=3: where clause fails *)
  let params = Param.bind Param.empty max_len 2 in
  match decode_string ~env:params (struct_typ s) "\x00\x03abc" with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Mixed input + output params ── *)

let test_mixed_params () =
  let max_val = Param.input "max_val" uint8 in
  let out_sum = Param.output "out_sum" uint8 in
  let s =
    param_struct "Mixed"
      [ Param.v max_val; Param.v out_sum ]
      ~where:Expr.(Wire.field_ref "out_sum" <= Wire.field_ref "max_val")
      [
        field "a"
          ~action:
            (Action.on_success [ Action.assign out_sum (Wire.field_ref "a") ])
          uint8;
        field "b"
          ~action:
            (Action.on_success
               [
                 Action.assign out_sum
                   Expr.(Wire.field_ref "out_sum" + Wire.field_ref "b");
               ])
          uint8;
      ]
  in
  (* a=10, b=20 => out_sum=30, max_val=50 => 30 <= 50: OK *)
  let params =
    Param.empty |> fun env ->
    Param.bind env max_val 50 |> fun env -> Param.init env out_sum 0
  in
  (match decode_string ~env:params (struct_typ s) "\x0A\x14" with
  | Ok () -> Alcotest.(check int) "out_sum" 30 (Param.get params out_sum)
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  (* a=10, b=20 => out_sum=30, max_val=20 => 30 > 20: FAIL *)
  let params =
    Param.empty |> fun env ->
    Param.bind env max_val 20 |> fun env -> Param.init env out_sum 0
  in
  match decode_string ~env:params (struct_typ s) "\x0A\x14" with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Codec.decode with params ── *)

type record_with_param = { x : int }

let param_codec =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  Codec.view "ParamCodec"
    ~params:[ Param.v limit; Param.v outx ]
    ~where:Expr.(Wire.field_ref "x" <= Wire.field_ref "limit")
    (fun x -> { x })
    Codec.
      [
        Codec.field "x"
          ~action:
            (Action.on_success [ Action.assign outx (Wire.field_ref "x") ])
          uint8
          (fun r -> r.x);
      ]

let test_codec_param_decode () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let buf = Bytes.of_string "\x05" in
  let params =
    Param.empty |> fun env ->
    Param.bind env limit 10 |> fun env -> Param.init env outx 0
  in
  let v =
    match Codec.decode ~env:params param_codec buf 0 with
    | Ok v -> v
    | Error e -> Alcotest.failf "%a" pp_parse_error e
  in
  Alcotest.(check int) "x" 5 v.x;
  Alcotest.(check int) "outx" 5 (Param.get params outx)

let test_codec_param_where_fail () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let buf = Bytes.of_string "\x05" in
  let params =
    Param.empty |> fun env ->
    Param.bind env limit 3 |> fun env -> Param.init env outx 0
  in
  match Codec.decode ~env:params param_codec buf 0 with
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

(* ── 3D rendering ── *)

let test_3d_rendering () =
  let limit = Param.input "limit" uint16be in
  let out = Param.output "out" uint32be in
  let s =
    param_struct "Rendered"
      [ Param.v limit; Param.v out ]
      ~where:Expr.(Wire.field_ref "x" <= Wire.field_ref "limit")
      [
        field "x"
          ~action:(Action.on_success [ Action.assign out (Wire.field_ref "x") ])
          uint16be;
      ]
  in
  let m = module_ [ typedef ~entrypoint:true s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT16BE limit" true
    (Re.execp (Re.compile (Re.str "UINT16BE limit")) output);
  Alcotest.(check bool)
    "contains mutable UINT32BE *out" true
    (Re.execp (Re.compile (Re.str "mutable")) output);
  Alcotest.(check bool)
    "contains where" true
    (Re.execp (Re.compile (Re.str "where")) output);
  Alcotest.(check bool)
    "contains on-success" true
    (Re.execp (Re.compile (Re.str ":on-success")) output);
  Alcotest.(check bool)
    "contains entrypoint" true
    (Re.execp (Re.compile (Re.str "entrypoint")) output)

(* ── No params (default behavior) ── *)

let test_no_params () =
  let s = struct_ "Simple" [ field "x" uint8 ] in
  match decode_string (struct_typ s) "\x42" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Suite ── *)

let suite =
  ( "param",
    [
      (* construction *)
      Alcotest.test_case "input spec" `Quick test_input_spec;
      Alcotest.test_case "output spec" `Quick test_output_spec;
      (* bindings *)
      Alcotest.test_case "input binding" `Quick test_input_binding;
      Alcotest.test_case "output binding" `Quick test_output_binding;
      (* runtime: input params *)
      Alcotest.test_case "input param constraint" `Quick
        test_input_param_constraint;
      (* runtime: output params *)
      Alcotest.test_case "output param action" `Quick test_output_param_action;
      Alcotest.test_case "output param computed" `Quick
        test_output_param_computed;
      (* where clause *)
      Alcotest.test_case "where clause pass" `Quick test_where_clause_pass;
      Alcotest.test_case "where clause fail" `Quick test_where_clause_fail;
      (* mixed *)
      Alcotest.test_case "mixed input + output" `Quick test_mixed_params;
      (* codec *)
      Alcotest.test_case "codec decode with params" `Quick
        test_codec_param_decode;
      Alcotest.test_case "codec where fail" `Quick test_codec_param_where_fail;
      (* 3D rendering *)
      Alcotest.test_case "3D rendering" `Quick test_3d_rendering;
      (* default *)
      Alcotest.test_case "no params" `Quick test_no_params;
    ] )
