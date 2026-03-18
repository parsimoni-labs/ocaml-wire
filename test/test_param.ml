(* Tests for Param module: typed parameter handles, runtime environments, and
   decode-time semantics. *)

open Wire
open Wire.C

(* ── Param.input / Param.output / Param.v ── *)

let test_input_spec () =
  let p = Param.input "limit" uint8 0 in
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

(* ── Param.set / Param.get ── *)

let test_input_binding () =
  let p = Param.input "limit" uint8 0 in
  Param.set p 42;
  Alcotest.(check int) "value" 42 (Param.get p)

let test_output_binding () =
  let p = Param.output "out" uint8 in
  Param.set p 7;
  Alcotest.(check int) "initial value" 7 (Param.get p)

(* ── Input param visible to constraints (via Codec) ── *)

type bounded_record = { x : int }

let test_input_param_constraint () =
  let limit = Param.input "limit" uint8 10 in
  let c =
    Codec.view "Bounded" ~params:[ Param.Pack limit ]
      (fun x -> { x })
      Codec.
        [
          Codec.field "x"
            ~constraint_:Expr.(Wire.field_ref "x" <= Wire.field_ref "limit")
            uint8
            (fun r -> r.x);
        ]
  in
  (* limit=10, x=5: passes *)
  let buf = Bytes.of_string "\x05" in
  (match Codec.decode c buf 0 with
  | Ok r -> Alcotest.(check int) "x" 5 r.x
  | Error e -> Alcotest.failf "pass: %a" pp_parse_error e);
  (* limit=3, x=5: fails *)
  Param.set limit 3;
  match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Output param written by action (via Codec) ── *)

let test_output_param_action () =
  let out = Param.output "out" uint8 in
  let c =
    Codec.view "Writer" ~params:[ Param.Pack out ]
      (fun x -> { x })
      Codec.
        [
          Codec.field "x"
            ~action:
              (Action.on_success [ Action.assign out (Wire.field_ref "x") ])
            uint8
            (fun r -> r.x);
        ]
  in
  let buf = Bytes.of_string "\x2A" in
  match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 42 (Param.get out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_output_param_computed () =
  let out = Param.output "out" uint16be in
  let c =
    Codec.view "Computed" ~params:[ Param.Pack out ]
      (fun x -> { x })
      Codec.
        [
          Codec.field "x"
            ~action:
              (Action.on_success
                 [ Action.assign out Expr.(Wire.field_ref "x" * int 2) ])
            uint8
            (fun r -> r.x);
        ]
  in
  let buf = Bytes.of_string "\x15" in
  match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 42 (Param.get out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Where clause with params (via Codec) ── *)

type bounded_value = { bv_value : int }

let test_where_clause_pass () =
  let max_val = Param.input "max_val" uint16be 100 in
  let c =
    Codec.view "Bounded" ~params:[ Param.Pack max_val ]
      ~where:Expr.(Wire.field_ref "value" <= Wire.field_ref "max_val")
      (fun value -> { bv_value = value })
      Codec.[ Codec.field "value" uint16be (fun r -> r.bv_value) ]
  in
  (* max_val=100, value=50: passes *)
  let buf = Bytes.of_string "\x00\x32" in
  match Codec.decode c buf 0 with
  | Ok r -> Alcotest.(check int) "value" 50 r.bv_value
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_where_clause_fail () =
  let max_val = Param.input "max_val" uint16be 10 in
  let c =
    Codec.view "Bounded" ~params:[ Param.Pack max_val ]
      ~where:Expr.(Wire.field_ref "value" <= Wire.field_ref "max_val")
      (fun value -> { bv_value = value })
      Codec.[ Codec.field "value" uint16be (fun r -> r.bv_value) ]
  in
  (* max_val=10, value=50: where clause fails *)
  let buf = Bytes.of_string "\x00\x32" in
  match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Mixed input + output params (via Codec) ── *)

type mixed_record = { a : int; b : int }

let test_mixed_params () =
  let max_val = Param.input "max_val" uint8 50 in
  let out_sum = Param.output "out_sum" uint8 in
  let c =
    Codec.view "Mixed"
      ~params:[ Param.Pack max_val; Param.Pack out_sum ]
      ~where:Expr.(Wire.field_ref "out_sum" <= Wire.field_ref "max_val")
      (fun a b -> { a; b })
      Codec.
        [
          Codec.field "a"
            ~action:
              (Action.on_success [ Action.assign out_sum (Wire.field_ref "a") ])
            uint8
            (fun r -> r.a);
          Codec.field "b"
            ~action:
              (Action.on_success
                 [
                   Action.assign out_sum
                     Expr.(Wire.field_ref "out_sum" + Wire.field_ref "b");
                 ])
            uint8
            (fun r -> r.b);
        ]
  in
  (* a=10, b=20 => out_sum=30, max_val=50 => 30 <= 50: OK *)
  let buf = Bytes.of_string "\x0A\x14" in
  (match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.(check int) "out_sum" 30 (Param.get out_sum)
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  (* a=10, b=20 => out_sum=30, max_val=20 => 30 > 20: FAIL *)
  Param.set max_val 20;
  Param.set out_sum 0;
  match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Codec.decode with params ── *)

type record_with_param = { x : int }

let param_codec =
  let limit = Param.input "limit" uint8 10 in
  let outx = Param.output "outx" uint8 in
  Codec.view "ParamCodec"
    ~params:[ Param.Pack limit; Param.Pack outx ]
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
  let buf = Bytes.of_string "\x05" in
  let v =
    match Codec.decode param_codec buf 0 with
    | Ok v -> v
    | Error e -> Alcotest.failf "%a" pp_parse_error e
  in
  Alcotest.(check int) "x" 5 v.x

let test_codec_param_where_fail () =
  let limit = Param.input "limit" uint8 3 in
  let outx = Param.output "outx" uint8 in
  let c =
    Codec.view "ParamCodecFail"
      ~params:[ Param.Pack limit; Param.Pack outx ]
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
  in
  let buf = Bytes.of_string "\x05" in
  match Codec.decode c buf 0 with
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

(* ── 3D rendering ── *)

let test_3d_rendering () =
  let limit = Param.input "limit" uint16be 0 in
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
