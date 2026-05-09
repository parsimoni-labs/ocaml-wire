(* Tests for Param module: typed parameter handles, runtime environments, and
   decode-time semantics. *)

open Wire
open Wire.Everparse.Raw

(* -- Param.input / Param.output / Param.decl -- *)

let test_input_spec () =
  let p = Param.input "limit" uint8 in
  let spec = Param.decl p in
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
  let spec = Param.decl p in
  let s = param_struct "T" [ spec ] [ field "x" uint8 ] in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains mutable" true
    (Re.execp (Re.compile (Re.str "mutable")) output);
  Alcotest.(check bool)
    "contains out" true
    (Re.execp (Re.compile (Re.str "out")) output)

(* -- Param.bind / Param.get / Param.env -- *)

let test_input_binding () =
  let p = Param.input "limit" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x" ~constraint_:Expr.(Field.ref f_x <= Param.expr p) uint8
  in
  let c = Codec.v "InputBinding" (fun x -> x) Codec.[ (cf_x $ fun r -> r) ] in
  let env = Codec.env c |> Param.bind p 42 in
  Alcotest.(check int) "value" 42 (Param.get env p)

let test_output_binding () =
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
      uint8
  in
  let c = Codec.v "OutputBinding" (fun x -> x) Codec.[ (cf_x $ fun r -> r) ] in
  let env = Codec.env c in
  (* output param starts at 0 *)
  Alcotest.(check int) "initial value" 0 (Param.get env out);
  (* after decode, output param is written by the action *)
  let buf = Bytes.of_string "\x07" in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.(check int) "after decode" 7 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Input param visible to constraints (via Codec) -- *)

type bounded_record = { x : int }

let test_input_param_constraint () =
  let limit = Param.input "limit" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x" ~constraint_:Expr.(Field.ref f_x <= Param.expr limit) uint8
  in
  let c = Codec.v "Bounded" (fun x -> { x }) Codec.[ (cf_x $ fun r -> r.x) ] in
  (* limit=10, x=5: passes *)
  let buf = Bytes.of_string "\x05" in
  let env = Codec.env c |> Param.bind limit 10 in
  (match Codec.decode ~env c buf 0 with
  | Ok r -> Alcotest.(check int) "x" 5 r.x
  | Error e -> Alcotest.failf "pass: %a" pp_parse_error e);
  (* limit=3, x=5: fails *)
  let env = Codec.env c |> Param.bind limit 3 in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Output param written by action (via Codec) -- *)

let test_output_param_action () =
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
      uint8
  in
  let c = Codec.v "Writer" (fun x -> { x }) Codec.[ (cf_x $ fun r -> r.x) ] in
  let buf = Bytes.of_string "\x2A" in
  let env = Codec.env c in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 42 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_output_param_computed () =
  let out = Param.output "out" uint16be in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:
        (Action.on_success [ Action.assign out Expr.(Field.ref f_x * int 2) ])
      uint8
  in
  let c = Codec.v "Computed" (fun x -> { x }) Codec.[ (cf_x $ fun r -> r.x) ] in
  let buf = Bytes.of_string "\x15" in
  let env = Codec.env c in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.(check int) "out" 42 (Param.get env out)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Where clause with params (via Codec) -- *)

type bounded_value = { bv_value : int }

let test_where_clause_pass () =
  let max_val = Param.input "max_val" uint16be in
  let f_value = Field.v "value" uint16be in
  let cf_value = Field.v "value" uint16be in
  let c =
    Codec.v "Bounded"
      ~where:Expr.(Field.ref f_value <= Param.expr max_val)
      (fun value -> { bv_value = value })
      Codec.[ (cf_value $ fun r -> r.bv_value) ]
  in
  (* max_val=100, value=50: passes *)
  let buf = Bytes.of_string "\x00\x32" in
  let env = Codec.env c |> Param.bind max_val 100 in
  match Codec.decode ~env c buf 0 with
  | Ok r -> Alcotest.(check int) "value" 50 r.bv_value
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_where_clause_fail () =
  let max_val = Param.input "max_val" uint16be in
  let f_value = Field.v "value" uint16be in
  let cf_value = Field.v "value" uint16be in
  let c =
    Codec.v "Bounded"
      ~where:Expr.(Field.ref f_value <= Param.expr max_val)
      (fun value -> { bv_value = value })
      Codec.[ (cf_value $ fun r -> r.bv_value) ]
  in
  (* max_val=10, value=50: where clause fails *)
  let buf = Bytes.of_string "\x00\x32" in
  let env = Codec.env c |> Param.bind max_val 10 in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Mixed input + output params (via Codec) -- *)

type mixed_record = { a : int; b : int }

let test_mixed_params () =
  let max_val = Param.input "max_val" uint8 in
  let out_sum = Param.output "out_sum" uint8 in
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint8 in
  let cf_a =
    Field.v "a"
      ~action:(Action.on_success [ Action.assign out_sum (Field.ref f_a) ])
      uint8
  in
  let cf_b =
    Field.v "b"
      ~action:
        (Action.on_success
           [ Action.assign out_sum Expr.(Param.expr out_sum + Field.ref f_b) ])
      uint8
  in
  let c =
    Codec.v "Mixed"
      ~where:Expr.(Param.expr out_sum <= Param.expr max_val)
      (fun a b -> { a; b })
      Codec.[ (cf_a $ fun r -> r.a); (cf_b $ fun r -> r.b) ]
  in
  (* a=10, b=20 => out_sum=30, max_val=50 => 30 <= 50: OK *)
  let buf = Bytes.of_string "\x0A\x14" in
  let env = Codec.env c |> Param.bind max_val 50 in
  (match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.(check int) "out_sum" 30 (Param.get env out_sum)
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  (* a=10, b=20 => out_sum=30, max_val=20 => 30 > 20: FAIL *)
  let env = Codec.env c |> Param.bind max_val 20 in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- Codec.decode ~env:params with -- *)

type record_with_param = { x : int }

let test_codec_param_decode () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign outx (Field.ref f_x) ])
      uint8
  in
  let c =
    Codec.v "ParamCodec"
      ~where:Expr.(Field.ref f_x <= Param.expr limit)
      (fun x -> { x })
      Codec.[ (cf_x $ fun r -> r.x) ]
  in
  let buf = Bytes.of_string "\x05" in
  let env = Codec.env c |> Param.bind limit 10 in
  let v =
    match Codec.decode ~env c buf 0 with
    | Ok v -> v
    | Error e -> Alcotest.failf "%a" pp_parse_error e
  in
  Alcotest.(check int) "x" 5 v.x

let test_codec_param_where_fail () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign outx (Field.ref f_x) ])
      uint8
  in
  let c =
    Codec.v "ParamCodecFail"
      ~where:Expr.(Field.ref f_x <= Param.expr limit)
      (fun x -> { x })
      Codec.[ (cf_x $ fun r -> r.x) ]
  in
  let buf = Bytes.of_string "\x05" in
  let env = Codec.env c |> Param.bind limit 3 in
  match Codec.decode ~env c buf 0 with
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

(* -- 3D rendering -- *)

let test_3d_rendering () =
  let limit = Param.input "limit" uint16be in
  let out = Param.output "out" uint32be in
  let f_x = Field.v "x" uint16be in
  let f_limit = Field.v "limit" uint16be in
  let s =
    param_struct "Rendered"
      [ Param.decl limit; Param.decl out ]
      ~where:Expr.(Field.ref f_x <= Field.ref f_limit)
      [
        field "x"
          ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
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
  (* The struct-level [where] referencing fields is lowered to a field-level
     [{ ... }] constraint -- 3D's struct [where] only sees parameters. *)
  Alcotest.(check bool)
    "contains lowered where expression" true
    (Re.execp (Re.compile (Re.str "x <= limit")) output);
  Alcotest.(check bool)
    "contains on-success" true
    (Re.execp (Re.compile (Re.str ":on-success")) output);
  Alcotest.(check bool)
    "contains entrypoint" true
    (Re.execp (Re.compile (Re.str "entrypoint")) output)

(* -- No params (default behavior) -- *)

let test_no_params () =
  let s = struct_ "Simple" [ field "x" uint8 ] in
  match of_string (struct_typ s) "\x42" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Param-driven size -- *)

let decode_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

type param_size_record = { ps_data : string; ps_tag : int }

let ps_size_param = Param.input "data_size" uint8

let param_size_codec =
  Codec.v "ParamSize"
    (fun data tag -> { ps_data = data; ps_tag = tag })
    Codec.
      [
        ( Field.v "Data" (byte_array ~size:(Param.expr ps_size_param)) $ fun r ->
          r.ps_data );
        (Field.v "Tag" uint8 $ fun r -> r.ps_tag);
      ]

let test_param_size_decode () =
  let env = Codec.env param_size_codec |> Param.bind ps_size_param 4 in
  let buf = Bytes.create 5 in
  Bytes.blit_string "ABCD" 0 buf 0 4;
  Bytes.set_uint8 buf 4 0xFF;
  let r = decode_ok (Codec.decode ~env param_size_codec buf 0) in
  Alcotest.(check string) "data" "ABCD" r.ps_data;
  Alcotest.(check int) "tag" 0xFF r.ps_tag

let test_param_size_different_sizes () =
  (* Same codec, different param values *)
  let env2 = Codec.env param_size_codec |> Param.bind ps_size_param 2 in
  let buf2 = Bytes.create 3 in
  Bytes.blit_string "XY" 0 buf2 0 2;
  Bytes.set_uint8 buf2 2 0xAA;
  let r2 = decode_ok (Codec.decode ~env:env2 param_size_codec buf2 0) in
  Alcotest.(check string) "data 2" "XY" r2.ps_data;
  Alcotest.(check int) "tag 2" 0xAA r2.ps_tag;
  let env8 = Codec.env param_size_codec |> Param.bind ps_size_param 8 in
  let buf8 = Bytes.create 9 in
  Bytes.blit_string "12345678" 0 buf8 0 8;
  Bytes.set_uint8 buf8 8 0xBB;
  let r8 = decode_ok (Codec.decode ~env:env8 param_size_codec buf8 0) in
  Alcotest.(check string) "data 8" "12345678" r8.ps_data;
  Alcotest.(check int) "tag 8" 0xBB r8.ps_tag

let test_param_size_zero () =
  let env = Codec.env param_size_codec |> Param.bind ps_size_param 0 in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xFF;
  let r = decode_ok (Codec.decode ~env param_size_codec buf 0) in
  Alcotest.(check string) "data" "" r.ps_data;
  Alcotest.(check int) "tag" 0xFF r.ps_tag

(* -- Suite -- *)

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
      (* param-driven size *)
      Alcotest.test_case "param: size decode" `Quick test_param_size_decode;
      Alcotest.test_case "param: different sizes" `Quick
        test_param_size_different_sizes;
      Alcotest.test_case "param: size zero" `Quick test_param_size_zero;
    ] )
