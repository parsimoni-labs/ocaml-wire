(* Test d3t library *)

open D3t

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Helper: encode record to string using Codec API *)
let encode_record_to_string codec v =
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let decode_record_from_string codec s =
  let ws = Codec.wire_size codec in
  if String.length s < ws then
    Error (Unexpected_eof { expected = ws; got = String.length s })
  else Ok (Codec.decode codec (Bytes.of_string s) 0)

let test_bitfields () =
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 bf_uint32);
        field "y"
          ~constraint_:Expr.(ref "y" <= int 900)
          (bits ~width:10 bf_uint32);
        field "z"
          ~constraint_:Expr.(ref "y" + ref "z" <= int 60000)
          (bits ~width:16 bf_uint32);
      ]
  in
  let m = module_ "Bitfields" [ typedef bf ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains BF" true (contains ~sub:"BF" output)

let test_enumerations () =
  let m =
    module_ "Enumerations"
      [
        enum_decl "Enum8"
          [ ("Enum8_1", 0); ("Enum8_2", 1); ("Enum8_3", 2) ]
          uint8;
      ]
  in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains enum" true (contains ~sub:"enum" output);
  Alcotest.(check bool) "contains Enum8_1" true (contains ~sub:"Enum8_1" output)

let test_field_dependence () =
  let t_struct = param_struct "t" [ param "a" uint32 ] [ field "x" uint32 ] in
  let s_struct =
    struct_ "s"
      [ field "a" uint32; field "b" (apply (type_ref "t") [ ref "a" ]) ]
  in
  let m = module_ "FieldDependence" [ typedef t_struct; typedef s_struct ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains param" true (contains ~sub:"UINT32 a" output)

let test_casetype () =
  let d_casetype =
    casetype_decl "_D"
      [ param "key" uint32 ]
      uint32
      [ decl_case 1 uint16; decl_case 2 uint32 ]
  in
  let m = module_ "Casetype" [ d_casetype ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool)
    "contains casetype" true
    (contains ~sub:"casetype" output);
  Alcotest.(check bool)
    "contains switch" true
    (contains ~sub:"switch (key)" output);
  (* Public name should not have underscore *)
  Alcotest.(check bool) "public name is D" true (contains ~sub:"} D;" output)

let test_pretty_print () =
  let simple =
    struct_ "Simple" [ field "a" uint8; field "b" uint16be; field "c" uint32 ]
  in
  let m = module_ "Simple" [ typedef simple ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains typedef" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output)

(* Parsing tests *)

let test_parse_uint8 () =
  let input = "\x42" in
  match parse_string uint8 input with
  | Ok v -> Alcotest.(check int) "uint8 value" 0x42 v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_uint16_le () =
  let input = "\x01\x02" in
  match parse_string uint16 input with
  | Ok v -> Alcotest.(check int) "uint16 le value" 0x0201 v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_uint16_be () =
  let input = "\x01\x02" in
  match parse_string uint16be input with
  | Ok v -> Alcotest.(check int) "uint16 be value" 0x0102 v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_uint32_le () =
  let input = "\x01\x02\x03\x04" in
  match parse_string uint32 input with
  | Ok v -> Alcotest.(check int32) "uint32 le value" 0x04030201l v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_uint32_be () =
  let input = "\x01\x02\x03\x04" in
  match parse_string uint32be input with
  | Ok v -> Alcotest.(check int32) "uint32 be value" 0x01020304l v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_uint64_le () =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match parse_string uint64 input with
  | Ok v -> Alcotest.(check int64) "uint64 le value" 0x0807060504030201L v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_array () =
  let input = "\x01\x02\x03" in
  let t = array ~len:(int 3) uint8 in
  match parse_string t input with
  | Ok v -> Alcotest.(check (list int)) "array values" [ 1; 2; 3 ] v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_byte_array () =
  let input = "hello" in
  let t = byte_array ~size:(int 5) in
  match parse_string t input with
  | Ok v -> Alcotest.(check string) "byte_array value" "hello" v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_enum_valid () =
  let input = "\x01" in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  match parse_string t input with
  | Ok v -> Alcotest.(check int) "enum value" 1 v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_enum_invalid () =
  let input = "\xFF" in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  match parse_string t input with
  | Ok _ -> Alcotest.fail "expected error for invalid enum"
  | Error (Invalid_enum { value; _ }) ->
      Alcotest.(check int) "invalid enum value" 255 value
  | Error e ->
      Alcotest.fail (Format.asprintf "wrong error: %a" pp_parse_error e)

let test_parse_all_bytes () =
  let input = "hello world" in
  match parse_string all_bytes input with
  | Ok v -> Alcotest.(check string) "all_bytes value" "hello world" v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_all_zeros_valid () =
  let input = "\x00\x00\x00" in
  match parse_string all_zeros input with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_all_zeros_invalid () =
  let input = "\x00\x01\x00" in
  match parse_string all_zeros input with
  | Ok _ -> Alcotest.fail "expected error for non-zero byte"
  | Error (All_zeros_failed { offset }) ->
      Alcotest.(check int) "non-zero offset" 1 offset
  | Error e ->
      Alcotest.fail (Format.asprintf "wrong error: %a" pp_parse_error e)

let test_parse_bitfield () =
  let input = "\xFF\xFF\xFF\xFF" in
  let t = bits ~width:6 bf_uint32 in
  match parse_string t input with
  | Ok v -> Alcotest.(check int) "bitfield value (6 bits)" 63 v
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_eof () =
  let input = "\x01" in
  match parse_string uint16 input with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error (Unexpected_eof { expected; got }) ->
      Alcotest.(check int) "expected bytes" 2 expected;
      Alcotest.(check int) "got bytes" 1 got
  | Error e ->
      Alcotest.fail (Format.asprintf "wrong error: %a" pp_parse_error e)

let test_parse_struct () =
  let input = "\x01\x02\x03" in
  let s =
    struct_ "Test" [ field "a" uint8; field "b" uint8; field "c" uint8 ]
  in
  let t = struct_typ s in
  match parse_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_struct_constraint () =
  (* Test struct with constraint that should pass *)
  let input = "\x0A" in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  match parse_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_parse_struct_constraint_fail () =
  (* Test struct with constraint that should fail *)
  let input = "\xFF" in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  match parse_string t input with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "wrong error: %a" pp_parse_error e)

(* Encoding tests *)

let test_encode_uint8 () =
  let encoded = encode_to_string uint8 0x42 in
  Alcotest.(check string) "uint8 encoding" "\x42" encoded

let test_encode_uint16_le () =
  let encoded = encode_to_string uint16 0x0201 in
  Alcotest.(check string) "uint16 le encoding" "\x01\x02" encoded

let test_encode_uint16_be () =
  let encoded = encode_to_string uint16be 0x0102 in
  Alcotest.(check string) "uint16 be encoding" "\x01\x02" encoded

let test_encode_uint32_le () =
  let encoded = encode_to_string uint32 0x04030201l in
  Alcotest.(check string) "uint32 le encoding" "\x01\x02\x03\x04" encoded

let test_encode_uint32_be () =
  let encoded = encode_to_string uint32be 0x01020304l in
  Alcotest.(check string) "uint32 be encoding" "\x01\x02\x03\x04" encoded

let test_encode_array () =
  let t = array ~len:(int 3) uint8 in
  let encoded = encode_to_string t [ 1; 2; 3 ] in
  Alcotest.(check string) "array encoding" "\x01\x02\x03" encoded

let test_encode_byte_array () =
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t "hello" in
  Alcotest.(check string) "byte_array encoding" "hello" encoded

let test_encode_enum () =
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  let encoded = encode_to_string t 1 in
  Alcotest.(check string) "enum encoding" "\x01" encoded

let test_encode_bitfield () =
  let t = bits ~width:6 bf_uint32 in
  let encoded = encode_to_string t 63 in
  (* 63 = 0x3F, but stored in 4 bytes as uint32 LE *)
  Alcotest.(check string) "bitfield encoding" "\x3F\x00\x00\x00" encoded

(* Roundtrip tests *)

let test_roundtrip_uint8 () =
  let original = 0x42 in
  let encoded = encode_to_string uint8 original in
  match parse_string uint8 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint8" original decoded
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_roundtrip_uint16 () =
  let original = 0x1234 in
  let encoded = encode_to_string uint16 original in
  match parse_string uint16 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint16" original decoded
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_roundtrip_uint32 () =
  let original = 0x12345678l in
  let encoded = encode_to_string uint32 original in
  match parse_string uint32 encoded with
  | Ok decoded -> Alcotest.(check int32) "roundtrip uint32" original decoded
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_roundtrip_array () =
  let original = [ 1; 2; 3; 4; 5 ] in
  let t = array ~len:(int 5) uint8 in
  let encoded = encode_to_string t original in
  match parse_string t encoded with
  | Ok decoded -> Alcotest.(check (list int)) "roundtrip array" original decoded
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_roundtrip_byte_array () =
  let original = "hello" in
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t original in
  match parse_string t encoded with
  | Ok decoded ->
      Alcotest.(check string) "roundtrip byte_array" original decoded
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

(* Record codec tests *)

type simple_record = { a : int; b : int; c : int32 }

let simple_record_codec =
  let open Codec in
  record "SimpleRecord" (fun a b c -> { a; b; c })
  |+ field "a" uint8 (fun r -> r.a)
  |+ field "b" uint16 (fun r -> r.b)
  |+ field "c" uint32 (fun r -> r.c)
  |> seal

let test_record_encode () =
  let v = { a = 0x42; b = 0x1234; c = 0x56789ABCl } in
  match encode_record_to_string simple_record_codec v with
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)
  | Ok encoded ->
      (* uint8 + uint16_le + uint32_le *)
      Alcotest.(check int) "length" 7 (String.length encoded);
      Alcotest.(check int) "byte 0 (a)" 0x42 (Char.code encoded.[0]);
      (* uint16 LE: 0x1234 -> 0x34, 0x12 *)
      Alcotest.(check int) "byte 1 (b low)" 0x34 (Char.code encoded.[1]);
      Alcotest.(check int) "byte 2 (b high)" 0x12 (Char.code encoded.[2])

let test_record_decode () =
  let input = "\x42\x34\x12\xBC\x9A\x78\x56" in
  match decode_record_from_string simple_record_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 0x42 v.a;
      Alcotest.(check int) "b" 0x1234 v.b;
      Alcotest.(check int32) "c" 0x56789ABCl v.c
  | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e)

let test_record_roundtrip () =
  let original = { a = 0xAB; b = 0xCDEF; c = 0x12345678l } in
  match encode_record_to_string simple_record_codec original with
  | Error e -> Alcotest.fail (Format.asprintf "encode: %a" pp_parse_error e)
  | Ok encoded -> (
      match decode_record_from_string simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a roundtrip" original.a decoded.a;
          Alcotest.(check int) "b roundtrip" original.b decoded.b;
          Alcotest.(check int32) "c roundtrip" original.c decoded.c
      | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e))

let test_record_to_struct () =
  let s = Codec.to_struct simple_record_codec in
  let m = module_ "SimpleRecord" [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a;" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b;" output);
  Alcotest.(check bool) "contains field c" true (contains ~sub:"c;" output)

(* Record with multiple uint16be fields *)
type multi_record = { x : int; y : int }

let multi_record_codec =
  let open Codec in
  record "MultiRecord" (fun x y -> { x; y })
  |+ field "x" uint16be (fun r -> r.x)
  |+ field "y" uint16be (fun r -> r.y)
  |> seal

let test_record_with_multi () =
  let original = { x = 0x1234; y = 0x5678 } in
  match encode_record_to_string multi_record_codec original with
  | Error e -> Alcotest.fail (Format.asprintf "encode: %a" pp_parse_error e)
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      match decode_record_from_string multi_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "x" original.x decoded.x;
          Alcotest.(check int) "y" original.y decoded.y
      | Error e -> Alcotest.fail (Format.asprintf "%a" pp_parse_error e))

(* FFI stub generation tests *)

let test_c_stubs () =
  let s =
    struct_ "SimpleHeader"
      [ field "version" uint8; field "length" uint16; field "flags" uint8 ]
  in
  let stubs = to_c_stubs [ s ] in
  Alcotest.(check bool)
    "contains read stub" true
    (contains ~sub:"caml_d3t_SimpleHeader_read" stubs);
  Alcotest.(check bool)
    "contains write stub" true
    (contains ~sub:"caml_d3t_SimpleHeader_write" stubs)

let suite =
  ( "d3t",
    [
      (* generation *)
      Alcotest.test_case "generation: bitfields" `Quick test_bitfields;
      Alcotest.test_case "generation: enumerations" `Quick test_enumerations;
      Alcotest.test_case "generation: field dependence" `Quick
        test_field_dependence;
      Alcotest.test_case "generation: casetype" `Quick test_casetype;
      Alcotest.test_case "generation: pretty print" `Quick test_pretty_print;
      (* parsing *)
      Alcotest.test_case "parse: uint8" `Quick test_parse_uint8;
      Alcotest.test_case "parse: uint16 le" `Quick test_parse_uint16_le;
      Alcotest.test_case "parse: uint16 be" `Quick test_parse_uint16_be;
      Alcotest.test_case "parse: uint32 le" `Quick test_parse_uint32_le;
      Alcotest.test_case "parse: uint32 be" `Quick test_parse_uint32_be;
      Alcotest.test_case "parse: uint64 le" `Quick test_parse_uint64_le;
      Alcotest.test_case "parse: array" `Quick test_parse_array;
      Alcotest.test_case "parse: byte_array" `Quick test_parse_byte_array;
      Alcotest.test_case "parse: enum valid" `Quick test_parse_enum_valid;
      Alcotest.test_case "parse: enum invalid" `Quick test_parse_enum_invalid;
      Alcotest.test_case "parse: all_bytes" `Quick test_parse_all_bytes;
      Alcotest.test_case "parse: all_zeros valid" `Quick
        test_parse_all_zeros_valid;
      Alcotest.test_case "parse: all_zeros invalid" `Quick
        test_parse_all_zeros_invalid;
      Alcotest.test_case "parse: bitfield" `Quick test_parse_bitfield;
      Alcotest.test_case "parse: eof error" `Quick test_parse_eof;
      Alcotest.test_case "parse: struct" `Quick test_parse_struct;
      Alcotest.test_case "parse: struct constraint" `Quick
        test_parse_struct_constraint;
      Alcotest.test_case "parse: struct constraint fail" `Quick
        test_parse_struct_constraint_fail;
      (* encoding *)
      Alcotest.test_case "encode: uint8" `Quick test_encode_uint8;
      Alcotest.test_case "encode: uint16 le" `Quick test_encode_uint16_le;
      Alcotest.test_case "encode: uint16 be" `Quick test_encode_uint16_be;
      Alcotest.test_case "encode: uint32 le" `Quick test_encode_uint32_le;
      Alcotest.test_case "encode: uint32 be" `Quick test_encode_uint32_be;
      Alcotest.test_case "encode: array" `Quick test_encode_array;
      Alcotest.test_case "encode: byte_array" `Quick test_encode_byte_array;
      Alcotest.test_case "encode: enum" `Quick test_encode_enum;
      Alcotest.test_case "encode: bitfield" `Quick test_encode_bitfield;
      (* roundtrip *)
      Alcotest.test_case "roundtrip: uint8" `Quick test_roundtrip_uint8;
      Alcotest.test_case "roundtrip: uint16" `Quick test_roundtrip_uint16;
      Alcotest.test_case "roundtrip: uint32" `Quick test_roundtrip_uint32;
      Alcotest.test_case "roundtrip: array" `Quick test_roundtrip_array;
      Alcotest.test_case "roundtrip: byte_array" `Quick
        test_roundtrip_byte_array;
      (* record *)
      Alcotest.test_case "record: encode" `Quick test_record_encode;
      Alcotest.test_case "record: decode" `Quick test_record_decode;
      Alcotest.test_case "record: roundtrip" `Quick test_record_roundtrip;
      Alcotest.test_case "record: to_struct" `Quick test_record_to_struct;
      Alcotest.test_case "record: with_multi" `Quick test_record_with_multi;
      (* ffi stubs *)
      Alcotest.test_case "ffi: c_stubs" `Quick test_c_stubs;
    ] )
