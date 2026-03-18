(* Tests for types.ml: 3D generation, streaming parse/encode *)

open Wire
open Wire.C

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Helper: parse from a string delivered in slices of [chunk_size] bytes.
   Forces multi-byte values to straddle slice boundaries. *)
let parse_chunked ~chunk_size typ s =
  let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:chunk_size s in
  Wire.decode typ reader

(* Helper: encode to string via streaming writer with [chunk_size] buffer *)
let encode_chunked ~chunk_size typ v =
  let buf = Buffer.create 64 in
  let writer = Bytesrw.Bytes.Writer.of_buffer ~slice_length:chunk_size buf in
  Wire.encode typ v writer;
  Bytesrw.Bytes.Writer.write_eod writer;
  Buffer.contents buf

(* ── Generation tests ── *)

let test_bitfields () =
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 U32);
        field "y"
          ~constraint_:Expr.(field_ref "y" <= int 900)
          (bits ~width:10 U32);
        field "z"
          ~constraint_:Expr.(field_ref "y" + field_ref "z" <= int 60000)
          (bits ~width:16 U32);
      ]
  in
  let m = module_ [ typedef bf ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains BF" true (contains ~sub:"BF" output)

let test_enumerations () =
  let m =
    module_
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
      [ field "a" uint32; field "b" (apply (type_ref "t") [ field_ref "a" ]) ]
  in
  let m = module_ [ typedef t_struct; typedef s_struct ] in
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
  let m = module_ [ d_casetype ] in
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
  let m = module_ [ typedef simple ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains typedef" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output)

(* ── Parsing tests ── *)

let test_parse_uint8 () =
  let input = "\x42" in
  match decode_string uint8 input with
  | Ok v -> Alcotest.(check int) "uint8 value" 0x42 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_le () =
  let input = "\x01\x02" in
  match decode_string uint16 input with
  | Ok v -> Alcotest.(check int) "uint16 le value" 0x0201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_be () =
  let input = "\x01\x02" in
  match decode_string uint16be input with
  | Ok v -> Alcotest.(check int) "uint16 be value" 0x0102 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_le () =
  let input = "\x01\x02\x03\x04" in
  match decode_string uint32 input with
  | Ok v -> Alcotest.(check int) "uint32 le value" 0x04030201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_be () =
  let input = "\x01\x02\x03\x04" in
  match decode_string uint32be input with
  | Ok v -> Alcotest.(check int) "uint32 be value" 0x01020304 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint64_le () =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match decode_string uint64 input with
  | Ok v -> Alcotest.(check int64) "uint64 le value" 0x0807060504030201L v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_array () =
  let input = "\x01\x02\x03" in
  let t = array ~len:(int 3) uint8 in
  match decode_string t input with
  | Ok v -> Alcotest.(check (list int)) "array values" [ 1; 2; 3 ] v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_byte_array () =
  let input = "hello" in
  let t = byte_array ~size:(int 5) in
  match decode_string t input with
  | Ok v -> Alcotest.(check string) "byte_array value" "hello" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_variants_valid () =
  let input = "\x01" in
  let t = variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] uint8 in
  match decode_string t input with
  | Ok v -> Alcotest.(check bool) "variants value" true (v = `B)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_variants_invalid () =
  let input = "\xFF" in
  let t = variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] uint8 in
  match decode_string t input with
  | Ok _ -> Alcotest.fail "expected error for invalid enum"
  | Error (Invalid_enum { value; _ }) ->
      Alcotest.(check int) "invalid enum value" 255 value
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_all_bytes () =
  let input = "hello world" in
  match decode_string all_bytes input with
  | Ok v -> Alcotest.(check string) "all_bytes value" "hello world" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_valid () =
  let input = "\x00\x00\x00" in
  match decode_string all_zeros input with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_invalid () =
  let input = "\x00\x01\x00" in
  match decode_string all_zeros input with
  | Ok _ -> Alcotest.fail "expected error for non-zero byte"
  | Error (All_zeros_failed { offset }) ->
      Alcotest.(check int) "non-zero offset" 1 offset
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_bitfield () =
  let input = "\xFF\xFF\xFF\xFF" in
  let t = bits ~width:6 U32 in
  match decode_string t input with
  | Ok v -> Alcotest.(check int) "bitfield value (6 bits)" 63 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_eof () =
  let input = "\x01" in
  match decode_string uint16 input with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error (Unexpected_eof { expected; got }) ->
      Alcotest.(check int) "expected bytes" 2 expected;
      Alcotest.(check int) "got bytes" 1 got
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_struct () =
  let input = "\x01\x02\x03" in
  let s =
    struct_ "Test" [ field "a" uint8; field "b" uint8; field "c" uint8 ]
  in
  let t = struct_typ s in
  match decode_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_constraint () =
  (* Test struct with constraint that should pass *)
  let input = "\x0A" in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(field_ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  match decode_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_constraint_fail () =
  (* Test struct with constraint that should fail *)
  let input = "\xFF" in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(field_ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  match decode_string t input with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_struct_action_return_true () =
  let input = "\x0A" in
  let s =
    struct_ "ActionOk"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(field_ref "x" <= int 10) ])
          uint8;
      ]
  in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_action_return_false () =
  let input = "\x0B" in
  let s =
    struct_ "ActionFail"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(field_ref "x" <= int 10) ])
          uint8;
      ]
  in
  match decode_string (struct_typ s) input with
  | Ok () -> Alcotest.fail "expected action failure"
  | Error (Constraint_failed "field action") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_struct_action_var () =
  let input = "\x04" in
  let s =
    struct_ "ActionVar"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.var "twice" Expr.(field_ref "x" * int 2);
                 Action.return_bool Expr.(field_ref "twice" = int 8);
               ])
          uint8;
      ]
  in
  match decode_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_action_abort () =
  let input = "\x01" in
  let s =
    struct_ "ActionAbort"
      [ field "x" ~action:(Action.on_success [ Action.abort ]) uint8 ]
  in
  match decode_string (struct_typ s) input with
  | Ok () -> Alcotest.fail "expected abort"
  | Error (Constraint_failed "field action") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* ── Encoding tests ── *)

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
  let encoded = encode_to_string uint32 0x04030201 in
  Alcotest.(check string) "uint32 le encoding" "\x01\x02\x03\x04" encoded

let test_encode_uint32_be () =
  let encoded = encode_to_string uint32be 0x01020304 in
  Alcotest.(check string) "uint32 be encoding" "\x01\x02\x03\x04" encoded

let test_encode_array () =
  let t = array ~len:(int 3) uint8 in
  let encoded = encode_to_string t [ 1; 2; 3 ] in
  Alcotest.(check string) "array encoding" "\x01\x02\x03" encoded

let test_encode_byte_array () =
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t "hello" in
  Alcotest.(check string) "byte_array encoding" "hello" encoded

let test_encode_variants () =
  let t = variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] uint8 in
  let encoded = encode_to_string t `B in
  Alcotest.(check string) "variants encoding" "\x01" encoded

let test_encode_bitfield () =
  let t = bits ~width:6 U32 in
  let encoded = encode_to_string t 63 in
  (* 63 = 0x3F, but stored in 4 bytes as uint32 LE *)
  Alcotest.(check string) "bitfield encoding" "\x3F\x00\x00\x00" encoded

(* ── Roundtrip tests ── *)

let test_roundtrip_uint8 () =
  let original = 0x42 in
  let encoded = encode_to_string uint8 original in
  match decode_string uint8 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint8" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint16 () =
  let original = 0x1234 in
  let encoded = encode_to_string uint16 original in
  match decode_string uint16 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint16" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint32 () =
  let original = 0x12345678 in
  let encoded = encode_to_string uint32 original in
  match decode_string uint32 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint32" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_array () =
  let original = [ 1; 2; 3; 4; 5 ] in
  let t = array ~len:(int 5) uint8 in
  let encoded = encode_to_string t original in
  match decode_string t encoded with
  | Ok decoded -> Alcotest.(check (list int)) "roundtrip array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_byte_array () =
  let original = "hello" in
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t original in
  match decode_string t encoded with
  | Ok decoded ->
      Alcotest.(check string) "roundtrip byte_array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* ── Streaming: cross-slice boundary tests ── *)

(* Parse roundtrip with every chunk size forcing boundary straddles *)
let test_stream_uint8 () =
  let encoded = encode_to_string uint8 42 in
  match parse_chunked ~chunk_size:1 uint8 encoded with
  | Ok v -> Alcotest.(check int) "uint8 chunk=1" 42 v
  | Error e -> Alcotest.failf "uint8 chunk=1: %a" pp_parse_error e

let test_stream_uint16_chunk1 () =
  let encoded = encode_to_string uint16 0xCAFE in
  match parse_chunked ~chunk_size:1 uint16 encoded with
  | Ok v -> Alcotest.(check int) "uint16 chunk=1" 0xCAFE v
  | Error e -> Alcotest.failf "uint16 chunk=1: %a" pp_parse_error e

let test_stream_uint16_chunk3 () =
  (* chunk=3 means 2-byte value fits in one slice — fast path *)
  let encoded = encode_to_string uint16be 0xBEEF in
  match parse_chunked ~chunk_size:3 uint16be encoded with
  | Ok v -> Alcotest.(check int) "uint16be chunk=3" 0xBEEF v
  | Error e -> Alcotest.failf "uint16be chunk=3: %a" pp_parse_error e

let test_stream_uint32_chunk1 () =
  let encoded = encode_to_string uint32 0xDEADBEEF in
  match parse_chunked ~chunk_size:1 uint32 encoded with
  | Ok v -> Alcotest.(check int) "uint32 chunk=1" 0xDEADBEEF v
  | Error e -> Alcotest.failf "uint32 chunk=1: %a" pp_parse_error e

let test_stream_uint32_chunk3 () =
  (* chunk=3: 4-byte value straddles at byte 3 *)
  let encoded = encode_to_string uint32be 0x12345678 in
  match parse_chunked ~chunk_size:3 uint32be encoded with
  | Ok v -> Alcotest.(check int) "uint32be chunk=3" 0x12345678 v
  | Error e -> Alcotest.failf "uint32be chunk=3: %a" pp_parse_error e

let test_stream_uint64_chunk1 () =
  let encoded = encode_to_string uint64 0x0102030405060708L in
  match parse_chunked ~chunk_size:1 uint64 encoded with
  | Ok v -> Alcotest.(check int64) "uint64 chunk=1" 0x0102030405060708L v
  | Error e -> Alcotest.failf "uint64 chunk=1: %a" pp_parse_error e

let test_stream_uint64_chunk3 () =
  let encoded = encode_to_string uint64be 0xFEDCBA9876543210L in
  match parse_chunked ~chunk_size:3 uint64be encoded with
  | Ok v -> Alcotest.(check int64) "uint64be chunk=3" 0xFEDCBA9876543210L v
  | Error e -> Alcotest.failf "uint64be chunk=3: %a" pp_parse_error e

let test_stream_uint64_chunk5 () =
  let encoded = encode_to_string uint64 0xAAAABBBBCCCCDDDDL in
  match parse_chunked ~chunk_size:5 uint64 encoded with
  | Ok v -> Alcotest.(check int64) "uint64 chunk=5" 0xAAAABBBBCCCCDDDDL v
  | Error e -> Alcotest.failf "uint64 chunk=5: %a" pp_parse_error e

let test_stream_bitfield_chunk1 () =
  (* Bitfield: 6+10+16 bits packed in a uint32 *)
  let bf = bits ~width:6 U32 in
  let encoded = encode_to_string bf 42 in
  match parse_chunked ~chunk_size:1 bf encoded with
  | Ok v ->
      (* 42 written in top 6 bits of uint32: 42 << 26, then read back as 6-bit *)
      Alcotest.(check int) "bitfield chunk=1" 42 v
  | Error e -> Alcotest.failf "bitfield chunk=1: %a" pp_parse_error e

(* Encode roundtrip through chunked writer *)
let test_stream_encode_chunk1 () =
  let v = 0xDEADBEEF in
  let encoded = encode_chunked ~chunk_size:1 uint32be v in
  match decode_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=1" v decoded
  | Error e -> Alcotest.failf "encode chunk=1: %a" pp_parse_error e

let test_stream_encode_chunk3 () =
  let v = 0x12345678 in
  let encoded = encode_chunked ~chunk_size:3 uint32be v in
  match decode_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=3" v decoded
  | Error e -> Alcotest.failf "encode chunk=3: %a" pp_parse_error e

(* ── Suite ── *)

let suite =
  ( "types",
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
      Alcotest.test_case "parse: variants valid" `Quick
        test_parse_variants_valid;
      Alcotest.test_case "parse: variants invalid" `Quick
        test_parse_variants_invalid;
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
      Alcotest.test_case "parse: struct action return true" `Quick
        test_parse_struct_action_return_true;
      Alcotest.test_case "parse: struct action return false" `Quick
        test_parse_struct_action_return_false;
      Alcotest.test_case "parse: struct action var" `Quick
        test_parse_struct_action_var;
      Alcotest.test_case "parse: struct action abort" `Quick
        test_parse_struct_action_abort;
      (* encoding *)
      Alcotest.test_case "encode: uint8" `Quick test_encode_uint8;
      Alcotest.test_case "encode: uint16 le" `Quick test_encode_uint16_le;
      Alcotest.test_case "encode: uint16 be" `Quick test_encode_uint16_be;
      Alcotest.test_case "encode: uint32 le" `Quick test_encode_uint32_le;
      Alcotest.test_case "encode: uint32 be" `Quick test_encode_uint32_be;
      Alcotest.test_case "encode: array" `Quick test_encode_array;
      Alcotest.test_case "encode: byte_array" `Quick test_encode_byte_array;
      Alcotest.test_case "encode: variants" `Quick test_encode_variants;
      Alcotest.test_case "encode: bitfield" `Quick test_encode_bitfield;
      (* roundtrip *)
      Alcotest.test_case "roundtrip: uint8" `Quick test_roundtrip_uint8;
      Alcotest.test_case "roundtrip: uint16" `Quick test_roundtrip_uint16;
      Alcotest.test_case "roundtrip: uint32" `Quick test_roundtrip_uint32;
      Alcotest.test_case "roundtrip: array" `Quick test_roundtrip_array;
      Alcotest.test_case "roundtrip: byte_array" `Quick
        test_roundtrip_byte_array;
      (* streaming: cross-slice boundary *)
      Alcotest.test_case "stream: uint8 chunk=1" `Quick test_stream_uint8;
      Alcotest.test_case "stream: uint16 chunk=1" `Quick
        test_stream_uint16_chunk1;
      Alcotest.test_case "stream: uint16 chunk=3" `Quick
        test_stream_uint16_chunk3;
      Alcotest.test_case "stream: uint32 chunk=1" `Quick
        test_stream_uint32_chunk1;
      Alcotest.test_case "stream: uint32 chunk=3" `Quick
        test_stream_uint32_chunk3;
      Alcotest.test_case "stream: uint64 chunk=1" `Quick
        test_stream_uint64_chunk1;
      Alcotest.test_case "stream: uint64 chunk=3" `Quick
        test_stream_uint64_chunk3;
      Alcotest.test_case "stream: uint64 chunk=5" `Quick
        test_stream_uint64_chunk5;
      Alcotest.test_case "stream: bitfield chunk=1" `Quick
        test_stream_bitfield_chunk1;
      Alcotest.test_case "stream: encode chunk=1" `Quick
        test_stream_encode_chunk1;
      Alcotest.test_case "stream: encode chunk=3" `Quick
        test_stream_encode_chunk3;
    ] )
