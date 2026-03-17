(* Test wire library *)

open Wire

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Helper: parse from a string delivered in slices of [chunk_size] bytes.
   Forces multi-byte values to straddle slice boundaries. *)
let parse_chunked ~chunk_size typ s =
  let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:chunk_size s in
  Wire.parse typ reader

(* Helper: encode to string via streaming writer with [chunk_size] buffer *)
let encode_chunked ~chunk_size typ v =
  let buf = Buffer.create 64 in
  let writer = Bytesrw.Bytes.Writer.of_buffer ~slice_length:chunk_size buf in
  Wire.encode typ v writer;
  Bytesrw.Bytes.Writer.write_eod writer;
  Buffer.contents buf

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
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_le () =
  let input = "\x01\x02" in
  match parse_string uint16 input with
  | Ok v -> Alcotest.(check int) "uint16 le value" 0x0201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_be () =
  let input = "\x01\x02" in
  match parse_string uint16be input with
  | Ok v -> Alcotest.(check int) "uint16 be value" 0x0102 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_le () =
  let input = "\x01\x02\x03\x04" in
  match parse_string uint32 input with
  | Ok v -> Alcotest.(check int) "uint32 le value" 0x04030201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_be () =
  let input = "\x01\x02\x03\x04" in
  match parse_string uint32be input with
  | Ok v -> Alcotest.(check int) "uint32 be value" 0x01020304 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint64_le () =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match parse_string uint64 input with
  | Ok v -> Alcotest.(check int64) "uint64 le value" 0x0807060504030201L v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_array () =
  let input = "\x01\x02\x03" in
  let t = array ~len:(int 3) uint8 in
  match parse_string t input with
  | Ok v -> Alcotest.(check (list int)) "array values" [ 1; 2; 3 ] v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_byte_array () =
  let input = "hello" in
  let t = byte_array ~size:(int 5) in
  match parse_string t input with
  | Ok v -> Alcotest.(check string) "byte_array value" "hello" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_enum_valid () =
  let input = "\x01" in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  match parse_string t input with
  | Ok v -> Alcotest.(check int) "enum value" 1 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_enum_invalid () =
  let input = "\xFF" in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  match parse_string t input with
  | Ok _ -> Alcotest.fail "expected error for invalid enum"
  | Error (Invalid_enum { value; _ }) ->
      Alcotest.(check int) "invalid enum value" 255 value
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_all_bytes () =
  let input = "hello world" in
  match parse_string all_bytes input with
  | Ok v -> Alcotest.(check string) "all_bytes value" "hello world" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_valid () =
  let input = "\x00\x00\x00" in
  match parse_string all_zeros input with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_invalid () =
  let input = "\x00\x01\x00" in
  match parse_string all_zeros input with
  | Ok _ -> Alcotest.fail "expected error for non-zero byte"
  | Error (All_zeros_failed { offset }) ->
      Alcotest.(check int) "non-zero offset" 1 offset
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_bitfield () =
  let input = "\xFF\xFF\xFF\xFF" in
  let t = bits ~width:6 bf_uint32 in
  match parse_string t input with
  | Ok v -> Alcotest.(check int) "bitfield value (6 bits)" 63 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_eof () =
  let input = "\x01" in
  match parse_string uint16 input with
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
  match parse_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

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
  | Error e -> Alcotest.failf "%a" pp_parse_error e

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
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

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
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint16 () =
  let original = 0x1234 in
  let encoded = encode_to_string uint16 original in
  match parse_string uint16 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint16" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint32 () =
  let original = 0x12345678 in
  let encoded = encode_to_string uint32 original in
  match parse_string uint32 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint32" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_array () =
  let original = [ 1; 2; 3; 4; 5 ] in
  let t = array ~len:(int 5) uint8 in
  let encoded = encode_to_string t original in
  match parse_string t encoded with
  | Ok decoded -> Alcotest.(check (list int)) "roundtrip array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_byte_array () =
  let original = "hello" in
  let t = byte_array ~size:(int 5) in
  let encoded = encode_to_string t original in
  match parse_string t encoded with
  | Ok decoded ->
      Alcotest.(check string) "roundtrip byte_array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* Record codec tests *)

type simple_record = { a : int; b : int; c : int }

let simple_record_codec =
  Codec.view "SimpleRecord"
    (fun a b c -> { a; b; c })
    Codec.
      [
        Codec.field "a" uint8 (fun r -> r.a);
        Codec.field "b" uint16 (fun r -> r.b);
        Codec.field "c" uint32 (fun r -> r.c);
      ]

let test_record_encode () =
  let v = { a = 0x42; b = 0x1234; c = 0x56789ABC } in
  match encode_record_to_string simple_record_codec v with
  | Error e -> Alcotest.failf "%a" pp_parse_error e
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
      Alcotest.(check int) "c" 0x56789ABC v.c
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_record_roundtrip () =
  let original = { a = 0xAB; b = 0xCDEF; c = 0x12345678 } in
  match encode_record_to_string simple_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record_from_string simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a roundtrip" original.a decoded.a;
          Alcotest.(check int) "b roundtrip" original.b decoded.b;
          Alcotest.(check int) "c roundtrip" original.c decoded.c
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

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
  Codec.view "MultiRecord"
    (fun x y -> { x; y })
    Codec.
      [
        Codec.field "x" uint16be (fun r -> r.x);
        Codec.field "y" uint16be (fun r -> r.y);
      ]

let test_record_with_multi () =
  let original = { x = 0x1234; y = 0x5678 } in
  match encode_record_to_string multi_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      match decode_record_from_string multi_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "x" original.x decoded.x;
          Alcotest.(check int) "y" original.y decoded.y
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Record with byte_array field *)
type ba_record = { id : int; uuid : string; tag : int }

let ba_record_codec =
  Codec.view "BaRecord"
    (fun id uuid tag -> { id; uuid; tag })
    Codec.
      [
        Codec.field "id" uint32be (fun r -> r.id);
        Codec.field "uuid" (byte_array ~size:(int 16)) (fun r -> r.uuid);
        Codec.field "tag" uint16be (fun r -> r.tag);
      ]

let test_record_byte_array_roundtrip () =
  let original = { id = 0x12345678; uuid = "0123456789abcdef"; tag = 0xABCD } in
  match encode_record_to_string ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      match decode_record_from_string ba_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "id" original.id decoded.id;
          Alcotest.(check string) "uuid" original.uuid decoded.uuid;
          Alcotest.(check int) "tag" original.tag decoded.tag
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_byte_array_padding () =
  (* Short string should be zero-padded *)
  let original = { id = 1; uuid = "short"; tag = 2 } in
  match encode_record_to_string ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      (* Verify zero padding: bytes 9..19 should be zero *)
      for i = 9 to 19 do
        Alcotest.(check int)
          (Fmt.str "padding byte %d" i)
          0
          (Char.code encoded.[i])
      done;
      match decode_record_from_string ba_record_codec encoded with
      | Ok decoded ->
          (* Decoded uuid includes the zero padding *)
          Alcotest.(check int) "uuid length" 16 (String.length decoded.uuid);
          Alcotest.(check string)
            "uuid prefix" "short"
            (String.sub decoded.uuid 0 5)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Codec bitfield tests *)

type bf32_record = { bf_a : int; bf_b : int; bf_c : int; bf_d : int }

let bf32_codec =
  Codec.view "Bf32Test"
    (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
    Codec.
      [
        Codec.field "a" (bits ~width:3 bf_uint32be) (fun t -> t.bf_a);
        Codec.field "b" (bits ~width:5 bf_uint32be) (fun t -> t.bf_b);
        Codec.field "c" (bits ~width:16 bf_uint32be) (fun t -> t.bf_c);
        Codec.field "d" (bits ~width:8 bf_uint32be) (fun t -> t.bf_d);
      ]

type bf16_record = {
  bf_ver : int;
  bf_flags : int;
  bf_id : int;
  bf_count : int;
  bf_len : int;
}

let bf16_codec =
  Codec.view "Bf16Test"
    (fun ver flags id count len ->
      {
        bf_ver = ver;
        bf_flags = flags;
        bf_id = id;
        bf_count = count;
        bf_len = len;
      })
    Codec.
      [
        Codec.field "ver" (bits ~width:3 bf_uint16be) (fun t -> t.bf_ver);
        Codec.field "flags" (bits ~width:2 bf_uint16be) (fun t -> t.bf_flags);
        Codec.field "id" (bits ~width:11 bf_uint16be) (fun t -> t.bf_id);
        Codec.field "count" (bits ~width:14 bf_uint16be) (fun t -> t.bf_count);
        Codec.field "len" (bits ~width:2 bf_uint16be) (fun t -> t.bf_len);
      ]

let test_codec_bitfield_wire_size () =
  Alcotest.(check int) "bf32 wire_size" 4 (Codec.wire_size bf32_codec);
  Alcotest.(check int) "bf16 wire_size" 4 (Codec.wire_size bf16_codec)

let test_codec_bitfield_roundtrip () =
  let original = { bf_a = 5; bf_b = 20; bf_c = 0x1234; bf_d = 0xAB } in
  match encode_record_to_string bf32_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record_from_string bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" original.bf_a decoded.bf_a;
          Alcotest.(check int) "b" original.bf_b decoded.bf_b;
          Alcotest.(check int) "c" original.bf_c decoded.bf_c;
          Alcotest.(check int) "d" original.bf_d decoded.bf_d
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_byte_layout () =
  (* a=5 (3b), b=20 (5b), c=0x1234 (16b), d=0xAB (8b)
     MSB-first packing: 101_10100_0001001000110100_10101011
     = 0xB4 0x12 0x34 0xAB *)
  let v = { bf_a = 5; bf_b = 20; bf_c = 0x1234; bf_d = 0xAB } in
  match encode_record_to_string bf32_codec v with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded ->
      Alcotest.(check int) "length" 4 (String.length encoded);
      Alcotest.(check int) "byte 0" 0xB4 (Char.code encoded.[0]);
      Alcotest.(check int) "byte 1" 0x12 (Char.code encoded.[1]);
      Alcotest.(check int) "byte 2" 0x34 (Char.code encoded.[2]);
      Alcotest.(check int) "byte 3" 0xAB (Char.code encoded.[3])

let test_codec_bitfield_decode () =
  (* Decode 0xB41234AB -> a=5, b=20, c=0x1234, d=0xAB *)
  let input = "\xB4\x12\x34\xAB" in
  match decode_record_from_string bf32_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 5 v.bf_a;
      Alcotest.(check int) "b" 20 v.bf_b;
      Alcotest.(check int) "c" 0x1234 v.bf_c;
      Alcotest.(check int) "d" 0xAB v.bf_d
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_codec_bitfield_multi_group () =
  (* Two bf_uint16be groups: (3+2+11=16) + (14+2=16) = 32 bits = 4 bytes *)
  let v =
    { bf_ver = 5; bf_flags = 2; bf_id = 0x7FF; bf_count = 0x3FFF; bf_len = 3 }
  in
  match encode_record_to_string bf16_codec v with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      (* First group: 101_10_11111111111 = 0xB7FF *)
      Alcotest.(check int) "byte 0" 0xB7 (Char.code encoded.[0]);
      Alcotest.(check int) "byte 1" 0xFF (Char.code encoded.[1]);
      (* Second group: 11111111111111_11 = 0xFFFF *)
      Alcotest.(check int) "byte 2" 0xFF (Char.code encoded.[2]);
      Alcotest.(check int) "byte 3" 0xFF (Char.code encoded.[3]);
      (* Roundtrip decode *)
      match decode_record_from_string bf16_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "ver" v.bf_ver decoded.bf_ver;
          Alcotest.(check int) "flags" v.bf_flags decoded.bf_flags;
          Alcotest.(check int) "id" v.bf_id decoded.bf_id;
          Alcotest.(check int) "count" v.bf_count decoded.bf_count;
          Alcotest.(check int) "len" v.bf_len decoded.bf_len
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_to_struct () =
  let s = Codec.to_struct bf32_codec in
  let m = module_ "Bf32Test" [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT32BE" true
    (contains ~sub:"UINT32BE" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b" output)

(* Zero-copy view tests *)

let test_view_get_uint () =
  let codec, f_x, f_y =
    let f_x = Codec.field "x" uint16be (fun r -> r.x) in
    let f_y = Codec.field "y" uint16be (fun r -> r.y) in
    let codec =
      Codec.view "ViewUint" (fun a b -> { x = a; y = b }) Codec.[ f_x; f_y ]
    in
    (codec, f_x, f_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  Alcotest.(check int)
    "get x" 0x1234
    ((Staged.unstage (Codec.get codec f_x)) buf 0);
  Alcotest.(check int)
    "get y" 0x5678
    ((Staged.unstage (Codec.get codec f_y)) buf 0)

let test_view_get_bitfield () =
  let codec, f_a, f_d =
    let f_a = Codec.field "a" (bits ~width:3 bf_uint32be) (fun t -> t.bf_a) in
    let f_d = Codec.field "d" (bits ~width:8 bf_uint32be) (fun t -> t.bf_d) in
    let codec =
      Codec.view "ViewBf"
        (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
        Codec.
          [
            f_a;
            Codec.field "b" (bits ~width:5 bf_uint32be) (fun t -> t.bf_b);
            Codec.field "c" (bits ~width:16 bf_uint32be) (fun t -> t.bf_c);
            f_d;
          ]
    in
    (codec, f_a, f_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  Alcotest.(check int) "get a" 5 ((Staged.unstage (Codec.get codec f_a)) buf 0);
  Alcotest.(check int)
    "get d" 0xAB
    ((Staged.unstage (Codec.get codec f_d)) buf 0)

let test_view_get_bool () =
  let codec, f_flag =
    let f_flag = Codec.field "flag" (bool (bits ~width:1 bf_uint8)) fst in
    let codec =
      Codec.view "ViewBool"
        (fun flag code -> (flag, code))
        Codec.[ f_flag; Codec.field "code" (bits ~width:7 bf_uint8) snd ]
    in
    (codec, f_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x80;
  Alcotest.(check bool)
    "get flag=true" true
    ((Staged.unstage (Codec.get codec f_flag)) buf 0);
  Bytes.set_uint8 buf 0 0x00;
  Alcotest.(check bool)
    "get flag=false" false
    ((Staged.unstage (Codec.get codec f_flag)) buf 0)

let test_view_set_bitfield () =
  let codec, f_a, f_d =
    let f_a = Codec.field "a" (bits ~width:3 bf_uint32be) (fun t -> t.bf_a) in
    let f_d = Codec.field "d" (bits ~width:8 bf_uint32be) (fun t -> t.bf_d) in
    let codec =
      Codec.view "ViewSetBf"
        (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
        Codec.
          [
            f_a;
            Codec.field "b" (bits ~width:5 bf_uint32be) (fun t -> t.bf_b);
            Codec.field "c" (bits ~width:16 bf_uint32be) (fun t -> t.bf_c);
            f_d;
          ]
    in
    (codec, f_a, f_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  (Staged.unstage (Codec.set codec f_a)) buf 0 3;
  Alcotest.(check int)
    "get a after set" 3
    ((Staged.unstage (Codec.get codec f_a)) buf 0);
  let r = Codec.decode codec buf 0 in
  Alcotest.(check int) "b preserved" 20 r.bf_b;
  Alcotest.(check int) "c preserved" 0x1234 r.bf_c;
  Alcotest.(check int) "d preserved" 0xAB r.bf_d;
  (Staged.unstage (Codec.set codec f_d)) buf 0 0x42;
  Alcotest.(check int)
    "get d after set" 0x42
    ((Staged.unstage (Codec.get codec f_d)) buf 0);
  let r = Codec.decode codec buf 0 in
  Alcotest.(check int) "a still 3" 3 r.bf_a;
  Alcotest.(check int) "b still 20" 20 r.bf_b;
  Alcotest.(check int) "c still 0x1234" 0x1234 r.bf_c

let test_view_set_uint () =
  let codec, f_x, f_y =
    let f_x = Codec.field "x" uint16be (fun r -> r.x) in
    let f_y = Codec.field "y" uint16be (fun r -> r.y) in
    let codec =
      Codec.view "ViewSetUint" (fun x y -> { x; y }) Codec.[ f_x; f_y ]
    in
    (codec, f_x, f_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (Staged.unstage (Codec.set codec f_x)) buf 0 0xAAAA;
  Alcotest.(check int)
    "get x after set" 0xAAAA
    ((Staged.unstage (Codec.get codec f_x)) buf 0);
  Alcotest.(check int)
    "y unchanged" 0x5678
    ((Staged.unstage (Codec.get codec f_y)) buf 0)

let test_view_bounds_check () =
  let codec =
    Codec.view "ViewBounds"
      (fun a -> a)
      Codec.[ Codec.field "a" uint32be (fun a -> a) ]
  in
  let buf = Bytes.create 2 in
  match Codec.decode codec buf 0 with
  | _ -> Alcotest.fail "expected Parse_error for short buffer"
  | exception Parse_error (Unexpected_eof _) -> ()
  | exception e -> Alcotest.failf "wrong exception: %s" (Printexc.to_string e)

let test_view_with_offset () =
  let codec, f_a =
    let f_a = Codec.field "a" uint16be (fun a -> a) in
    let codec = Codec.view "ViewOff" (fun a -> a) Codec.[ f_a ] in
    (codec, f_a)
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_uint16_be buf 2 0x2222;
  Bytes.set_uint16_be buf 4 0x3333;
  Alcotest.(check int)
    "get at offset 2" 0x2222
    ((Staged.unstage (Codec.get codec f_a)) buf 2)

let test_view_set_bool () =
  let codec, f_flag =
    let f_flag = Codec.field "flag" (bool (bits ~width:1 bf_uint8)) fst in
    let codec =
      Codec.view "ViewSetBool"
        (fun flag code -> (flag, code))
        Codec.[ f_flag; Codec.field "code" (bits ~width:7 bf_uint8) snd ]
    in
    (codec, f_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec f_flag)) buf 0 true;
  Alcotest.(check bool)
    "get flag after set true" true
    ((Staged.unstage (Codec.get codec f_flag)) buf 0);
  Alcotest.(check int) "byte value" 0x80 (Bytes.get_uint8 buf 0);
  (Staged.unstage (Codec.set codec f_flag)) buf 0 false;
  Alcotest.(check bool)
    "get flag after set false" false
    ((Staged.unstage (Codec.get codec f_flag)) buf 0);
  Alcotest.(check int) "byte cleared" 0x00 (Bytes.get_uint8 buf 0)

(* Field sharing tests — same field spec used in two codecs *)

let test_view_shared_field_spec () =
  (* Two codecs with different layouts, each with their own field "x".
     Codec1: [u16be x] [u16be y]   → x at offset 0
     Codec2: [u16be pad] [u16be x] → x at offset 2
     Each codec gets a fresh field object. *)
  let f1_x = Codec.field "x" uint16be (fun (x, _) -> x) in
  let codec1 =
    Codec.view "Share1"
      (fun x y -> (x, y))
      Codec.[ f1_x; Codec.field "y" uint16be (fun (_, y) -> y) ]
  in
  let f2_x = Codec.field "x" uint16be (fun (x, _) -> x) in
  let codec2 =
    Codec.view "Share2"
      (fun _pad x -> (x, 0))
      Codec.[ Codec.field "pad" uint16be (fun _ -> 0); f2_x ]
  in
  let buf1 = Bytes.create 4 in
  Bytes.set_uint16_be buf1 0 0xAAAA;
  Bytes.set_uint16_be buf1 2 0xBBBB;
  let buf2 = Bytes.create 4 in
  Bytes.set_uint16_be buf2 0 0x0000;
  Bytes.set_uint16_be buf2 2 0xCCCC;
  (* f1_x reads at offset 0, f2_x reads at offset 2 *)
  Alcotest.(check int)
    "codec1 get x" 0xAAAA
    ((Staged.unstage (Codec.get codec1 f1_x)) buf1 0);
  Alcotest.(check int)
    "codec2 get x" 0xCCCC
    ((Staged.unstage (Codec.get codec2 f2_x)) buf2 0)

let test_view_shared_bitfield_spec () =
  (* Two codecs with different bitfield layouts, each with their own field "a".
     Codec1: [3-bit a] [5-bit b]            → a is top 3 bits
     Codec2: [5-bit pad] [3-bit a]           → a is bottom 3 bits
     Each codec gets a fresh field object. *)
  let f1_a = Codec.field "a" (bits ~width:3 bf_uint8) (fun (a, _) -> a) in
  let codec1 =
    Codec.view "ShareBf1"
      (fun a b -> (a, b))
      Codec.[ f1_a; Codec.field "b" (bits ~width:5 bf_uint8) (fun (_, b) -> b) ]
  in
  let f2_a = Codec.field "a" (bits ~width:3 bf_uint8) (fun (a, _) -> a) in
  let codec2 =
    Codec.view "ShareBf2"
      (fun _pad a -> (a, 0))
      Codec.[ Codec.field "pad" (bits ~width:5 bf_uint8) (fun _ -> 0); f2_a ]
  in
  (* 0xE3 = 0b_111_00011
     codec1 reads top 3 bits → 7
     codec2 reads bottom 3 bits → 3 *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xE3;
  Alcotest.(check int)
    "codec1 get a (top 3)" 7
    ((Staged.unstage (Codec.get codec1 f1_a)) buf 0);
  Alcotest.(check int)
    "codec2 get a (bot 3)" 3
    ((Staged.unstage (Codec.get codec2 f2_a)) buf 0)

let test_view_shared_set_independent () =
  (* set via one codec's field must not affect the other's interpretation *)
  let f1 = Codec.field "v" (bits ~width:4 bf_uint8) (fun (v, _) -> v) in
  let codec1 =
    Codec.view "SetShare1"
      (fun v pad -> (v, pad))
      Codec.[ f1; Codec.field "pad" (bits ~width:4 bf_uint8) (fun (_, p) -> p) ]
  in
  let f2 = Codec.field "v" (bits ~width:4 bf_uint8) (fun (v, _) -> v) in
  let codec2 =
    Codec.view "SetShare2"
      (fun pad v -> (v, pad))
      Codec.[ Codec.field "pad" (bits ~width:4 bf_uint8) (fun (_, p) -> p); f2 ]
  in
  (* Start: 0x00. Set codec1's field (top nibble) to 0xA *)
  let buf = Bytes.create 1 in
  (Staged.unstage (Codec.set codec1 f1)) buf 0 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* codec2's field is bottom nibble — should still be 0 *)
  Alcotest.(check int)
    "codec2 get after set1" 0
    ((Staged.unstage (Codec.get codec2 f2)) buf 0);
  (* Set codec2's field (bottom nibble) to 0x5 *)
  (Staged.unstage (Codec.set codec2 f2)) buf 0 0x5;
  Alcotest.(check int) "byte after set2" 0xA5 (Bytes.get_uint8 buf 0);
  (* codec1's field should still be 0xA *)
  Alcotest.(check int)
    "codec1 get after set2" 0xA
    ((Staged.unstage (Codec.get codec1 f1)) buf 0)

(* ── byte_slice tests ── *)

module Bs = Bytesrw.Bytes.Slice

let test_view_byte_slice_get () =
  (* A record with a fixed-size byte_slice field returns a sub-slice *)
  let f_payload =
    Codec.field "payload" (byte_slice ~size:(int 4)) (fun (_, p) -> p)
  in
  let codec =
    Codec.view "SliceRec"
      (fun hdr payload -> (hdr, payload))
      Codec.[ Codec.field "hdr" uint16be (fun (h, _) -> h); f_payload ]
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xABCD;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload = (Staged.unstage (Codec.get codec f_payload)) buf 0 in
  (* payload should be a slice into buf at offset 2, length 4 *)
  Alcotest.(check int) "payload first" 2 (Bs.first payload);
  Alcotest.(check int) "payload length" 4 (Bs.length payload);
  Alcotest.(check bool) "same buffer" true (Bs.bytes payload == buf);
  Alcotest.(check int)
    "payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload));
  Alcotest.(check int)
    "payload[3]" 0x40
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload + 3))

let test_view_byte_slice_decode () =
  (* decode also produces a correct sub-slice *)
  let codec =
    Codec.view "SliceDec"
      (fun tag payload -> (tag, payload))
      Codec.
        [
          Codec.field "tag" uint8 (fun (t, _) -> t);
          Codec.field "data" (byte_slice ~size:(int 3)) (fun (_, p) -> p);
        ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xFF;
  Bytes.set_uint8 buf 1 0xAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let tag, payload = Codec.decode codec buf 0 in
  Alcotest.(check int) "tag" 0xFF tag;
  Alcotest.(check int) "payload first" 1 (Bs.first payload);
  Alcotest.(check int) "payload length" 3 (Bs.length payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_view_byte_slice_nested () =
  (* Two-layer nested protocol: get payload slice, then get inner field *)
  let f_val = Codec.field "val" uint16be (fun v -> v) in
  let inner_codec = Codec.view "Inner" (fun v -> v) Codec.[ f_val ] in
  let f_payload =
    Codec.field "payload" (byte_slice ~size:(int 2)) (fun (_, p) -> p)
  in
  let outer_codec =
    Codec.view "Outer"
      (fun hdr payload -> (hdr, payload))
      Codec.[ Codec.field "hdr" uint16be (fun (h, _) -> h); f_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x1234;
  let payload_off = (Staged.unstage (Codec.sub outer_codec f_payload)) buf 0 in
  let inner_val =
    (Staged.unstage (Codec.get inner_codec f_val)) buf payload_off
  in
  Alcotest.(check int) "inner val via zero-copy" 0x1234 inner_val

(* ── Raw access: get / set / sub ── *)

let test_raw_get_uint () =
  let f_a = Codec.field "a" uint16be (fun (a, _) -> a) in
  let f_b = Codec.field "b" uint8 (fun (_, b) -> b) in
  let codec = Codec.view "RawU" (fun a b -> (a, b)) Codec.[ f_a; f_b ] in
  let buf = Bytes.create 3 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint8 buf 2 0xFF;
  Alcotest.(check int)
    "get a" 0x1234
    ((Staged.unstage (Codec.get codec f_a)) buf 0);
  Alcotest.(check int)
    "get b" 0xFF
    ((Staged.unstage (Codec.get codec f_b)) buf 0)

let test_raw_get_bitfield () =
  let f_hi = Codec.field "hi" (bits ~width:4 bf_uint8) (fun (h, _) -> h) in
  let f_lo = Codec.field "lo" (bits ~width:4 bf_uint8) (fun (_, l) -> l) in
  let codec = Codec.view "RawBF" (fun hi lo -> (hi, lo)) Codec.[ f_hi; f_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  (* hi=0xA=10, lo=0x7=7 *)
  Alcotest.(check int)
    "get hi" 0xA
    ((Staged.unstage (Codec.get codec f_hi)) buf 0);
  Alcotest.(check int)
    "get lo" 0x7
    ((Staged.unstage (Codec.get codec f_lo)) buf 0)

let test_raw_set_uint () =
  let f_a = Codec.field "a" uint16be (fun (a, _) -> a) in
  let f_b = Codec.field "b" uint8 (fun (_, b) -> b) in
  let codec = Codec.view "RawSU" (fun a b -> (a, b)) Codec.[ f_a; f_b ] in
  let buf = Bytes.create 3 in
  Bytes.fill buf 0 3 '\x00';
  (Staged.unstage (Codec.set codec f_a)) buf 0 0xABCD;
  (Staged.unstage (Codec.set codec f_b)) buf 0 0x42;
  Alcotest.(check int) "set a" 0xABCD (Bytes.get_uint16_be buf 0);
  Alcotest.(check int) "set b" 0x42 (Bytes.get_uint8 buf 2)

let test_raw_set_bitfield () =
  let f_hi = Codec.field "hi" (bits ~width:4 bf_uint8) (fun (h, _) -> h) in
  let f_lo = Codec.field "lo" (bits ~width:4 bf_uint8) (fun (_, l) -> l) in
  let codec =
    Codec.view "RawSBF" (fun hi lo -> (hi, lo)) Codec.[ f_hi; f_lo ]
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec f_hi)) buf 0 0xC;
  (Staged.unstage (Codec.set codec f_lo)) buf 0 0x3;
  Alcotest.(check int) "set bf byte" 0xC3 (Bytes.get_uint8 buf 0)

let test_raw_sub_nested () =
  (* Two-layer nested protocol using sub + get: zero alloc *)
  let f_val = Codec.field "val" uint16be (fun v -> v) in
  let inner_codec = Codec.view "Inner" (fun v -> v) Codec.[ f_val ] in
  let f_payload =
    Codec.field "payload" (byte_slice ~size:(int 2)) (fun (_, p) -> p)
  in
  let outer_codec =
    Codec.view "Outer"
      (fun hdr payload -> (hdr, payload))
      Codec.[ Codec.field "hdr" uint16be (fun (h, _) -> h); f_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x5678;
  let inner_off = (Staged.unstage (Codec.sub outer_codec f_payload)) buf 0 in
  Alcotest.(check int) "sub offset" 2 inner_off;
  let inner_val =
    (Staged.unstage (Codec.get inner_codec f_val)) buf inner_off
  in
  Alcotest.(check int) "inner val via sub+get" 0x5678 inner_val

let test_raw_sub_three_layers () =
  (* Three-layer: outer -> mid -> inner, all zero-alloc via sub+get *)
  let f_x = Codec.field "x" uint8 (fun x -> x) in
  let inner = Codec.view "L3" (fun x -> x) Codec.[ f_x ] in
  let f_mid_payload =
    Codec.field "data" (byte_slice ~size:(int 1)) (fun (_, p) -> p)
  in
  let mid =
    Codec.view "L2"
      (fun tag payload -> (tag, payload))
      Codec.[ Codec.field "tag" uint8 (fun (t, _) -> t); f_mid_payload ]
  in
  let f_body =
    Codec.field "body" (byte_slice ~size:(int 2)) (fun (_, b) -> b)
  in
  let outer =
    Codec.view "L1"
      (fun hdr body -> (hdr, body))
      Codec.[ Codec.field "hdr" uint16be (fun (h, _) -> h); f_body ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let mid_off = (Staged.unstage (Codec.sub outer f_body)) buf 0 in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off = (Staged.unstage (Codec.sub mid f_mid_payload)) buf mid_off in
  Alcotest.(check int) "inner offset" 3 inner_off;
  let x = (Staged.unstage (Codec.get inner f_x)) buf inner_off in
  Alcotest.(check int) "3-layer get" 0xCC x

let test_raw_with_offset () =
  (* get / set work correctly with non-zero base offset *)
  let f_v = Codec.field "v" uint32be (fun v -> v) in
  let codec = Codec.view "RawOff" (fun v -> v) Codec.[ f_v ] in
  let buf = Bytes.create 20 in
  Bytes.fill buf 0 20 '\x00';
  (Staged.unstage (Codec.set codec f_v)) buf 10 0xDEADBEEF;
  Alcotest.(check int)
    "get at offset 10" 0xDEADBEEF
    ((Staged.unstage (Codec.get codec f_v)) buf 10)

(* FFI stub generation tests *)

let test_c_stubs () =
  let s =
    struct_ "SimpleHeader"
      [ field "version" uint8; field "length" uint16; field "flags" uint8 ]
  in
  let stubs = to_c_stubs [ s ] in
  Alcotest.(check bool)
    "contains check stub" true
    (contains ~sub:"caml_wire_simpleheader_check" stubs);
  Alcotest.(check bool)
    "contains error handler" true
    (contains ~sub:"simpleheader_err" stubs)

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
  let bf = bits ~width:6 bf_uint32 in
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
  match parse_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=1" v decoded
  | Error e -> Alcotest.failf "encode chunk=1: %a" pp_parse_error e

let test_stream_encode_chunk3 () =
  let v = 0x12345678 in
  let encoded = encode_chunked ~chunk_size:3 uint32be v in
  match parse_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=3" v decoded
  | Error e -> Alcotest.failf "encode chunk=3: %a" pp_parse_error e

(* ── Dependent-size byte_slice tests ── *)

type dep_slice_record = { ds_length : int; ds_payload : Bs.t }

let f_ds_length = Codec.field "Length" uint16be (fun r -> r.ds_length)

let f_ds_payload =
  Codec.field "Payload"
    (byte_slice ~size:(Codec.ref f_ds_length))
    (fun r -> r.ds_payload)

let dep_slice_codec =
  Codec.view "DepSlice"
    (fun length payload -> { ds_length = length; ds_payload = payload })
    Codec.[ f_ds_length; f_ds_payload ]

let test_dep_bslice_decode_empty () =
  (* length=0, no payload bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0;
  let r = Codec.decode dep_slice_codec buf 0 in
  Alcotest.(check int) "length" 0 r.ds_length;
  Alcotest.(check int) "payload length" 0 (Bs.length r.ds_payload)

let test_dep_bslice_decode_4 () =
  (* length=4, 4 payload bytes *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint8 buf 5 0xDD;
  let r = Codec.decode dep_slice_codec buf 0 in
  Alcotest.(check int) "length" 4 r.ds_length;
  Alcotest.(check int) "payload length" 4 (Bs.length r.ds_payload);
  Alcotest.(check int) "payload first" 2 (Bs.first r.ds_payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes r.ds_payload) (Bs.first r.ds_payload));
  Alcotest.(check int)
    "payload[3]" 0xDD
    (Bytes.get_uint8 (Bs.bytes r.ds_payload) (Bs.first r.ds_payload + 3))

let test_dep_bslice_decode_100 () =
  (* length=100, 100 payload bytes *)
  let buf = Bytes.create 102 in
  Bytes.set_uint16_be buf 0 100;
  for i = 0 to 99 do
    Bytes.set_uint8 buf (2 + i) (i land 0xFF)
  done;
  let r = Codec.decode dep_slice_codec buf 0 in
  Alcotest.(check int) "length" 100 r.ds_length;
  Alcotest.(check int) "payload length" 100 (Bs.length r.ds_payload);
  Alcotest.(check int)
    "payload[50]" 50
    (Bytes.get_uint8 (Bs.bytes r.ds_payload) (Bs.first r.ds_payload + 50))

let test_dep_bslice_roundtrip () =
  (* encode then decode: 2 bytes length + 4 bytes payload = 6 total *)
  let payload_data = Bytes.of_string "\x01\x02\x03\x04" in
  let original =
    { ds_length = 4; ds_payload = Bs.make payload_data ~first:0 ~length:4 }
  in
  let buf = Bytes.create 6 in
  Codec.encode dep_slice_codec original buf 0;
  (* Verify encoded length field *)
  Alcotest.(check int) "encoded length field" 4 (Bytes.get_uint16_be buf 0);
  (* Verify compute_wire_size reads the buffer correctly *)
  Alcotest.(check int)
    "compute_wire_size" 6
    (Codec.compute_wire_size dep_slice_codec buf 0);
  let decoded = Codec.decode dep_slice_codec buf 0 in
  Alcotest.(check int) "roundtrip length" 4 decoded.ds_length;
  Alcotest.(check int) "roundtrip payload len" 4 (Bs.length decoded.ds_payload);
  Alcotest.(check int)
    "roundtrip payload[0]" 0x01
    (Bytes.get_uint8
       (Bs.bytes decoded.ds_payload)
       (Bs.first decoded.ds_payload))

let test_dep_bslice_get_payload () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload =
    (Staged.unstage (Codec.get dep_slice_codec f_ds_payload)) buf 0
  in
  Alcotest.(check int) "get payload first" 2 (Bs.first payload);
  Alcotest.(check int) "get payload length" 4 (Bs.length payload);
  Alcotest.(check int)
    "get payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_dep_bslice_sub () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  let off = (Staged.unstage (Codec.sub dep_slice_codec f_ds_payload)) buf 0 in
  Alcotest.(check int) "sub offset" 2 off

let test_dep_bslice_set_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  (Staged.unstage (Codec.set dep_slice_codec f_ds_length)) buf 0 8;
  Alcotest.(check int)
    "set length" 8
    ((Staged.unstage (Codec.get dep_slice_codec f_ds_length)) buf 0)

let test_dep_bslice_get_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 42;
  Alcotest.(check int)
    "get length" 42
    ((Staged.unstage (Codec.get dep_slice_codec f_ds_length)) buf 0)

(* ── Dependent-size byte_array tests ── *)

type dep_array_record = { da_length : int; da_payload : string }

let f_da_length = Codec.field "Length" uint16be (fun r -> r.da_length)

let f_da_payload =
  Codec.field "Payload"
    (byte_array ~size:(Codec.ref f_da_length))
    (fun r -> r.da_payload)

let dep_array_codec =
  Codec.view "DepArray"
    (fun length payload -> { da_length = length; da_payload = payload })
    Codec.[ f_da_length; f_da_payload ]

let test_dep_byte_array_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 5;
  Bytes.blit_string "hello" 0 buf 2 5;
  let r = Codec.decode dep_array_codec buf 0 in
  Alcotest.(check int) "length" 5 r.da_length;
  Alcotest.(check string) "payload is string copy" "hello" r.da_payload

let test_dep_byte_array_roundtrip () =
  let original = { da_length = 3; da_payload = "abc" } in
  let buf = Bytes.create 5 in
  Codec.encode dep_array_codec original buf 0;
  let decoded = Codec.decode dep_array_codec buf 0 in
  Alcotest.(check int) "roundtrip length" 3 decoded.da_length;
  Alcotest.(check string) "roundtrip payload" "abc" decoded.da_payload

let test_dep_byte_array_get () =
  let buf = Bytes.create 5 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.blit_string "xyz" 0 buf 2 3;
  let payload =
    (Staged.unstage (Codec.get dep_array_codec f_da_payload)) buf 0
  in
  Alcotest.(check string) "get payload" "xyz" payload

(* ── Fixed field after variable field tests ── *)

type trailer_record = { tr_length : int; tr_payload : Bs.t; tr_checksum : int }

let f_tr_length = Codec.field "Length" uint16be (fun r -> r.tr_length)

let f_tr_payload =
  Codec.field "Payload"
    (byte_slice ~size:(Codec.ref f_tr_length))
    (fun r -> r.tr_payload)

let f_tr_checksum = Codec.field "Checksum" uint16be (fun r -> r.tr_checksum)

let trailer_codec =
  Codec.view "Trailer"
    (fun length payload checksum ->
      { tr_length = length; tr_payload = payload; tr_checksum = checksum })
    Codec.[ f_tr_length; f_tr_payload; f_tr_checksum ]

let test_dep_trailer_get_checksum () =
  (* [length:u16be=3] [payload:3 bytes] [checksum:u16be=0xBEEF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let checksum =
    (Staged.unstage (Codec.get trailer_codec f_tr_checksum)) buf 0
  in
  Alcotest.(check int) "get checksum" 0xBEEF checksum

let test_dep_trailer_set_checksum () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0x0000;
  (Staged.unstage (Codec.set trailer_codec f_tr_checksum)) buf 0 0xCAFE;
  Alcotest.(check int) "set checksum" 0xCAFE (Bytes.get_uint16_be buf 5)

let test_dep_trailer_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint16_be buf 5 0xDEAD;
  let r = Codec.decode trailer_codec buf 0 in
  Alcotest.(check int) "length" 3 r.tr_length;
  Alcotest.(check int) "payload length" 3 (Bs.length r.tr_payload);
  Alcotest.(check int) "payload first" 2 (Bs.first r.tr_payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes r.tr_payload) (Bs.first r.tr_payload));
  Alcotest.(check int) "checksum" 0xDEAD r.tr_checksum

let test_dep_trailer_roundtrip () =
  let payload_data = Bytes.of_string "\x01\x02" in
  let original =
    {
      tr_length = 2;
      tr_payload = Bs.make payload_data ~first:0 ~length:2;
      tr_checksum = 0x1234;
    }
  in
  let buf = Bytes.create 6 in
  Codec.encode trailer_codec original buf 0;
  let decoded = Codec.decode trailer_codec buf 0 in
  Alcotest.(check int) "rt length" 2 decoded.tr_length;
  Alcotest.(check int) "rt payload len" 2 (Bs.length decoded.tr_payload);
  Alcotest.(check int) "rt checksum" 0x1234 decoded.tr_checksum

(* ── wire_size API for variable codecs ── *)

let test_dep_is_fixed () =
  Alcotest.(check bool)
    "fixed codec is_fixed" true
    (Codec.is_fixed simple_record_codec);
  Alcotest.(check bool)
    "variable codec is_fixed" false
    (Codec.is_fixed dep_slice_codec);
  Alcotest.(check bool)
    "trailer codec is_fixed" false
    (Codec.is_fixed trailer_codec)

let test_dep_wire_size_raises () =
  (* wire_size raises Invalid_argument for variable-size codecs *)
  (match Codec.wire_size dep_slice_codec with
  | _ -> Alcotest.fail "expected Invalid_argument from wire_size"
  | exception Invalid_argument _ -> ());
  (match Codec.wire_size trailer_codec with
  | _ -> Alcotest.fail "expected Invalid_argument from wire_size"
  | exception Invalid_argument _ -> ());
  (* wire_size succeeds for fixed codecs *)
  Alcotest.(check int) "fixed wire_size" 7 (Codec.wire_size simple_record_codec)

let test_dep_min_wire_size () =
  (* min_wire_size for dep_slice_codec: just uint16be = 2 *)
  Alcotest.(check int) "dep_slice min" 2 (Codec.min_wire_size dep_slice_codec);
  (* min_wire_size for trailer_codec: uint16be + uint16be = 4 (variable payload excluded) *)
  Alcotest.(check int) "trailer min" 4 (Codec.min_wire_size trailer_codec);
  (* min_wire_size for fixed codec equals wire_size *)
  Alcotest.(check int)
    "fixed min"
    (Codec.wire_size simple_record_codec)
    (Codec.min_wire_size simple_record_codec)

let test_dep_compute_wire_size () =
  (* dep_slice: length=4 -> total = 2 + 4 = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Alcotest.(check int)
    "dep_slice compute" 6
    (Codec.compute_wire_size dep_slice_codec buf 0);
  (* dep_slice: length=0 -> total = 2 + 0 = 2 *)
  Bytes.set_uint16_be buf 0 0;
  Alcotest.(check int)
    "dep_slice compute 0" 2
    (Codec.compute_wire_size dep_slice_codec buf 0);
  (* trailer: length=3 -> total = 2 + 3 + 2 = 7 *)
  let buf2 = Bytes.create 7 in
  Bytes.set_uint16_be buf2 0 3;
  Alcotest.(check int)
    "trailer compute" 7
    (Codec.compute_wire_size trailer_codec buf2 0);
  (* fixed codec: compute returns fixed size without reading buffer *)
  Alcotest.(check int)
    "fixed compute"
    (Codec.wire_size simple_record_codec)
    (Codec.compute_wire_size simple_record_codec (Bytes.create 7) 0)

(* ── Codec.ref expression tests ── *)

let test_dep_codec_ref () =
  (* Codec.ref produces a valid expression used as byte_slice size *)
  let f_len = Codec.field "Len" uint8 (fun (l, _) -> l) in
  let f_data =
    Codec.field "Data" (byte_slice ~size:(Codec.ref f_len)) (fun (_, d) -> d)
  in
  let codec =
    Codec.view "RefTest" (fun len data -> (len, data)) Codec.[ f_len; f_data ]
  in
  (* buf: [len=5] [5 bytes payload] *)
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 5;
  for i = 0 to 4 do
    Bytes.set_uint8 buf (1 + i) (0x10 + i)
  done;
  let len, data = Codec.decode codec buf 0 in
  Alcotest.(check int) "ref len" 5 len;
  Alcotest.(check int) "ref data length" 5 (Bs.length data);
  Alcotest.(check int)
    "ref data[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data));
  Alcotest.(check int)
    "ref data[4]" 0x14
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data + 4))

let test_dep_ref_size_eval () =
  (* Test that the size expression is evaluated correctly for compute_wire_size *)
  let f_sz = Codec.field "Size" uint8 (fun (s, _) -> s) in
  let f_body =
    Codec.field "Body" (byte_slice ~size:(Codec.ref f_sz)) (fun (_, b) -> b)
  in
  let codec =
    Codec.view "RefSizeEval" (fun sz body -> (sz, body)) Codec.[ f_sz; f_body ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_uint8 buf 0 10;
  Alcotest.(check int) "compute size" 11 (Codec.compute_wire_size codec buf 0);
  Bytes.set_uint8 buf 0 0;
  Alcotest.(check int) "compute size 0" 1 (Codec.compute_wire_size codec buf 0)

(* ── to_struct for variable-size codecs ── *)

let test_dep_to_struct () =
  (* to_struct should produce a valid struct for variable-size codecs *)
  let s = Codec.to_struct dep_slice_codec in
  let m = module_ "DepSlice" [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output);
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output)

let test_dep_trailer_to_struct () =
  let s = Codec.to_struct trailer_codec in
  let m = module_ "Trailer" [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool)
    "contains Checksum" true
    (contains ~sub:"Checksum" output)

let suite =
  ( "wire",
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
      Alcotest.test_case "record: byte_array roundtrip" `Quick
        test_record_byte_array_roundtrip;
      Alcotest.test_case "record: byte_array padding" `Quick
        test_record_byte_array_padding;
      (* codec bitfields *)
      Alcotest.test_case "codec bitfield: wire_size" `Quick
        test_codec_bitfield_wire_size;
      Alcotest.test_case "codec bitfield: roundtrip" `Quick
        test_codec_bitfield_roundtrip;
      Alcotest.test_case "codec bitfield: byte layout" `Quick
        test_codec_bitfield_byte_layout;
      Alcotest.test_case "codec bitfield: decode" `Quick
        test_codec_bitfield_decode;
      Alcotest.test_case "codec bitfield: multi group" `Quick
        test_codec_bitfield_multi_group;
      Alcotest.test_case "codec bitfield: to_struct" `Quick
        test_codec_bitfield_to_struct;
      (* zero-copy view *)
      Alcotest.test_case "view: get uint" `Quick test_view_get_uint;
      Alcotest.test_case "view: get bitfield" `Quick test_view_get_bitfield;
      Alcotest.test_case "view: get bool" `Quick test_view_get_bool;
      Alcotest.test_case "view: set bitfield" `Quick test_view_set_bitfield;
      Alcotest.test_case "view: set uint" `Quick test_view_set_uint;
      Alcotest.test_case "view: set bool" `Quick test_view_set_bool;
      Alcotest.test_case "view: bounds check" `Quick test_view_bounds_check;
      Alcotest.test_case "view: with offset" `Quick test_view_with_offset;
      (* field sharing *)
      Alcotest.test_case "view: shared field spec" `Quick
        test_view_shared_field_spec;
      Alcotest.test_case "view: shared bitfield spec" `Quick
        test_view_shared_bitfield_spec;
      Alcotest.test_case "view: shared set independent" `Quick
        test_view_shared_set_independent;
      (* byte_slice *)
      Alcotest.test_case "view: byte_slice get" `Quick test_view_byte_slice_get;
      Alcotest.test_case "view: byte_slice decode" `Quick
        test_view_byte_slice_decode;
      Alcotest.test_case "view: byte_slice nested" `Quick
        test_view_byte_slice_nested;
      (* raw access: get / set / sub *)
      Alcotest.test_case "raw: get uint" `Quick test_raw_get_uint;
      Alcotest.test_case "raw: get bitfield" `Quick test_raw_get_bitfield;
      Alcotest.test_case "raw: set uint" `Quick test_raw_set_uint;
      Alcotest.test_case "raw: set bitfield" `Quick test_raw_set_bitfield;
      Alcotest.test_case "raw: sub nested" `Quick test_raw_sub_nested;
      Alcotest.test_case "raw: sub 3 layers" `Quick test_raw_sub_three_layers;
      Alcotest.test_case "raw: with offset" `Quick test_raw_with_offset;
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
      (* ffi stubs *)
      Alcotest.test_case "ffi: c_stubs" `Quick test_c_stubs;
      (* dependent-size byte_slice *)
      Alcotest.test_case "dep: byte_slice decode empty" `Quick
        test_dep_bslice_decode_empty;
      Alcotest.test_case "dep: byte_slice decode 4" `Quick
        test_dep_bslice_decode_4;
      Alcotest.test_case "dep: byte_slice decode 100" `Quick
        test_dep_bslice_decode_100;
      Alcotest.test_case "dep: byte_slice roundtrip" `Quick
        test_dep_bslice_roundtrip;
      Alcotest.test_case "dep: byte_slice get payload" `Quick
        test_dep_bslice_get_payload;
      Alcotest.test_case "dep: byte_slice sub" `Quick test_dep_bslice_sub;
      Alcotest.test_case "dep: byte_slice set length" `Quick
        test_dep_bslice_set_length;
      Alcotest.test_case "dep: byte_slice get length" `Quick
        test_dep_bslice_get_length;
      (* dependent-size byte_array *)
      Alcotest.test_case "dep: byte_array decode" `Quick
        test_dep_byte_array_decode;
      Alcotest.test_case "dep: byte_array roundtrip" `Quick
        test_dep_byte_array_roundtrip;
      Alcotest.test_case "dep: byte_array get" `Quick test_dep_byte_array_get;
      (* fixed field after variable field *)
      Alcotest.test_case "dep: fixed after variable get checksum" `Quick
        test_dep_trailer_get_checksum;
      Alcotest.test_case "dep: fixed after variable set checksum" `Quick
        test_dep_trailer_set_checksum;
      Alcotest.test_case "dep: fixed after variable decode" `Quick
        test_dep_trailer_decode;
      Alcotest.test_case "dep: fixed after variable roundtrip" `Quick
        test_dep_trailer_roundtrip;
      (* wire_size API for variable codecs *)
      Alcotest.test_case "dep: is_fixed" `Quick test_dep_is_fixed;
      Alcotest.test_case "dep: wire_size raises" `Quick
        test_dep_wire_size_raises;
      Alcotest.test_case "dep: min_wire_size" `Quick test_dep_min_wire_size;
      Alcotest.test_case "dep: compute_wire_size" `Quick
        test_dep_compute_wire_size;
      (* Codec.ref expressions *)
      Alcotest.test_case "dep: codec ref" `Quick test_dep_codec_ref;
      Alcotest.test_case "dep: codec ref size eval" `Quick
        test_dep_ref_size_eval;
      (* to_struct for variable-size codecs *)
      Alcotest.test_case "dep: to_struct" `Quick test_dep_to_struct;
      Alcotest.test_case "dep: trailer to_struct" `Quick
        test_dep_trailer_to_struct;
    ] )
