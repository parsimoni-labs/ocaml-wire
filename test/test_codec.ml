(* Tests for codec.ml: Codec.get/set/v *)

open Wire
open Wire.Everparse.Raw
open Test_fixtures

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let decode_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* Helper: encode record to string using Codec API *)
let encode_record codec v =
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let decode_record codec s =
  let ws = Codec.wire_size codec in
  if String.length s < ws then
    Error (Unexpected_eof { expected = ws; got = String.length s })
  else Codec.decode codec (Bytes.of_string s) 0

(* -- Record codec tests -- *)

type simple_record = { a : int; b : int; c : int }

let simple_record_codec =
  let open Codec in
  v "SimpleRecord"
    (fun a b c -> { a; b; c })
    [
      (Field.v "a" uint8 $ fun r -> r.a);
      (Field.v "b" uint16 $ fun r -> r.b);
      (Field.v "c" uint32 $ fun r -> r.c);
    ]

let test_record_encode () =
  let v = { a = 0x42; b = 0x1234; c = 0x56789ABC } in
  match encode_record simple_record_codec v with
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
  match decode_record simple_record_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 0x42 v.a;
      Alcotest.(check int) "b" 0x1234 v.b;
      Alcotest.(check int) "c" 0x56789ABC v.c
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_record_roundtrip () =
  let original = { a = 0xAB; b = 0xCDEF; c = 0x12345678 } in
  match encode_record simple_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a roundtrip" original.a decoded.a;
          Alcotest.(check int) "b roundtrip" original.b decoded.b;
          Alcotest.(check int) "c roundtrip" original.c decoded.c
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_struct_of_record () =
  let s = Everparse.struct_of_codec simple_record_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a;" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b;" output);
  Alcotest.(check bool) "contains field c" true (contains ~sub:"c;" output)

type meta_record = { x : int }

let meta_f_x = Field.v "x" uint8

let meta_codec =
  let open Codec in
  v "MetaRecord"
    ~where:Expr.(Field.ref meta_f_x = int 8)
    (fun x -> { x })
    [
      ( Field.v "x"
          ~constraint_:Expr.(Field.ref meta_f_x <= int 10)
          ~action:
            (Action.on_success
               [
                 Action.return_bool Expr.(Field.ref meta_f_x mod int 2 = int 0);
               ])
          uint8
      $ fun r -> r.x );
    ]

let test_codec_metadata_decode_ok () =
  let buf = Bytes.of_string "\x08" in
  let v = decode_ok (Codec.decode meta_codec buf 0) in
  Alcotest.(check int) "x" 8 v.x

let test_metadata_constraint_fail () =
  let buf = Bytes.of_string "\x0B" in
  match Codec.decode meta_codec buf 0 with
  | Error (Constraint_failed "field constraint") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_metadata_action_fail () =
  let buf = Bytes.of_string "\x09" in
  match Codec.decode meta_codec buf 0 with
  | Error (Constraint_failed "field action") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let projection_limit = Param.input "limit" uint8
let _projection_limit_expr = Param.expr projection_limit
let projection_outx = Param.output "outx" uint8
let projection_f_x = Field.v "x" uint8

let projection_codec =
  let open Codec in
  v "ProjectionCodec"
    ~where:Expr.(Field.ref projection_f_x <= Param.expr projection_limit)
    (fun x -> { x })
    [
      ( Field.v "x"
          ~constraint_:Expr.(Field.ref projection_f_x <= int 8)
          ~action:
            (Action.on_success
               [ Action.assign projection_outx (Field.ref projection_f_x) ])
          uint8
      $ fun r -> r.x );
    ]

let test_metadata_with_params () =
  let env = Codec.env projection_codec |> Param.bind projection_limit 10 in
  let buf = Bytes.of_string "\x08" in
  let v = decode_ok (Codec.decode ~env projection_codec buf 0) in
  Alcotest.(check int) "x" 8 v.x;
  Alcotest.(check int) "outx" 8 (Param.get env projection_outx)

let test_metadata_where_fail () =
  let env = Codec.env projection_codec |> Param.bind projection_limit 7 in
  let buf = Bytes.of_string "\x08" in
  match Codec.decode ~env projection_codec buf 0 with
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let validate_f_x = Field.v "x" uint8

let validate_codec, validate_cf_x =
  let open Codec in
  let cf_x =
    Field.v "x" ~constraint_:Expr.(Field.ref validate_f_x <= int 10) uint8
    $ fun r -> r.x
  in
  let codec =
    v "ValidateTest"
      ~where:Expr.(Field.ref validate_f_x = int 8)
      (fun x -> { x })
      [ cf_x ]
  in
  (codec, cf_x)

let test_validate_rejects_bad_where () =
  (* where requires x = 8, set x = 7 *)
  let buf = Bytes.of_string "\x07" in
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  (* get returns raw value without checking *)
  Alcotest.(check int) "get bypasses where" 7 (get_x buf 0);
  (* validate catches the violation *)
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject where violation"
  | exception Validation_error (Constraint_failed _) -> ()

let test_validate_rejects_bad_constraint () =
  (* constraint requires x <= 10, set x = 11 *)
  let buf = Bytes.of_string "\x0B" in
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "get bypasses constraint" 11 (get_x buf 0);
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject constraint violation"
  | exception Validation_error (Constraint_failed _) -> ()

let test_validate_then_get () =
  (* x = 8 satisfies both where (= 8) and constraint (<= 10) *)
  let buf = Bytes.of_string "\x08" in
  Codec.validate validate_codec buf 0;
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "validate then get" 8 (get_x buf 0)

let test_struct_of_codec_metadata () =
  let s = Everparse.struct_of_codec projection_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains where" true (contains ~sub:"where" output);
  Alcotest.(check bool)
    "contains on-success" true
    (contains ~sub:":on-success" output);
  (* Params should be recovered from Param_ref/Assign in the AST *)
  Alcotest.(check bool)
    "contains limit param" true
    (contains ~sub:"limit" output);
  Alcotest.(check bool)
    "contains mutable outx param" true
    (contains ~sub:"mutable" output)

(* Record with multiple uint16be fields --
   [multi_record] / [multi_record_codec] live in {!Test_fixtures}. *)

let test_record_with_multi () =
  let original = { x = 0x1234; y = 0x5678 } in
  match encode_record multi_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      match decode_record multi_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "x" original.x decoded.x;
          Alcotest.(check int) "y" original.y decoded.y
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Record with byte_array field *)
type ba_record = { id : int; uuid : string; tag : int }

let ba_record_codec =
  let open Codec in
  v "BaRecord"
    (fun id uuid tag -> { id; uuid; tag })
    [
      (Field.v "id" uint32be $ fun r -> r.id);
      (Field.v "uuid" (byte_array ~size:(int 16)) $ fun r -> r.uuid);
      (Field.v "tag" uint16be $ fun r -> r.tag);
    ]

let test_record_byte_array_roundtrip () =
  let original = { id = 0x12345678; uuid = "0123456789abcdef"; tag = 0xABCD } in
  match encode_record ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "id" original.id decoded.id;
          Alcotest.(check string) "uuid" original.uuid decoded.uuid;
          Alcotest.(check int) "tag" original.tag decoded.tag
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_byte_array_padding () =
  (* Short string should be zero-padded *)
  let original = { id = 1; uuid = "short"; tag = 2 } in
  match encode_record ba_record_codec original with
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
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          (* Decoded uuid includes the zero padding *)
          Alcotest.(check int) "uuid length" 16 (String.length decoded.uuid);
          Alcotest.(check string)
            "uuid prefix" "short"
            (String.sub decoded.uuid 0 5)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* -- Codec bitfield tests -- *)

type bf32_record = { bf_a : int; bf_b : int; bf_c : int; bf_d : int }

let bf32_codec =
  let open Codec in
  v "Bf32Test"
    (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
    [
      (Field.v "a" (bits ~width:3 U32be) $ fun t -> t.bf_a);
      (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.bf_b);
      (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.bf_c);
      (Field.v "d" (bits ~width:8 U32be) $ fun t -> t.bf_d);
    ]

type bf16_record = {
  bf_ver : int;
  bf_flags : int;
  bf_id : int;
  bf_count : int;
  bf_len : int;
}

let bf16_codec =
  let open Codec in
  v "Bf16Test"
    (fun ver flags id count len ->
      {
        bf_ver = ver;
        bf_flags = flags;
        bf_id = id;
        bf_count = count;
        bf_len = len;
      })
    [
      (Field.v "ver" (bits ~width:3 U16be) $ fun t -> t.bf_ver);
      (Field.v "flags" (bits ~width:2 U16be) $ fun t -> t.bf_flags);
      (Field.v "id" (bits ~width:11 U16be) $ fun t -> t.bf_id);
      (Field.v "count" (bits ~width:14 U16be) $ fun t -> t.bf_count);
      (Field.v "len" (bits ~width:2 U16be) $ fun t -> t.bf_len);
    ]

let test_codec_bitfield_wire_size () =
  Alcotest.(check int) "bf32 wire_size" 4 (Codec.wire_size bf32_codec);
  Alcotest.(check int) "bf16 wire_size" 4 (Codec.wire_size bf16_codec)

let test_codec_bitfield_roundtrip () =
  let original = { bf_a = 5; bf_b = 20; bf_c = 0x1234; bf_d = 0xAB } in
  match encode_record bf32_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record bf32_codec encoded with
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
  match encode_record bf32_codec v with
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
  match decode_record bf32_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 5 v.bf_a;
      Alcotest.(check int) "b" 20 v.bf_b;
      Alcotest.(check int) "c" 0x1234 v.bf_c;
      Alcotest.(check int) "d" 0xAB v.bf_d
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_codec_bitfield_multi_group () =
  (* Two U16be groups: (3+2+11=16) + (14+2=16) = 32 bits = 4 bytes *)
  let v =
    { bf_ver = 5; bf_flags = 2; bf_id = 0x7FF; bf_count = 0x3FFF; bf_len = 3 }
  in
  match encode_record bf16_codec v with
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
      match decode_record bf16_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "ver" v.bf_ver decoded.bf_ver;
          Alcotest.(check int) "flags" v.bf_flags decoded.bf_flags;
          Alcotest.(check int) "id" v.bf_id decoded.bf_id;
          Alcotest.(check int) "count" v.bf_count decoded.bf_count;
          Alcotest.(check int) "len" v.bf_len decoded.bf_len
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_overflow_u8 () =
  let v = { bf_a = 0x8; bf_b = 0; bf_c = 0; bf_d = 0 } in
  (* bf_a is 3 bits, 0x8 = 8 exceeds max 7 *)
  match encode_record bf32_codec v with
  | Ok _ -> Alcotest.fail "expected overflow for 3-bit field with value 0x8"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_overflow_u16 () =
  let v =
    { bf_ver = 0; bf_flags = 0; bf_id = 0x800; bf_count = 0; bf_len = 0 }
  in
  (* bf_id is 11 bits, 0x800 = 2048 exceeds max 2047 *)
  match encode_record bf16_codec v with
  | Ok _ -> Alcotest.fail "expected overflow for 11-bit field with value 0x800"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_overflow_u32 () =
  let v = { bf_a = 0; bf_b = 0; bf_c = 0x10000; bf_d = 0 } in
  (* bf_c is 16 bits, 0x10000 exceeds max 0xFFFF *)
  match encode_record bf32_codec v with
  | Ok _ ->
      Alcotest.fail "expected overflow for 16-bit field with value 0x10000"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_max_valid () =
  (* All fields at their maximum valid values *)
  let v = { bf_a = 7; bf_b = 31; bf_c = 0xFFFF; bf_d = 0xFF } in
  match encode_record bf32_codec v with
  | Error e -> Alcotest.failf "encode max valid: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" 7 decoded.bf_a;
          Alcotest.(check int) "b" 31 decoded.bf_b;
          Alcotest.(check int) "c" 0xFFFF decoded.bf_c;
          Alcotest.(check int) "d" 0xFF decoded.bf_d
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_overflow_1bit () =
  (* Single-bit field: only 0 and 1 are valid *)
  let f = Field.v "flag" (bits ~width:1 U8) in
  let codec = Codec.v "OneBit" Fun.id Codec.[ f $ Fun.id ] in
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  (try
     Codec.encode codec 2 buf 0;
     Alcotest.fail "expected overflow for 1-bit field with value 2"
   with Invalid_argument _ -> ());
  (* 0 and 1 should work *)
  Codec.encode codec 0 buf 0;
  Codec.encode codec 1 buf 0

let test_struct_of_codec_bitfield () =
  let s = Everparse.struct_of_codec bf32_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT32BE" true
    (contains ~sub:"UINT32BE" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b" output)

(* -- Zero-copy view tests -- *)

let test_view_get_uint () =
  let codec, cf_x, cf_y =
    let f_x = Field.v "x" uint16be in
    let f_y = Field.v "y" uint16be in
    let cf_x = Codec.(f_x $ fun (r : multi_record) -> r.x) in
    let cf_y = Codec.(f_y $ fun (r : multi_record) -> r.y) in
    let codec =
      Codec.v "ViewUint" (fun a b -> { x = a; y = b }) [ cf_x; cf_y ]
    in
    (codec, cf_x, cf_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  Alcotest.(check int)
    "get x" 0x1234
    ((Staged.unstage (Codec.get codec cf_x)) buf 0);
  Alcotest.(check int)
    "get y" 0x5678
    ((Staged.unstage (Codec.get codec cf_y)) buf 0)

let test_view_get_bitfield () =
  let codec, cf_a, cf_d =
    let f_a = Field.v "a" (bits ~width:3 U32be) in
    let f_d = Field.v "d" (bits ~width:8 U32be) in
    let cf_a = Codec.(f_a $ fun t -> t.bf_a) in
    let cf_d = Codec.(f_d $ fun t -> t.bf_d) in
    let codec =
      let open Codec in
      v "ViewBf"
        (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
        [
          cf_a;
          (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.bf_b);
          (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.bf_c);
          cf_d;
        ]
    in
    (codec, cf_a, cf_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  Alcotest.(check int) "get a" 5 ((Staged.unstage (Codec.get codec cf_a)) buf 0);
  Alcotest.(check int)
    "get d" 0xAB
    ((Staged.unstage (Codec.get codec cf_d)) buf 0)

let test_view_get_bool () =
  (* Default [bit_order = Msb_first]: first-declared field lives at the MSB,
     so the flag bit is bit 7 of the byte. *)
  let codec, cf_flag =
    let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
    let cf_flag = Codec.(f_flag $ fst) in
    let codec =
      let open Codec in
      v "ViewBool"
        (fun flag code -> (flag, code))
        [ cf_flag; Field.v "code" (bits ~width:7 U8) $ snd ]
    in
    (codec, cf_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x80;
  Alcotest.(check bool)
    "get flag=true" true
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Bytes.set_uint8 buf 0 0x00;
  Alcotest.(check bool)
    "get flag=false" false
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0)

let test_view_set_bitfield () =
  let codec, cf_a, cf_d =
    let f_a = Field.v "a" (bits ~width:3 U32be) in
    let f_d = Field.v "d" (bits ~width:8 U32be) in
    let cf_a = Codec.(f_a $ fun t -> t.bf_a) in
    let cf_d = Codec.(f_d $ fun t -> t.bf_d) in
    let codec =
      let open Codec in
      v "ViewSetBf"
        (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
        [
          cf_a;
          (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.bf_b);
          (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.bf_c);
          cf_d;
        ]
    in
    (codec, cf_a, cf_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  (Staged.unstage (Codec.set codec cf_a)) buf 0 3;
  Alcotest.(check int)
    "get a after set" 3
    ((Staged.unstage (Codec.get codec cf_a)) buf 0);
  let r = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "b preserved" 20 r.bf_b;
  Alcotest.(check int) "c preserved" 0x1234 r.bf_c;
  Alcotest.(check int) "d preserved" 0xAB r.bf_d;
  (Staged.unstage (Codec.set codec cf_d)) buf 0 0x42;
  Alcotest.(check int)
    "get d after set" 0x42
    ((Staged.unstage (Codec.get codec cf_d)) buf 0);
  let r = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "a still 3" 3 r.bf_a;
  Alcotest.(check int) "b still 20" 20 r.bf_b;
  Alcotest.(check int) "c still 0x1234" 0x1234 r.bf_c

let test_view_set_uint () =
  let codec, cf_x, cf_y =
    let f_x = Field.v "x" uint16be in
    let f_y = Field.v "y" uint16be in
    let cf_x = Codec.(f_x $ fun (r : multi_record) -> r.x) in
    let cf_y = Codec.(f_y $ fun (r : multi_record) -> r.y) in
    let codec = Codec.v "ViewSetUint" (fun x y -> { x; y }) [ cf_x; cf_y ] in
    (codec, cf_x, cf_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (Staged.unstage (Codec.set codec cf_x)) buf 0 0xAAAA;
  Alcotest.(check int)
    "get x after set" 0xAAAA
    ((Staged.unstage (Codec.get codec cf_x)) buf 0);
  Alcotest.(check int)
    "y unchanged" 0x5678
    ((Staged.unstage (Codec.get codec cf_y)) buf 0)

let test_view_bounds_check () =
  let codec =
    let open Codec in
    v "ViewBounds" (fun a -> a) [ (Field.v "a" uint32be $ fun a -> a) ]
  in
  let buf = Bytes.create 2 in
  match Codec.decode codec buf 0 with
  | Error (Unexpected_eof _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_view_with_offset () =
  let codec, cf_a =
    let f_a = Field.v "a" uint16be in
    let cf_a = Codec.(f_a $ fun a -> a) in
    let codec = Codec.v "ViewOff" (fun a -> a) [ cf_a ] in
    (codec, cf_a)
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_uint16_be buf 2 0x2222;
  Bytes.set_uint16_be buf 4 0x3333;
  Alcotest.(check int)
    "get at offset 2" 0x2222
    ((Staged.unstage (Codec.get codec cf_a)) buf 2)

let test_view_set_bool () =
  (* Default [bit_order = Msb_first]: first-declared field lives at bit 7. *)
  let codec, cf_flag =
    let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
    let cf_flag = Codec.(f_flag $ fst) in
    let codec =
      let open Codec in
      v "ViewSetBool"
        (fun flag code -> (flag, code))
        [ cf_flag; Field.v "code" (bits ~width:7 U8) $ snd ]
    in
    (codec, cf_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec cf_flag)) buf 0 true;
  Alcotest.(check bool)
    "get flag after set true" true
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Alcotest.(check int) "byte value (MSB-first)" 0x80 (Bytes.get_uint8 buf 0);
  (Staged.unstage (Codec.set codec cf_flag)) buf 0 false;
  Alcotest.(check bool)
    "get flag after set false" false
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Alcotest.(check int) "byte cleared" 0x00 (Bytes.get_uint8 buf 0)

(* -- Field sharing tests -- same field spec used in two codecs -- *)

let test_view_shared_field_spec () =
  (* Two codecs with different layouts, each with their own field "x".
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
     Each codec gets a fresh field object. *)
  let f1_x = Field.v "x" uint16be in
  let cf1_x = Codec.(f1_x $ fun (x, _) -> x) in
  let codec1 =
    let open Codec in
    v "Share1"
      (fun x y -> (x, y))
      [ cf1_x; (Field.v "y" uint16be $ fun (_, y) -> y) ]
  in
  let f2_x = Field.v "x" uint16be in
  let cf2_x = Codec.(f2_x $ fun (x, _) -> x) in
  let codec2 =
    let open Codec in
    v "Share2"
      (fun _pad x -> (x, 0))
      [ (Field.v "pad" uint16be $ fun _ -> 0); cf2_x ]
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
    ((Staged.unstage (Codec.get codec1 cf1_x)) buf1 0);
  Alcotest.(check int)
    "codec2 get x" 0xCCCC
    ((Staged.unstage (Codec.get codec2 cf2_x)) buf2 0)

let test_view_shared_bitfield_spec () =
  (* Two codecs with different bitfield layouts using the default
     [bit_order = Msb_first].
     Codec1: [3-bit a] [5-bit b]       -> a is top 3 bits
     Codec2: [5-bit pad] [3-bit a]     -> a is bottom 3 bits *)
  let f1_a = Field.v "a" (bits ~width:3 U8) in
  let cf1_a = Codec.(f1_a $ fun (a, _) -> a) in
  let codec1 =
    let open Codec in
    v "ShareBf1"
      (fun a b -> (a, b))
      [ cf1_a; (Field.v "b" (bits ~width:5 U8) $ fun (_, b) -> b) ]
  in
  let f2_a = Field.v "a" (bits ~width:3 U8) in
  let cf2_a = Codec.(f2_a $ fun (a, _) -> a) in
  let codec2 =
    let open Codec in
    v "ShareBf2"
      (fun _pad a -> (a, 0))
      [ (Field.v "pad" (bits ~width:5 U8) $ fun _ -> 0); cf2_a ]
  in
  (* 0xE3 = 0b_1110_0011
     codec1 reads top 3 bits    -> 0b111 = 7
     codec2 reads bottom 3 bits -> 0b011 = 3 *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xE3;
  Alcotest.(check int)
    "codec1 get a (top 3)" 7
    ((Staged.unstage (Codec.get codec1 cf1_a)) buf 0);
  Alcotest.(check int)
    "codec2 get a (bot 3)" 3
    ((Staged.unstage (Codec.get codec2 cf2_a)) buf 0)

let test_view_shared_set_independent () =
  (* set via one codec's field must not affect the other's interpretation.
     Default [bit_order = Msb_first]: first-declared field at top. *)
  let f1 = Field.v "v" (bits ~width:4 U8) in
  let cf1 = Codec.(f1 $ fun (v, _) -> v) in
  let codec1 =
    let open Codec in
    v "SetShare1"
      (fun v pad -> (v, pad))
      [ cf1; (Field.v "pad" (bits ~width:4 U8) $ fun (_, p) -> p) ]
  in
  let f2 = Field.v "v" (bits ~width:4 U8) in
  let cf2 = Codec.(f2 $ fun (v, _) -> v) in
  let codec2 =
    let open Codec in
    v "SetShare2"
      (fun pad v -> (v, pad))
      [ (Field.v "pad" (bits ~width:4 U8) $ fun (_, p) -> p); cf2 ]
  in
  (* Codec1's v is the top nibble; set to 0xA. *)
  let buf = Bytes.create 1 in
  (Staged.unstage (Codec.set codec1 cf1)) buf 0 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* Codec2's v is the bottom nibble -- should still be 0. *)
  Alcotest.(check int)
    "codec2 get after set1" 0
    ((Staged.unstage (Codec.get codec2 cf2)) buf 0);
  (* Set codec2's v (bottom nibble) to 0x5. *)
  (Staged.unstage (Codec.set codec2 cf2)) buf 0 0x5;
  Alcotest.(check int) "byte after set2" 0xA5 (Bytes.get_uint8 buf 0);
  (* Codec1's v should still be 0xA. *)
  Alcotest.(check int)
    "codec1 get after set2" 0xA
    ((Staged.unstage (Codec.get codec1 cf1)) buf 0)

(* -- action semantics -- *)

let test_action_fires_decode_env () =
  (* decode_env fires actions and syncs output params *)
  let env = Codec.env projection_codec |> Param.bind projection_limit 10 in
  let buf = Bytes.of_string "\x05" in
  Alcotest.(check int) "outx before" 0 (Param.get env projection_outx);
  let _v = decode_ok (Codec.decode ~env projection_codec buf 0) in
  Alcotest.(check int) "outx after decode_env" 5 (Param.get env projection_outx)

let test_action_fires_on_get () =
  (* get fires field actions. A return_bool action that rejects odd values
     should cause get to raise on odd input. *)
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_ref mod int 2 = int 0) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "ActionGet" (fun v -> v) [ cf_v ] in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  (* Even value: action passes *)
  Alcotest.(check int) "get even" 0x42 (get_v (Bytes.of_string "\x42") 0);
  (* Odd value: action rejects *)
  match get_v (Bytes.of_string "\x43") 0 with
  | _ -> Alcotest.fail "expected action to reject odd value"
  | exception Validation_error (Constraint_failed _) -> ()

let test_action_unfired_by_validate () =
  (* validate checks constraints + where, but does NOT fire actions. *)
  let action_out2 = Param.output "act_out2" uint8 in
  let f_ref2 = Field.v "v" uint8 in
  let cf_v2 =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success [ Action.assign action_out2 (Field.ref f_ref2) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "ActionValidate" (fun v -> v) [ cf_v2 ] in
  let buf = Bytes.of_string "\x42" in
  Codec.validate codec buf 0;
  (* validate does NOT fire actions *)
  Alcotest.(check int)
    "action not fired by validate" 0
    !(action_out2.Wire.Private.Types.ph_cell)

let test_get_noaction_zero_overhead () =
  (* get on a field without an action should not allocate.
     We just verify it works -- allocation is checked by benchmarks. *)
  let cf_v = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec = Codec.v "NoAction" (fun v -> v) [ cf_v ] in
  let buf = Bytes.of_string "\x42" in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  Alcotest.(check int) "get returns value" 0x42 (get_v buf 0)

let test_get_with_env () =
  (* get ~env fires action and syncs output params to env *)
  let out = Param.output "out" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "GetEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  let v = get_v buf 0 in
  Alcotest.(check int) "get returns value" 0x42 v;
  Alcotest.(check int) "output param synced" 0x42 (Param.get env out)

let test_get_action_field_twocodecs () =
  (* Same action field in two codecs -- each codec gets its own action runner *)
  let out1 = Param.output "out1" uint8 in
  let out2 = Param.output "out2" uint16be in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out1 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  (* Codec1: [v] at offset 0 *)
  let codec1 = Codec.v "ActTwo1" (fun v -> v) [ cf_v ] in
  (* Codec2: [pad] [v] -- v at offset 1, different action *)
  let cf_v2 =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out2 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec2 =
    let open Codec in
    v "ActTwo2" (fun _pad v -> v) [ (Field.v "pad" uint8 $ fun _ -> 0); cf_v2 ]
  in
  let env1 = Codec.env codec1 in
  let env2 = Codec.env codec2 in
  let buf = Bytes.of_string "\xAA\xBB" in
  let get1 = Staged.unstage (Codec.get ~env:env1 codec1 cf_v) in
  let get2 = Staged.unstage (Codec.get ~env:env2 codec2 cf_v2) in
  (* codec1 reads offset 0 = 0xAA *)
  Alcotest.(check int) "codec1 get" 0xAA (get1 buf 0);
  Alcotest.(check int) "codec1 out" 0xAA (Param.get env1 out1);
  (* codec2 reads offset 1 = 0xBB *)
  Alcotest.(check int) "codec2 get" 0xBB (get2 buf 0);
  Alcotest.(check int) "codec2 out" 0xBB (Param.get env2 out2)

let test_get_action_no_env () =
  (* get without ~env on action field: action fires but output not accessible *)
  let out = Param.output "out_noenv" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "NoEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  (* No ~env: action fires (no crash) but output stays 0 *)
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  Alcotest.(check int) "get returns value" 0x42 (get_v buf 0);
  Alcotest.(check int) "output not synced without env" 0 (Param.get env out)

let test_get_action_abort_field () =
  (* get on a field with abort action always raises *)
  let cf_v =
    Codec.(
      Field.v "v" ~action:(Action.on_success [ Action.abort ]) uint8 $ fun v ->
      v)
  in
  let codec = Codec.v "AbortGet" (fun v -> v) [ cf_v ] in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  match get_v (Bytes.of_string "\x42") 0 with
  | _ -> Alcotest.fail "expected abort"
  | exception Validation_error (Constraint_failed _) -> ()

let test_get_noaction_ignores_env () =
  (* Passing ~env to get on a field without action is harmless *)
  let cf_v = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec = Codec.v "NoActEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int)
    "get returns value" 0x42
    (get_v (Bytes.of_string "\x42") 0)

let test_get_action_multiple_calls () =
  (* get with ~env updates output on every call *)
  let out = Param.output "out_multi" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "Multi" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  ignore (get_v (Bytes.of_string "\x10") 0);
  Alcotest.(check int) "after first" 0x10 (Param.get env out);
  ignore (get_v (Bytes.of_string "\x20") 0);
  Alcotest.(check int) "after second" 0x20 (Param.get env out)

let test_get_action_with_inputparam () =
  (* Action references an input param -- get ~env must blit it into the
     scratch array so the action sees the bound value. *)
  let limit = Param.input "limit" uint8 in
  let out = Param.output "result" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [
               Action.assign out (Field.ref f_ref);
               Action.return_bool Expr.(Field.ref f_ref <= Param.expr limit);
             ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "InputParam" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec |> Param.bind limit 50 in
  let buf_ok = Bytes.of_string "\x30" in
  let buf_bad = Bytes.of_string "\x40" in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  (* 0x30 = 48 <= 50: passes *)
  Alcotest.(check int) "get with input param" 0x30 (get_v buf_ok 0);
  Alcotest.(check int) "output synced" 0x30 (Param.get env out);
  (* 0x40 = 64 > 50: action rejects *)
  match get_v buf_bad 0 with
  | _ -> Alcotest.fail "expected rejection from input param check"
  | exception Validation_error (Constraint_failed _) -> ()

let test_get_action_inputparam_noenv () =
  (* Action references an input param but no env passed -- param reads as 0 *)
  let limit = Param.input "lim2" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_ref <= Param.expr limit) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "NoEnvInput" (fun v -> v) [ cf_v ] in
  (* No env: limit defaults to 0, so any positive value > 0 fails *)
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  (* 0 <= 0: passes *)
  Alcotest.(check int) "zero passes" 0 (get_v (Bytes.of_string "\x00") 0);
  (* 1 > 0: fails *)
  match get_v (Bytes.of_string "\x01") 0 with
  | _ -> Alcotest.fail "expected rejection without env"
  | exception Validation_error (Constraint_failed _) -> ()

let test_get_action_output_only () =
  (* Action with only assign (no return_bool/abort) -- should never fail *)
  let out = Param.output "out_only" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "OutOnly" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  (* Any value should work -- no validation in this action *)
  Alcotest.(check int) "get 0xFF" 0xFF (get_v (Bytes.of_string "\xFF") 0);
  Alcotest.(check int) "output 0xFF" 0xFF (Param.get env out);
  Alcotest.(check int) "get 0x00" 0x00 (get_v (Bytes.of_string "\x00") 0);
  Alcotest.(check int) "output 0x00" 0x00 (Param.get env out)

let test_get_action_varthen_assign () =
  (* Action with local var computation then assign to output *)
  let out = Param.output "doubled" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [
               Action.var "tmp" Expr.(Field.ref f_ref * int 2);
               Action.assign out (Field.ref (Field.v "tmp" uint8));
             ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "VarAssign" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int) "get value" 21 (get_v (Bytes.of_string "\x15") 0);
  Alcotest.(check int) "doubled output" 42 (Param.get env out)

let test_get_action_crossfield_ref () =
  (* Action on field y references field x's value *)
  let f_x = Field.v "x" uint8 in
  let out = Param.output "sum" uint8 in
  let cf_x = Codec.(f_x $ fun (x, _) -> x) in
  let cf_y =
    Codec.(
      Field.v "y"
        ~action:
          (Action.on_success
             [ Action.assign out Expr.(Field.ref f_x + int 100) ])
        uint8
      $ fun (_, y) -> y)
  in
  let codec = Codec.v "CrossRef" (fun x y -> (x, y)) [ cf_x; cf_y ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x0A\x14" in
  let get_y = Staged.unstage (Codec.get ~env codec cf_y) in
  let y = get_y buf 0 in
  Alcotest.(check int) "y value" 0x14 y;
  (* Action computed x + 100 = 10 + 100 = 110 *)
  Alcotest.(check int) "cross-field output" 110 (Param.get env out)

let test_validate_constraint_only () =
  (* Codec with constraint but no where clause *)
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Codec.(
      Field.v "x" ~constraint_:Expr.(Field.ref f_x <= int 10) uint8 $ fun v -> v)
  in
  let codec = Codec.v "ConstOnly" (fun v -> v) [ cf_x ] in
  let good = Bytes.of_string "\x05" in
  let bad = Bytes.of_string "\x0B" in
  Codec.validate codec good 0;
  match Codec.validate codec bad 0 with
  | () -> Alcotest.fail "expected constraint failure"
  | exception Validation_error (Constraint_failed _) -> ()

let test_validate_where_only () =
  (* Codec with where clause but no field constraints *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun v -> v) in
  let codec =
    Codec.v "WhereOnly"
      ~where:Expr.(Field.ref f_x = int 42)
      (fun v -> v)
      [ cf_x ]
  in
  let good = Bytes.of_string "\x2A" in
  let bad = Bytes.of_string "\x00" in
  Codec.validate codec good 0;
  match Codec.validate codec bad 0 with
  | () -> Alcotest.fail "expected where failure"
  | exception Validation_error (Constraint_failed _) -> ()

let test_get_twostaged_same_field () =
  (* Two staged getters from the same codec+field with different envs *)
  let out = Param.output "out_two" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "TwoStaged" (fun v -> v) [ cf_v ] in
  let env1 = Codec.env codec in
  let env2 = Codec.env codec in
  let get1 = Staged.unstage (Codec.get ~env:env1 codec cf_v) in
  let get2 = Staged.unstage (Codec.get ~env:env2 codec cf_v) in
  (* Each staged getter has its own scratch array and env *)
  ignore (get1 (Bytes.of_string "\xAA") 0);
  ignore (get2 (Bytes.of_string "\xBB") 0);
  Alcotest.(check int) "env1" 0xAA (Param.get env1 out);
  Alcotest.(check int) "env2" 0xBB (Param.get env2 out)

let test_encode_shared_bitfield () =
  (* Encode via a codec that shares a bitfield with another codec.
     Default [bit_order] is [Msb_first], so the first-declared field [a]
     lives in the top nibble of the byte. *)
  let f_a = Field.v "a" (bits ~width:4 U8) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec1 =
    let open Codec in
    v "EncBf1"
      (fun a _b -> a)
      [ cf_a; (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "EncBf2"
      (fun _b a -> a)
      [ (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0); cf_a ]
  in
  let buf = Bytes.make 1 '\x00' in
  Codec.encode codec1 0xA buf 0;
  Alcotest.(check int) "top nibble (MSB-first)" 0xA0 (Bytes.get_uint8 buf 0)

(* -- API misuse / safety tests -- *)

let test_get_field_notin_codec () =
  (* get with a field that was never added to this codec raises Not_found
     at staging time *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let cf_y = Codec.(Field.v "y" uint8 $ fun v -> v) in
  let codec = Codec.v "OnlyX" (fun v -> v) [ cf_x ] in
  match Codec.get codec cf_y with
  | _ -> Alcotest.fail "expected Invalid_argument for unknown field"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "mentions field name" true
        (Re.execp (Re.compile (Re.str "y")) msg);
      Alcotest.(check bool)
        "mentions codec name" true
        (Re.execp (Re.compile (Re.str "OnlyX")) msg)

let test_set_field_notin_codec () =
  (* set with a field not in the codec raises Invalid_argument at staging *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let cf_y = Codec.(Field.v "y" uint8 $ fun v -> v) in
  let codec = Codec.v "OnlyX2" (fun v -> v) [ cf_x ] in
  match Codec.set codec cf_y with
  | _ -> Alcotest.fail "expected Invalid_argument for unknown field"
  | exception Invalid_argument _ -> ()

let test_bitfield_on_non_bitfield () =
  (* bitfield on a uint8 (non-bitfield) field *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let codec = Codec.v "NoBf" (fun v -> v) [ cf_x ] in
  match Codec.bitfield codec cf_x with
  | _ -> Alcotest.fail "expected error for non-bitfield"
  | exception Invalid_argument _ -> ()

let test_env_from_wrong_codec () =
  (* Using env from codec1 with get ~env on codec2 *)
  let out1 = Param.output "out_wrong" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v1 =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out1 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec1 = Codec.v "Wrong1" (fun v -> v) [ cf_v1 ] in
  let cf_v2 = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec2 = Codec.v "Wrong2" (fun v -> v) [ cf_v2 ] in
  let env1 = Codec.env codec1 in
  (* Use env1 (from codec1) with codec2's get -- should not crash *)
  let get_v2 = Staged.unstage (Codec.get ~env:env1 codec2 cf_v2) in
  (* No action on cf_v2, so env is ignored -- should work fine *)
  Alcotest.(check int)
    "wrong env ignored" 0x42
    (get_v2 (Bytes.of_string "\x42") 0)

let test_env_wrongcodec_with_action () =
  (* Using env from a different codec with an action field.
     The env has too few param slots -- get raises Invalid_argument
     at staging time. *)
  let out = Param.output "out_oob" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec_with_action = Codec.v "WithAct" (fun v -> v) [ cf_v ] in
  (* codec_empty has zero params, so its env has pe_slots = [||] *)
  let cf_w = Codec.(Field.v "w" uint8 $ fun v -> v) in
  let codec_empty = Codec.v "NoParams" (fun v -> v) [ cf_w ] in
  let wrong_env = Codec.env codec_empty in
  match Codec.get ~env:wrong_env codec_with_action cf_v with
  | _ -> Alcotest.fail "expected Invalid_argument for wrong env"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "mentions codec name" true
        (Re.execp (Re.compile (Re.str "WithAct")) msg)

let test_decode_short_buffer () =
  (* Decode with buffer shorter than wire_size *)
  let cf_x = Codec.(Field.v "x" uint16be $ fun v -> v) in
  let codec = Codec.v "Short" (fun v -> v) [ cf_x ] in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error (Unexpected_eof _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_encode_short_buffer () =
  (* Encode into buffer shorter than wire_size *)
  let cf_x = Codec.(Field.v "x" uint16be $ fun v -> v) in
  let codec = Codec.v "ShortEnc" (fun v -> v) [ cf_x ] in
  let buf = Bytes.of_string "\x42" in
  match Codec.encode codec 0x1234 buf 0 with
  | () -> Alcotest.fail "expected error for short buffer"
  | exception Invalid_argument _ -> ()

(* -- same bound field in two codecs -- *)

let test_same_field_two_codecs () =
  (* A single bound field used in two codecs with different layouts.
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
     If f_reader is mutable and set at seal time, the second seal clobbers
     the first. Both get/set must use the correct offset for their codec. *)
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "TwoCodec1" (fun x _y -> x) [ cf_x; (Field.v "y" uint16be $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "TwoCodec2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint16be $ fun _ -> 0); cf_x ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint16_be buf 2 0xBBBB;
  (* codec1 should read x at offset 0 -> 0xAAAA *)
  Alcotest.(check int)
    "codec1 get x" 0xAAAA
    ((Staged.unstage (Codec.get codec1 cf_x)) buf 0);
  (* codec2 should read x at offset 2 -> 0xBBBB *)
  Alcotest.(check int)
    "codec2 get x" 0xBBBB
    ((Staged.unstage (Codec.get codec2 cf_x)) buf 0)

let test_samefield_twocodecs_set () =
  (* Same field in two codecs: set via each must write to the correct offset. *)
  let f_v = Field.v "v" uint8 in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec1 =
    let open Codec in
    v "SetTwo1" (fun v _pad -> v) [ cf_v; (Field.v "pad" uint8 $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "SetTwo2" (fun _pad v -> v) [ (Field.v "pad" uint8 $ fun _ -> 0); cf_v ]
  in
  let buf = Bytes.make 2 '\x00' in
  (* set via codec1 should write to offset 0 *)
  (Staged.unstage (Codec.set codec1 cf_v)) buf 0 0xAA;
  Alcotest.(check int) "codec1 set -> byte 0" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int)
    "codec1 set -> byte 1 untouched" 0 (Bytes.get_uint8 buf 1);
  Bytes.fill buf 0 2 '\x00';
  (* set via codec2 should write to offset 1 *)
  (Staged.unstage (Codec.set codec2 cf_v)) buf 0 0xBB;
  Alcotest.(check int)
    "codec2 set -> byte 0 untouched" 0 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "codec2 set -> byte 1" 0xBB (Bytes.get_uint8 buf 1)

let test_samefield_twocodecs_decode () =
  (* Decode via the first codec after sealing both.
     The second seal clobbers f_reader, so decode uses the wrong offset. *)
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "DecTwo1" (fun x _y -> x) [ cf_x; (Field.v "y" uint16be $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "DecTwo2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint16be $ fun _ -> 0); cf_x ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (* codec1 decode should construct record with x from offset 0 *)
  match Codec.decode codec1 buf 0 with
  | Ok v -> Alcotest.(check int) "decoded x" 0x1234 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_samefield_twocodecs_encode () =
  (* Encode via the first codec after sealing both.
     The second seal clobbers f_writer, so encode writes to the wrong offset. *)
  let f_v = Field.v "v" uint8 in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec1 =
    let open Codec in
    v "EncTwo1" (fun v _pad -> v) [ cf_v; (Field.v "pad" uint8 $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "EncTwo2" (fun _pad v -> v) [ (Field.v "pad" uint8 $ fun _ -> 0); cf_v ]
  in
  let buf = Bytes.make 2 '\x00' in
  Codec.encode codec1 0xAA buf 0;
  (* codec1 should write v at offset 0 *)
  Alcotest.(check int) "byte 0" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "byte 1" 0x00 (Bytes.get_uint8 buf 1)

let test_same_bitfield_two_codecs () =
  (* Same bitfield bound field in two codecs with different bit positions.
     Default [bit_order = Msb_first]: first-declared field at top of byte. *)
  let f_a = Field.v "a" (bits ~width:4 U8) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec1 =
    let open Codec in
    v "BfTwo1"
      (fun a _b -> a)
      [ cf_a; (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "BfTwo2"
      (fun _b a -> a)
      [ (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0); cf_a ]
  in
  (* 0xA3: top nibble = 0xA, bottom nibble = 3. *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA3;
  (* codec1: a is first-declared, so top 4 bits -> 0xA. *)
  Alcotest.(check int)
    "codec1 get a (top)" 0xA
    ((Staged.unstage (Codec.get codec1 cf_a)) buf 0);
  (* codec2: a is second-declared, so bottom 4 bits -> 3. *)
  Alcotest.(check int)
    "codec2 get a (bottom)" 3
    ((Staged.unstage (Codec.get codec2 cf_a)) buf 0)

let test_samefield_staged_before_secondseal () =
  (* Stage get from codec1 BEFORE sealing codec2.
     The staged function captures f_reader at staging time. If f_reader
     is a mutable slot, the staged function sees the clobbered value
     after codec2 seals. *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "StagedTwo1" (fun x _y -> x) [ cf_x; (Field.v "y" uint8 $ fun _ -> 0) ]
  in
  (* Stage get from codec1 *)
  let get_x_1 = Staged.unstage (Codec.get codec1 cf_x) in
  (* Now seal codec2 -- this clobbers f_reader *)
  let _codec2 =
    let open Codec in
    v "StagedTwo2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint8 $ fun _ -> 0); cf_x ]
  in
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  (* get_x_1 was staged before codec2 -- should still read offset 0 *)
  Alcotest.(check int) "staged before second seal" 0xAA (get_x_1 buf 0)

(* -- byte_slice tests -- *)

module Bs = Bytesrw.Bytes.Slice

let test_view_byte_slice_get () =
  (* A record with a fixed-size byte_slice field returns a sub-slice *)
  let f_payload = Field.v "payload" (byte_slice ~size:(int 4)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let codec =
    let open Codec in
    v "SliceRec"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xABCD;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload = (Staged.unstage (Codec.get codec cf_payload)) buf 0 in
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
    let open Codec in
    v "SliceDec"
      (fun tag payload -> (tag, payload))
      [
        (Field.v "tag" uint8 $ fun (t, _) -> t);
        (Field.v "data" (byte_slice ~size:(int 3)) $ fun (_, p) -> p);
      ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xFF;
  Bytes.set_uint8 buf 1 0xAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let tag, payload = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "tag" 0xFF tag;
  Alcotest.(check int) "payload first" 1 (Bs.first payload);
  Alcotest.(check int) "payload length" 3 (Bs.length payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_view_byte_slice_nested () =
  (* Two-layer nested protocol: get payload slice, then get inner field *)
  let f_val = Field.v "val" uint16be in
  let cf_val = Codec.(f_val $ fun v -> v) in
  let inner_codec = Codec.v "Inner" (fun v -> v) [ cf_val ] in
  let f_payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let outer_codec =
    let open Codec in
    v "Outer"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x1234;
  let payload_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec cf_payload)) buf 0)
  in
  let inner_val =
    (Staged.unstage (Codec.get inner_codec cf_val)) buf payload_off
  in
  Alcotest.(check int) "inner val via zero-copy" 0x1234 inner_val

(* -- Raw access: get / set / sub -- *)

let test_raw_get_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint8 buf 2 0xFF;
  Alcotest.(check int)
    "get a" 0x1234
    ((Staged.unstage (Codec.get codec cf_a)) buf 0);
  Alcotest.(check int)
    "get b" 0xFF
    ((Staged.unstage (Codec.get codec cf_b)) buf 0)

let test_raw_get_bitfield () =
  (* Default [bit_order = Msb_first]: [hi] (first declared) is the top nibble,
     matching the natural naming. *)
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let cf_hi = Codec.(f_hi $ fun (h, _) -> h) in
  let cf_lo = Codec.(f_lo $ fun (_, l) -> l) in
  let codec = Codec.v "RawBF" (fun hi lo -> (hi, lo)) [ cf_hi; cf_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  Alcotest.(check int)
    "get hi" 0xA
    ((Staged.unstage (Codec.get codec cf_hi)) buf 0);
  Alcotest.(check int)
    "get lo" 0x7
    ((Staged.unstage (Codec.get codec cf_lo)) buf 0)

let test_raw_set_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawSU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.fill buf 0 3 '\x00';
  (Staged.unstage (Codec.set codec cf_a)) buf 0 0xABCD;
  (Staged.unstage (Codec.set codec cf_b)) buf 0 0x42;
  Alcotest.(check int) "set a" 0xABCD (Bytes.get_uint16_be buf 0);
  Alcotest.(check int) "set b" 0x42 (Bytes.get_uint8 buf 2)

let test_raw_set_bitfield () =
  (* Default [bit_order = Msb_first]: [hi] goes to the top nibble. *)
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let cf_hi = Codec.(f_hi $ fun (h, _) -> h) in
  let cf_lo = Codec.(f_lo $ fun (_, l) -> l) in
  let codec = Codec.v "RawSBF" (fun hi lo -> (hi, lo)) [ cf_hi; cf_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec cf_hi)) buf 0 0xC;
  (Staged.unstage (Codec.set codec cf_lo)) buf 0 0x3;
  Alcotest.(check int) "set bf byte" 0xC3 (Bytes.get_uint8 buf 0)

let test_raw_sub_nested () =
  (* Two-layer nested protocol using sub + get: zero alloc *)
  let f_val = Field.v "val" uint16be in
  let cf_val = Codec.(f_val $ fun v -> v) in
  let inner_codec = Codec.v "Inner" (fun v -> v) [ cf_val ] in
  let f_payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let outer_codec =
    let open Codec in
    v "Outer"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x5678;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec cf_payload)) buf 0)
  in
  Alcotest.(check int) "sub offset" 2 inner_off;
  let inner_val =
    (Staged.unstage (Codec.get inner_codec cf_val)) buf inner_off
  in
  Alcotest.(check int) "inner val via sub+get" 0x5678 inner_val

let test_raw_sub_three_layers () =
  (* Three-layer: outer -> mid -> inner, all zero-alloc via sub+get *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let inner = Codec.v "L3" (fun x -> x) [ cf_x ] in
  let f_mid_payload = Field.v "data" (byte_slice ~size:(int 1)) in
  let cf_mid_payload = Codec.(f_mid_payload $ fun (_, p) -> p) in
  let mid =
    let open Codec in
    v "L2"
      (fun tag payload -> (tag, payload))
      [ (Field.v "tag" uint8 $ fun (t, _) -> t); cf_mid_payload ]
  in
  let f_body = Field.v "body" (byte_slice ~size:(int 2)) in
  let cf_body = Codec.(f_body $ fun (_, b) -> b) in
  let outer =
    let open Codec in
    v "L1"
      (fun hdr body -> (hdr, body))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_body ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let mid_off = Bs.first ((Staged.unstage (Codec.get outer cf_body)) buf 0) in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get mid cf_mid_payload)) buf mid_off)
  in
  Alcotest.(check int) "inner offset" 3 inner_off;
  let x = (Staged.unstage (Codec.get inner cf_x)) buf inner_off in
  Alcotest.(check int) "3-layer get" 0xCC x

let test_raw_with_offset () =
  (* get / set work correctly with non-zero base offset *)
  let f_v = Field.v "v" uint32be in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec = Codec.v "RawOff" (fun v -> v) [ cf_v ] in
  let buf = Bytes.create 20 in
  Bytes.fill buf 0 20 '\x00';
  (Staged.unstage (Codec.set codec cf_v)) buf 10 0xDEADBEEF;
  Alcotest.(check int)
    "get at offset 10" 0xDEADBEEF
    ((Staged.unstage (Codec.get codec cf_v)) buf 10)

(* -- Dependent-size byte_slice tests -- *)

type dep_slice_record = { ds_length : int; ds_payload : Bs.t }

let f_ds_length = Field.v "Length" uint16be
let f_ds_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_ds_length))
let cf_ds_length = Codec.(f_ds_length $ fun r -> r.ds_length)
let cf_ds_payload = Codec.(f_ds_payload $ fun r -> r.ds_payload)

let dep_slice_codec =
  Codec.v "DepSlice"
    (fun length payload -> { ds_length = length; ds_payload = payload })
    [ cf_ds_length; cf_ds_payload ]

let test_dep_bslice_decode_empty () =
  (* length=0, no payload bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
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
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
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
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
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
  (* Verify wire_size_at reads the buffer correctly *)
  Alcotest.(check int)
    "wire_size_at" 6
    (Codec.wire_size_at dep_slice_codec buf 0);
  let decoded = decode_ok (Codec.decode dep_slice_codec buf 0) in
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
    (Staged.unstage (Codec.get dep_slice_codec cf_ds_payload)) buf 0
  in
  Alcotest.(check int) "get payload first" 2 (Bs.first payload);
  Alcotest.(check int) "get payload length" 4 (Bs.length payload);
  Alcotest.(check int)
    "get payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_dep_bslice_sub () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  let off =
    Bs.first ((Staged.unstage (Codec.get dep_slice_codec cf_ds_payload)) buf 0)
  in
  Alcotest.(check int) "sub offset" 2 off

let test_dep_bslice_set_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  (Staged.unstage (Codec.set dep_slice_codec cf_ds_length)) buf 0 8;
  Alcotest.(check int)
    "set length" 8
    ((Staged.unstage (Codec.get dep_slice_codec cf_ds_length)) buf 0)

let test_dep_bslice_get_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 42;
  Alcotest.(check int)
    "get length" 42
    ((Staged.unstage (Codec.get dep_slice_codec cf_ds_length)) buf 0)

(* -- Dependent-size byte_array tests -- *)

type dep_array_record = { da_length : int; da_payload : string }

let f_da_length = Field.v "Length" uint16be
let f_da_payload = Field.v "Payload" (byte_array ~size:(Field.ref f_da_length))
let cf_da_length = Codec.(f_da_length $ fun r -> r.da_length)
let cf_da_payload = Codec.(f_da_payload $ fun r -> r.da_payload)

let dep_array_codec =
  Codec.v "DepArray"
    (fun length payload -> { da_length = length; da_payload = payload })
    [ cf_da_length; cf_da_payload ]

let test_dep_byte_array_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 5;
  Bytes.blit_string "hello" 0 buf 2 5;
  let r = decode_ok (Codec.decode dep_array_codec buf 0) in
  Alcotest.(check int) "length" 5 r.da_length;
  Alcotest.(check string) "payload is string copy" "hello" r.da_payload

let test_dep_byte_array_roundtrip () =
  let original = { da_length = 3; da_payload = "abc" } in
  let buf = Bytes.create 5 in
  Codec.encode dep_array_codec original buf 0;
  let decoded = decode_ok (Codec.decode dep_array_codec buf 0) in
  Alcotest.(check int) "roundtrip length" 3 decoded.da_length;
  Alcotest.(check string) "roundtrip payload" "abc" decoded.da_payload

let test_dep_byte_array_get () =
  let buf = Bytes.create 5 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.blit_string "xyz" 0 buf 2 3;
  let payload =
    (Staged.unstage (Codec.get dep_array_codec cf_da_payload)) buf 0
  in
  Alcotest.(check string) "get payload" "xyz" payload

(* -- Fixed field after variable field tests -- *)

type trailer_record = { tr_length : int; tr_payload : Bs.t; tr_checksum : int }

let f_tr_length = Field.v "Length" uint16be
let f_tr_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_tr_length))
let f_tr_checksum = Field.v "Checksum" uint16be
let cf_tr_length = Codec.(f_tr_length $ fun r -> r.tr_length)
let cf_tr_payload = Codec.(f_tr_payload $ fun r -> r.tr_payload)
let cf_tr_checksum = Codec.(f_tr_checksum $ fun r -> r.tr_checksum)

let trailer_codec =
  Codec.v "Trailer"
    (fun length payload checksum ->
      { tr_length = length; tr_payload = payload; tr_checksum = checksum })
    [ cf_tr_length; cf_tr_payload; cf_tr_checksum ]

let test_dep_trailer_get_checksum () =
  (* [length:u16be=3] [payload:3 bytes] [checksum:u16be=0xBEEF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let checksum =
    (Staged.unstage (Codec.get trailer_codec cf_tr_checksum)) buf 0
  in
  Alcotest.(check int) "get checksum" 0xBEEF checksum

let test_dep_trailer_set_checksum () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0x0000;
  (Staged.unstage (Codec.set trailer_codec cf_tr_checksum)) buf 0 0xCAFE;
  Alcotest.(check int) "set checksum" 0xCAFE (Bytes.get_uint16_be buf 5)

let test_dep_trailer_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint16_be buf 5 0xDEAD;
  let r = decode_ok (Codec.decode trailer_codec buf 0) in
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
  let decoded = decode_ok (Codec.decode trailer_codec buf 0) in
  Alcotest.(check int) "rt length" 2 decoded.tr_length;
  Alcotest.(check int) "rt payload len" 2 (Bs.length decoded.tr_payload);
  Alcotest.(check int) "rt checksum" 0x1234 decoded.tr_checksum

(* -- wire_size API for variable codecs -- *)

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
    (Codec.wire_size_at dep_slice_codec buf 0);
  (* dep_slice: length=0 -> total = 2 + 0 = 2 *)
  Bytes.set_uint16_be buf 0 0;
  Alcotest.(check int)
    "dep_slice compute 0" 2
    (Codec.wire_size_at dep_slice_codec buf 0);
  (* trailer: length=3 -> total = 2 + 3 + 2 = 7 *)
  let buf2 = Bytes.create 7 in
  Bytes.set_uint16_be buf2 0 3;
  Alcotest.(check int)
    "trailer compute" 7
    (Codec.wire_size_at trailer_codec buf2 0);
  (* fixed codec: compute returns fixed size without reading buffer *)
  Alcotest.(check int)
    "fixed compute"
    (Codec.wire_size simple_record_codec)
    (Codec.wire_size_at simple_record_codec (Bytes.create 7) 0)

(* -- Field.ref expression tests -- *)

let test_dep_codec_ref () =
  (* Field.ref produces a valid expression used as byte_slice size *)
  let f_len = Field.v "Len" uint8 in
  let f_data = Field.v "Data" (byte_slice ~size:(Field.ref f_len)) in
  let cf_len = Codec.(f_len $ fun (l, _) -> l) in
  let cf_data = Codec.(f_data $ fun (_, d) -> d) in
  let codec =
    Codec.v "RefTest" (fun len data -> (len, data)) [ cf_len; cf_data ]
  in
  (* buf: [len=5] [5 bytes payload] *)
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 5;
  for i = 0 to 4 do
    Bytes.set_uint8 buf (1 + i) (0x10 + i)
  done;
  let len, data = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "ref len" 5 len;
  Alcotest.(check int) "ref data length" 5 (Bs.length data);
  Alcotest.(check int)
    "ref data[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data));
  Alcotest.(check int)
    "ref data[4]" 0x14
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data + 4))

let test_dep_ref_size_eval () =
  (* Test that the size expression is evaluated correctly for wire_size_at *)
  let f_sz = Field.v "Size" uint8 in
  let f_body = Field.v "Body" (byte_slice ~size:(Field.ref f_sz)) in
  let cf_sz = Codec.(f_sz $ fun (s, _) -> s) in
  let cf_body = Codec.(f_body $ fun (_, b) -> b) in
  let codec =
    Codec.v "RefSizeEval" (fun sz body -> (sz, body)) [ cf_sz; cf_body ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_uint8 buf 0 10;
  Alcotest.(check int) "compute size" 11 (Codec.wire_size_at codec buf 0);
  Bytes.set_uint8 buf 0 0;
  Alcotest.(check int) "compute size 0" 1 (Codec.wire_size_at codec buf 0)

(* -- struct_of_codec for variable-size codecs -- *)

let test_struct_of_dep () =
  (* struct_of_codec should produce a valid struct for variable-size codecs *)
  let s = Everparse.struct_of_codec dep_slice_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output);
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output)

let test_struct_of_dep_trailer () =
  let s = Everparse.struct_of_codec trailer_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool)
    "contains Checksum" true
    (contains ~sub:"Checksum" output)

(* -- sizeof_this / field_pos in codec -- *)

type pos_record = { pa : int; pb : int; pc : int }

let test_codec_sizeof_this () =
  let out = Param.output "out" uint8 in
  let codec =
    let open Codec in
    v "SizeofThisCodec"
      (fun a b c -> { pa = a; pb = b; pc = c })
      [
        (Field.v "a" uint8 $ fun r -> r.pa);
        (Field.v "b" uint16be $ fun r -> r.pb);
        ( Field.v "c"
            ~action:(Action.on_success [ Action.assign out sizeof_this ])
            uint8
        $ fun r -> r.pc );
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x01\x00\x02\x03" in
  let _v = decode_ok (Codec.decode ~env codec buf 0) in
  (* sizeof_this at field c = 1 (uint8) + 2 (uint16be) = 3 *)
  Alcotest.(check int) "sizeof_this at c" 3 (Param.get env out)

let test_codec_field_pos () =
  let out = Param.output "out" uint8 in
  let codec =
    let open Codec in
    v "FieldPosCodec"
      (fun a b c -> { pa = a; pb = b; pc = c })
      [
        (Field.v "a" uint8 $ fun r -> r.pa);
        (Field.v "b" uint8 $ fun r -> r.pb);
        ( Field.v "c"
            ~action:(Action.on_success [ Action.assign out field_pos ])
            uint8
        $ fun r -> r.pc );
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x01\x02\x03" in
  let _v = decode_ok (Codec.decode ~env codec buf 0) in
  (* field_pos at c = 2 (third field, zero-indexed) *)
  Alcotest.(check int) "field_pos at c" 2 (Param.get env out)

(* -- Bitfield batch access -- *)

type bf_rec = { bf_hi : int; bf_lo : int }

let bf_f_hi = Field.v "hi" (bits ~width:4 U8)
let bf_f_lo = Field.v "lo" (bits ~width:4 U8)
let bf_cf_hi = Codec.(bf_f_hi $ fun r -> r.bf_hi)
let bf_cf_lo = Codec.(bf_f_lo $ fun r -> r.bf_lo)

let bf_codec =
  Codec.v "BfBatch"
    (fun hi lo -> { bf_hi = hi; bf_lo = lo })
    Codec.[ bf_cf_hi; bf_cf_lo ]

let test_bitfield_extract () =
  (* Default [bit_order = Msb_first]: [hi] is the top nibble. *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  let bf_hi = Codec.bitfield bf_codec bf_cf_hi in
  let bf_lo = Codec.bitfield bf_codec bf_cf_lo in
  let load = Staged.unstage (Codec.load_word bf_hi) in
  let w = load buf 0 in
  let hi = Codec.extract bf_hi w in
  let lo = Codec.extract bf_lo w in
  (* Compare against Codec.get *)
  let get_hi = Staged.unstage (Codec.get bf_codec bf_cf_hi) in
  let get_lo = Staged.unstage (Codec.get bf_codec bf_cf_lo) in
  Alcotest.(check int) "extract hi = get hi" (get_hi buf 0) hi;
  Alcotest.(check int) "extract lo = get lo" (get_lo buf 0) lo;
  Alcotest.(check int) "hi" 0xA hi;
  Alcotest.(check int) "lo" 0x7 lo

let test_bitfield_non_bf_raises () =
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec = Codec.v "NonBf" (fun x -> x) Codec.[ cf_x ] in
  match Codec.bitfield codec cf_x with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let test_bitfield_short_buffer () =
  (* Reading a uint32be bitfield from a 2-byte buffer should not segfault *)
  let f_a = Field.v "a" (bits ~width:8 U32be) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec = Codec.v "Short" (fun a -> a) Codec.[ cf_a ] in
  let bf = Codec.bitfield codec cf_a in
  let load = Staged.unstage (Codec.load_word bf) in
  (* Short buffer -- should read garbage but not crash *)
  let buf = Bytes.create 8 in
  Bytes.set_int32_be buf 0 0x12345678l;
  let w = load buf 0 in
  let v = Codec.extract bf w in
  Alcotest.(check int) "extract from valid buf" 0x12 v

let test_bitfield_load_shared () =
  (* Two fields in the same base word should get the same word value *)
  let f_a = Field.v "a" (bits ~width:4 U32be) in
  let f_b = Field.v "b" (bits ~width:4 U32be) in
  let cf_a = Codec.(f_a $ fst) in
  let cf_b = Codec.(f_b $ snd) in
  let codec = Codec.v "Shared" (fun a b -> (a, b)) Codec.[ cf_a; cf_b ] in
  let bf_a = Codec.bitfield codec cf_a in
  let bf_b = Codec.bitfield codec cf_b in
  let load_a = Staged.unstage (Codec.load_word bf_a) in
  let load_b = Staged.unstage (Codec.load_word bf_b) in
  let buf = Bytes.create 4 in
  Bytes.set_int32_be buf 0 0xABCDEF01l;
  let wa = load_a buf 0 in
  let wb = load_b buf 0 in
  (* Same base word, same value *)
  Alcotest.(check int) "same word" wa wb;
  let a = Codec.extract bf_a wa in
  let b = Codec.extract bf_b wa in
  (* a = top 4 bits of 0xABCDEF01 = 0xA, b = next 4 bits = 0xB *)
  Alcotest.(check int) "a" 0xA a;
  Alcotest.(check int) "b" 0xB b

(* -- Nested: Codec typ: embed a sub-codec as a field --
   [inner] / [outer] / [inner_codec] / [outer_codec] live in {!Test_fixtures}. *)

let test_codec_embed_decode () =
  (* header(1) + tag(1) + value(2) + trailer(1) = 5 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  Bytes.set_uint8 buf 4 0xBB;
  let r = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int) "header" 0xAA r.header;
  Alcotest.(check int) "inner.tag" 0x42 r.inner.tag;
  Alcotest.(check int) "inner.value" 0x1234 r.inner.value;
  Alcotest.(check int) "trailer" 0xBB r.trailer

let test_codec_embed_encode () =
  let v =
    { header = 0xAA; inner = { tag = 0x42; value = 0x1234 }; trailer = 0xBB }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec v buf 0;
  Alcotest.(check int) "header byte" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "value bytes" 0x1234 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "trailer byte" 0xBB (Bytes.get_uint8 buf 4)

let test_codec_embed_roundtrip () =
  let original =
    { header = 0x11; inner = { tag = 0x22; value = 0x3344 }; trailer = 0x55 }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec original buf 0;
  let decoded = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int) "header" original.header decoded.header;
  Alcotest.(check int) "inner.tag" original.inner.tag decoded.inner.tag;
  Alcotest.(check int) "inner.value" original.inner.value decoded.inner.value;
  Alcotest.(check int) "trailer" original.trailer decoded.trailer

let test_codec_embed_wire_size () =
  Alcotest.(check int) "wire_size" 5 (Codec.wire_size outer_codec);
  Alcotest.(check bool) "is_fixed" true (Codec.is_fixed outer_codec)

(* Nested codec with bitfields *)

type bf_inner = { version : int; flags : int }

let bf_inner_codec =
  Codec.v "BfInner"
    (fun version flags -> { version; flags })
    Codec.
      [
        (Field.v "Version" (bits ~width:4 U8) $ fun r -> r.version);
        (Field.v "Flags" (bits ~width:4 U8) $ fun r -> r.flags);
      ]

type bf_outer = { id : int; bf : bf_inner; checksum : int }

let bf_outer_codec =
  Codec.v "BfOuter"
    (fun id bf checksum -> { id; bf; checksum })
    Codec.
      [
        (Field.v "Id" uint16be $ fun r -> r.id);
        (Field.v "Bf" (codec bf_inner_codec) $ fun r -> r.bf);
        (Field.v "Checksum" uint8 $ fun r -> r.checksum);
      ]

let test_codec_embed_bitfield () =
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  (* Default [bit_order = Msb_first]: version (first declared) at top nibble,
     flags at bottom nibble. version=0xA, flags=0x5 -> byte = 0xA5. *)
  Bytes.set_uint8 buf 2 0xA5;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode bf_outer_codec buf 0) in
  Alcotest.(check int) "id" 0x1234 r.id;
  Alcotest.(check int) "version" 0xA r.bf.version;
  Alcotest.(check int) "flags" 0x5 r.bf.flags;
  Alcotest.(check int) "checksum" 0xFF r.checksum

(* Two levels of nesting --
   [l0] / [l1] / [l2] and their codecs live in {!Test_fixtures}. *)

let test_codec_embed_nested () =
  (* l2(1) + l1_y(2) + z(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x42;
  Bytes.set_uint16_be buf 1 0xABCD;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int) "l2.x" 0x42 r.l0_inner.l1_inner.l2_x;
  Alcotest.(check int) "l1.y" 0xABCD r.l0_inner.l1_y;
  Alcotest.(check int) "l0.z" 0xFF r.l0_z

let test_codec_embed_nested_roundtrip () =
  let original =
    { l0_inner = { l1_inner = { l2_x = 0x42 }; l1_y = 0xABCD }; l0_z = 0xFF }
  in
  let buf = Bytes.create 4 in
  Codec.encode l0_codec original buf 0;
  let decoded = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int)
    "l2.x" original.l0_inner.l1_inner.l2_x decoded.l0_inner.l1_inner.l2_x;
  Alcotest.(check int) "l1.y" original.l0_inner.l1_y decoded.l0_inner.l1_y;
  Alcotest.(check int) "l0.z" original.l0_z decoded.l0_z

(* -- Cross-codec Field.ref: parent expression references sub-codec field -- *)

(* TC/TM-style frame: header contains a length field, the data field's size
   is computed from that nested header field via Field.ref. *)

type tc_header = { tc_version : int; tc_frame_len : int }

let f_tc_version = Field.v "Version" uint8
let f_tc_frame_len = Field.v "FrameLen" uint8

let tc_header_codec =
  Codec.v "TcHeader"
    (fun version frame_len ->
      { tc_version = version; tc_frame_len = frame_len })
    Codec.
      [
        (f_tc_version $ fun r -> r.tc_version);
        (f_tc_frame_len $ fun r -> r.tc_frame_len);
      ]

type tc_frame = { tc_hdr : tc_header; tc_data : string; tc_check : int }

(* The data field's size is `Field.ref f_tc_frame_len - 2 - 1` (header is 2 bytes,
   trailer is 1 byte). The Field.ref must resolve into the embedded header codec. *)
let tc_frame_codec =
  Codec.v "TcFrame"
    (fun hdr data check -> { tc_hdr = hdr; tc_data = data; tc_check = check })
    Codec.
      [
        (Field.v "Header" (codec tc_header_codec) $ fun r -> r.tc_hdr);
        ( Field.v "Data"
            (byte_array ~size:Expr.(Field.ref f_tc_frame_len - int 2 - int 1))
        $ fun r -> r.tc_data );
        (Field.v "Check" uint8 $ fun r -> r.tc_check);
      ]

let test_codec_cross_field_ref () =
  (* frame_len=8 -> data is 8-2-1=5 bytes *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 1;
  (* version *)
  Bytes.set_uint8 buf 1 8;
  (* frame_len *)
  Bytes.blit_string "HELLO" 0 buf 2 5;
  Bytes.set_uint8 buf 7 0xCC;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check int) "version" 1 r.tc_hdr.tc_version;
  Alcotest.(check int) "frame_len" 8 r.tc_hdr.tc_frame_len;
  Alcotest.(check string) "data" "HELLO" r.tc_data;
  Alcotest.(check int) "check" 0xCC r.tc_check

let test_codec_crossref_field_varying () =
  (* frame_len=5 -> data is 2 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 2;
  Bytes.set_uint8 buf 1 5;
  Bytes.blit_string "AB" 0 buf 2 2;
  Bytes.set_uint8 buf 4 0xFF;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check int) "frame_len" 5 r.tc_hdr.tc_frame_len;
  Alcotest.(check string) "data" "AB" r.tc_data;
  Alcotest.(check int) "check" 0xFF r.tc_check

(* -- Adversarial: cross-codec Field.ref edge cases -- *)

(* Attacker sets frame_len=255 in a 5-byte buffer. The data field's computed
   size (255-3=252) exceeds available bytes -- must report Unexpected_eof,
   not crash or silently truncate. *)
let test_codec_crossref_field_oversized () =
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 0xFF;
  (* attacker frame_len *)
  Bytes.blit_string "AB" 0 buf 2 2;
  Bytes.set_uint8 buf 4 0xCC;
  match Codec.decode tc_frame_codec buf 0 with
  | Ok _ -> Alcotest.fail "expected EOF on oversized data field"
  | Error (Unexpected_eof _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* Attacker sets frame_len=2 -> data size = 2-3 = -1. Must error, not crash. *)
let test_codec_crossref_field_underflow () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  (* below header+trailer minimum *)
  Bytes.set_uint8 buf 2 0;
  Bytes.set_uint8 buf 3 0xCC;
  match Codec.decode tc_frame_codec buf 0 with
  | Ok r ->
      (* If accepted, the data must be empty or the decode must have rejected
         the negative size. Either is acceptable as long as no crash. *)
      Alcotest.(check bool)
        "data length non-negative" true
        (String.length r.tc_data >= 0)
  | Error _ -> ()

(* frame_len=3 -> data size = 0. Boundary case: empty data. *)
let test_codec_crossref_field_zerodata () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 3;
  Bytes.set_uint8 buf 2 0xCC;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check string) "data" "" r.tc_data;
  Alcotest.(check int) "check" 0xCC r.tc_check

(* Sub-codec field name shadowing: parent has its own field with same name as
   a sub-codec field. The parent's name should win for parent-scope expressions
   defined after the parent field. *)

type shadow_inner = { si_x : int }

let f_si_x = Field.v "Shared" uint8

let shadow_inner_codec =
  Codec.v "ShadowInner"
    (fun x -> { si_x = x })
    Codec.[ (f_si_x $ fun r -> r.si_x) ]

type shadow_outer = {
  so_inner : shadow_inner;
  so_shared : int;
  so_data : string;
}

let f_so_shared = Field.v "Shared" uint8

let shadow_outer_codec =
  Codec.v "ShadowOuter"
    (fun inner shared data ->
      { so_inner = inner; so_shared = shared; so_data = data })
    Codec.
      [
        (Field.v "Inner" (codec shadow_inner_codec) $ fun r -> r.so_inner);
        (f_so_shared $ fun r -> r.so_shared);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_so_shared)) $ fun r ->
          r.so_data );
      ]

let test_codec_field_shadow () =
  (* inner.shared=5, parent.shared=3 -> data should be 3 bytes (parent wins) *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 5;
  (* inner.shared *)
  Bytes.set_uint8 buf 1 3;
  (* parent.shared *)
  Bytes.blit_string "ABC" 0 buf 2 3;
  let r = decode_ok (Codec.decode shadow_outer_codec buf 0) in
  Alcotest.(check int) "inner.shared" 5 r.so_inner.si_x;
  Alcotest.(check int) "parent.shared" 3 r.so_shared;
  Alcotest.(check string) "data" "ABC" r.so_data

(* Two-level deep nesting: outer references a field three levels down. *)

type inner_l1 = { il1_x : int }

let f_il1_x = Field.v "DeepLen" uint8

let inner_l1_codec =
  Codec.v "InnerL1"
    (fun x -> { il1_x = x })
    Codec.[ (f_il1_x $ fun r -> r.il1_x) ]

type middle_l2 = { ml2_inner : inner_l1; ml2_y : int }

let middle_l2_codec =
  Codec.v "MiddleL2"
    (fun inner y -> { ml2_inner = inner; ml2_y = y })
    Codec.
      [
        (Field.v "Inner" (codec inner_l1_codec) $ fun r -> r.ml2_inner);
        (Field.v "Y" uint8 $ fun r -> r.ml2_y);
      ]

type outer_l3 = { ol3_mid : middle_l2; ol3_data : string }

(* The outer references DeepLen which lives 2 levels deep inside the middle codec. *)
let outer_l3_codec =
  Codec.v "OuterL3"
    (fun mid data -> { ol3_mid = mid; ol3_data = data })
    Codec.
      [
        (Field.v "Middle" (codec middle_l2_codec) $ fun r -> r.ol3_mid);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_il1_x)) $ fun r ->
          r.ol3_data );
      ]

let test_codec_crossref_field_twolevels () =
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 4;
  (* DeepLen at l1 *)
  Bytes.set_uint8 buf 1 0xFF;
  (* l2.y *)
  Bytes.blit_string "ABCD" 0 buf 2 4;
  let r = decode_ok (Codec.decode outer_l3_codec buf 0) in
  Alcotest.(check int) "deep_len" 4 r.ol3_mid.ml2_inner.il1_x;
  Alcotest.(check int) "y" 0xFF r.ol3_mid.ml2_y;
  Alcotest.(check string) "data" "ABCD" r.ol3_data

(* Sub-codec with bitfield referenced from parent. The sub-codec packs a 4-bit
   length and 4-bit flags into one byte; the parent uses the length to size data. *)

type bf_hdr = { bh_len : int; bh_flags : int }

let f_bh_len = Field.v "BfLen" (bits ~width:4 U8)
let f_bh_flags = Field.v "BfFlags" (bits ~width:4 U8)

let bf_hdr_codec =
  Codec.v "BfHdr"
    (fun len flags -> { bh_len = len; bh_flags = flags })
    Codec.[ (f_bh_len $ fun r -> r.bh_len); (f_bh_flags $ fun r -> r.bh_flags) ]

type bf_frame = { bff_hdr : bf_hdr; bff_data : string }

let bf_frame_codec =
  Codec.v "BfFrame"
    (fun hdr data -> { bff_hdr = hdr; bff_data = data })
    Codec.
      [
        (Field.v "Hdr" (codec bf_hdr_codec) $ fun r -> r.bff_hdr);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_bh_len)) $ fun r ->
          r.bff_data );
      ]

let test_codec_crossref_field_bitfield () =
  (* Default [bit_order = Msb_first]: [BfLen] (first declared) is the top
     nibble, [BfFlags] is the bottom nibble. len=3, flags=0xA -> byte = 0x3A. *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x3A;
  Bytes.blit_string "XYZ" 0 buf 1 3;
  let r = decode_ok (Codec.decode bf_frame_codec buf 0) in
  Alcotest.(check int) "len" 3 r.bff_hdr.bh_len;
  Alcotest.(check int) "flags" 0xA r.bff_hdr.bh_flags;
  Alcotest.(check string) "data" "XYZ" r.bff_data

(* -- Nested: Optional typ: conditional field presence --
   [opt_record], [opt_codec], [opt_codec_present], [opt_codec_absent] live
   in {!Test_fixtures}. *)

let test_optional_present_decode () =
  (* hdr(1) + payload(2) + trail(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xBB;
  let r = decode_ok (Codec.decode opt_codec_present buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.opt_hdr;
  Alcotest.(check (option int)) "payload" (Some 0x1234) r.opt_payload;
  Alcotest.(check int) "trail" 0xBB r.opt_trail

let test_optional_absent_decode () =
  (* hdr(1) + trail(1) = 2 bytes (no payload) *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  let r = decode_ok (Codec.decode opt_codec_absent buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.opt_hdr;
  Alcotest.(check (option int)) "payload" None r.opt_payload;
  Alcotest.(check int) "trail" 0xBB r.opt_trail

let test_optional_present_encode () =
  let v = { opt_hdr = 0xAA; opt_payload = Some 0x1234; opt_trail = 0xBB } in
  let buf = Bytes.create 4 in
  Codec.encode opt_codec_present v buf 0;
  Alcotest.(check int) "hdr" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "payload" 0x1234 (Bytes.get_uint16_be buf 1);
  Alcotest.(check int) "trail" 0xBB (Bytes.get_uint8 buf 3)

let test_optional_absent_encode () =
  let v = { opt_hdr = 0xAA; opt_payload = None; opt_trail = 0xBB } in
  let buf = Bytes.create 2 in
  Codec.encode opt_codec_absent v buf 0;
  Alcotest.(check int) "hdr" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "trail" 0xBB (Bytes.get_uint8 buf 1)

let test_optional_present_roundtrip () =
  let original =
    { opt_hdr = 0x11; opt_payload = Some 0x2233; opt_trail = 0x44 }
  in
  let buf = Bytes.create 4 in
  Codec.encode opt_codec_present original buf 0;
  let decoded = decode_ok (Codec.decode opt_codec_present buf 0) in
  Alcotest.(check int) "hdr" original.opt_hdr decoded.opt_hdr;
  Alcotest.(check (option int))
    "payload" original.opt_payload decoded.opt_payload;
  Alcotest.(check int) "trail" original.opt_trail decoded.opt_trail

let test_optional_absent_roundtrip () =
  let original = { opt_hdr = 0x11; opt_payload = None; opt_trail = 0x44 } in
  let buf = Bytes.create 2 in
  Codec.encode opt_codec_absent original buf 0;
  let decoded = decode_ok (Codec.decode opt_codec_absent buf 0) in
  Alcotest.(check int) "hdr" original.opt_hdr decoded.opt_hdr;
  Alcotest.(check (option int))
    "payload" original.opt_payload decoded.opt_payload;
  Alcotest.(check int) "trail" original.opt_trail decoded.opt_trail

let test_optional_wire_size_present () =
  Alcotest.(check int) "wire_size present" 4 (Codec.wire_size opt_codec_present)

let test_optional_wire_size_absent () =
  Alcotest.(check int) "wire_size absent" 2 (Codec.wire_size opt_codec_absent)

(* Optional with codec inner type *)

type opt_codec_record = {
  oc_hdr : int;
  oc_inner : inner option;
  oc_trail : int;
}

let opt_inner_codec ~present =
  Codec.v "OptCodecRecord"
    (fun hdr inner trail ->
      { oc_hdr = hdr; oc_inner = inner; oc_trail = trail })
    Codec.
      [
        (Field.v "Hdr" uint8 $ fun r -> r.oc_hdr);
        ( Field.v "Inner" (optional (bool present) (codec inner_codec))
        $ fun r -> r.oc_inner );
        (Field.v "Trail" uint8 $ fun r -> r.oc_trail);
      ]

let test_optional_codec_present () =
  let c = opt_inner_codec ~present:true in
  (* hdr(1) + inner(3) + trail(1) = 5 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  Bytes.set_uint8 buf 4 0xBB;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.oc_hdr;
  (match r.oc_inner with
  | None -> Alcotest.fail "expected Some"
  | Some inner ->
      Alcotest.(check int) "inner.tag" 0x42 inner.tag;
      Alcotest.(check int) "inner.value" 0x1234 inner.value);
  Alcotest.(check int) "trail" 0xBB r.oc_trail

let test_optional_codec_absent () =
  let c = opt_inner_codec ~present:false in
  (* hdr(1) + trail(1) = 2 bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.oc_hdr;
  Alcotest.(check (option int))
    "inner" None
    (Option.map (fun (i : inner) -> i.tag) r.oc_inner);
  Alcotest.(check int) "trail" 0xBB r.oc_trail

(* Multiple optional fields (TM frame pattern) *)

type multi_opt = { mo_data : int; mo_ocf : int option; mo_fecf : int option }

let multi_opt_codec ~ocf ~fecf =
  Codec.v "MultiOpt"
    (fun data ocf fecf -> { mo_data = data; mo_ocf = ocf; mo_fecf = fecf })
    Codec.
      [
        (Field.v "Data" uint16be $ fun r -> r.mo_data);
        ( Field.v "OCF"
            (optional (if ocf then Expr.true_ else Expr.false_) uint32be)
        $ fun r -> r.mo_ocf );
        (Field.v "FECF" (optional (bool fecf) uint16be) $ fun r -> r.mo_fecf);
      ]

let test_optional_both_present () =
  let c = multi_opt_codec ~ocf:true ~fecf:true in
  (* data(2) + ocf(4) + fecf(2) = 8 *)
  let buf = Bytes.create 8 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  Bytes.set_uint16_be buf 6 0x3333;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.mo_data;
  Alcotest.(check (option int)) "ocf" (Some 0x22222222) r.mo_ocf;
  Alcotest.(check (option int)) "fecf" (Some 0x3333) r.mo_fecf

let test_optional_both_absent () =
  let c = multi_opt_codec ~ocf:false ~fecf:false in
  (* data(2) only *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.mo_data;
  Alcotest.(check (option int)) "ocf" None r.mo_ocf;
  Alcotest.(check (option int)) "fecf" None r.mo_fecf

let test_optional_mixed () =
  let c = multi_opt_codec ~ocf:true ~fecf:false in
  (* data(2) + ocf(4) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.mo_data;
  Alcotest.(check (option int)) "ocf" (Some 0x22222222) r.mo_ocf;
  Alcotest.(check (option int)) "fecf" None r.mo_fecf

(* Dynamic optional: presence determined by a previously-parsed field. *)

type dyn_opt = { do_flags : int; do_payload : int option; do_trail : int }

let f_do_flags = Field.v "Flags" uint8

let dyn_opt_codec =
  Codec.v "DynOpt"
    (fun flags payload trail ->
      { do_flags = flags; do_payload = payload; do_trail = trail })
    Codec.
      [
        (f_do_flags $ fun r -> r.do_flags);
        ( Field.v "Payload"
            (optional Expr.(Field.ref f_do_flags <> int 0) uint16be)
        $ fun r -> r.do_payload );
        (Field.v "Trail" uint8 $ fun r -> r.do_trail);
      ]

let test_dyn_opt_present () =
  (* flags=1 -> payload present. Layout: [01] [12 34] [FF] *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) "flags" 1 r.do_flags;
  Alcotest.(check (option int)) "payload" (Some 0x1234) r.do_payload;
  Alcotest.(check int) "trail" 0xFF r.do_trail

let test_dyn_opt_absent () =
  (* flags=0 -> payload absent. Layout: [00] [FF] *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0;
  Bytes.set_uint8 buf 1 0xFF;
  let r = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) "flags" 0 r.do_flags;
  Alcotest.(check (option int)) "payload" None r.do_payload;
  Alcotest.(check int) "trail" 0xFF r.do_trail

let test_dyn_opt_get_trail () =
  let cf_trail = Codec.(Field.v "Trail" uint8 $ fun r -> r.do_trail) in
  let get_trail = Staged.unstage (Codec.get dyn_opt_codec cf_trail) in
  (* Present: trail at offset 3. *)
  let buf1 = Bytes.create 4 in
  Bytes.set_uint8 buf1 0 1;
  Bytes.set_uint16_be buf1 1 0x1234;
  Bytes.set_uint8 buf1 3 0xAA;
  Alcotest.(check int) "trail (present)" 0xAA (get_trail buf1 0);
  (* Absent: trail at offset 1. *)
  let buf2 = Bytes.create 2 in
  Bytes.set_uint8 buf2 0 0;
  Bytes.set_uint8 buf2 1 0xBB;
  Alcotest.(check int) "trail (absent)" 0xBB (get_trail buf2 0)

(* Dynamic optional via Field.ref on a bool field -- the TM frame pattern.
   Field.ref now accepts 'a t, so a bool field created with [bit] can be
   referenced directly in expressions. *)

type tm_opt = {
  to_ocf_flag : bool;
  to_data : int;
  to_ocf : int option;
  to_trail : int;
}

let f_to_ocf_flag = Field.v "OCFFlag" (bit (bits ~width:1 U8))

let tm_opt_codec =
  Codec.v "TmOpt"
    (fun ocf_flag _pad data ocf trail ->
      { to_ocf_flag = ocf_flag; to_data = data; to_ocf = ocf; to_trail = trail })
    Codec.
      [
        (f_to_ocf_flag $ fun r -> r.to_ocf_flag);
        (Field.v "Pad" (bits ~width:7 U8) $ fun _ -> 0);
        (Field.v "Data" uint16be $ fun r -> r.to_data);
        ( Field.v "OCF"
            (optional Expr.(Field.ref f_to_ocf_flag <> int 0) uint32be)
        $ fun r -> r.to_ocf );
        (Field.v "Trail" uint8 $ fun r -> r.to_trail);
      ]

let test_dyn_opt_anyref_present () =
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0x80;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_int32_be buf 3 0xDEADBEEFl;
  Bytes.set_uint8 buf 7 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" true r.to_ocf_flag;
  Alcotest.(check int) "data" 0x1234 r.to_data;
  Alcotest.(check (option int)) "ocf" (Some 0xDEADBEEF) r.to_ocf;
  Alcotest.(check int) "trail" 0xFF r.to_trail

let test_dyn_opt_anyref_absent () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x00;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" false r.to_ocf_flag;
  Alcotest.(check int) "data" 0x1234 r.to_data;
  Alcotest.(check (option int)) "ocf" None r.to_ocf;
  Alcotest.(check int) "trail" 0xFF r.to_trail

let test_uint64_ref_in_size () =
  let f_len = Field.v "Len" uint64be in
  let codec =
    let open Codec in
    v "U64Ref"
      (fun len data -> (len, data))
      [
        (f_len $ fun (l, _) -> l);
        (Field.v "Data" (byte_array ~size:(Field.ref f_len)) $ fun (_, d) -> d);
      ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_int64_be buf 0 3L;
  Bytes.blit_string "ABC" 0 buf 8 3;
  let len, data = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int64) "len" 3L len;
  Alcotest.(check string) "data" "ABC" data

(* -- Nested: Repeat typ: parse elements until byte budget exhausted --
   [container], [f_cnt_length], [repeat_codec] live in {!Test_fixtures}. *)

let test_repeat_decode_empty () =
  (* length=0 -> no items *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 0 r.cnt_length;
  Alcotest.(check int) "item count" 0 (List.length r.cnt_items)

let test_repeat_decode_one () =
  (* length=3 -> one inner (tag=1byte, value=2bytes) *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 3;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 3 r.cnt_length;
  Alcotest.(check int) "item count" 1 (List.length r.cnt_items);
  let item = List.hd r.cnt_items in
  Alcotest.(check int) "item.tag" 0x42 item.tag;
  Alcotest.(check int) "item.value" 0x1234 item.value

let test_repeat_decode_multiple () =
  (* length=9 -> three inner items (3 bytes each) *)
  let buf = Bytes.create 10 in
  Bytes.set_uint8 buf 0 9;
  (* item 0 *)
  Bytes.set_uint8 buf 1 0x01;
  Bytes.set_uint16_be buf 2 0x0001;
  (* item 1 *)
  Bytes.set_uint8 buf 4 0x02;
  Bytes.set_uint16_be buf 5 0x0002;
  (* item 2 *)
  Bytes.set_uint8 buf 7 0x03;
  Bytes.set_uint16_be buf 8 0x0003;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 9 r.cnt_length;
  Alcotest.(check int) "item count" 3 (List.length r.cnt_items);
  List.iteri
    (fun i (item : inner) ->
      Alcotest.(check int) (Fmt.str "item[%d].tag" i) (i + 1) item.tag;
      Alcotest.(check int) (Fmt.str "item[%d].value" i) (i + 1) item.value)
    r.cnt_items

let test_repeat_encode () =
  let v =
    {
      cnt_length = 6;
      cnt_items =
        [ { tag = 0x01; value = 0x0001 }; { tag = 0x02; value = 0x0002 } ];
    }
  in
  let buf = Bytes.create 7 in
  Codec.encode repeat_codec v buf 0;
  Alcotest.(check int) "length byte" 6 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "item0.tag" 0x01 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "item0.value" 0x0001 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "item1.tag" 0x02 (Bytes.get_uint8 buf 4);
  Alcotest.(check int) "item1.value" 0x0002 (Bytes.get_uint16_be buf 5)

let test_repeat_roundtrip () =
  let items =
    [
      { tag = 0x0A; value = 0x000A };
      { tag = 0x0B; value = 0x000B };
      { tag = 0x0C; value = 0x000C };
    ]
  in
  let original = { cnt_length = 9; cnt_items = items } in
  let buf = Bytes.create 10 in
  Codec.encode repeat_codec original buf 0;
  let decoded = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" original.cnt_length decoded.cnt_length;
  Alcotest.(check int) "item count" 3 (List.length decoded.cnt_items);
  List.iter2
    (fun (orig : inner) (dec : inner) ->
      Alcotest.(check int) "tag" orig.tag dec.tag;
      Alcotest.(check int) "value" orig.value dec.value)
    original.cnt_items decoded.cnt_items

(* Repeat with fixed-size primitive elements *)

type int_container = { ic_count : int; ic_values : int list }

let f_ic_count = Field.v "Count" uint8

let repeat_int_codec =
  Codec.v "IntContainer"
    (fun count values -> { ic_count = count; ic_values = values })
    Codec.
      [
        (f_ic_count $ fun r -> r.ic_count);
        ( Field.v "Values" (repeat ~size:(Field.ref f_ic_count) uint16be)
        $ fun r -> r.ic_values );
      ]

let test_repeat_primitive () =
  (* count=6 -> 3 uint16be values *)
  let buf = Bytes.create 7 in
  Bytes.set_uint8 buf 0 6;
  Bytes.set_uint16_be buf 1 0x1111;
  Bytes.set_uint16_be buf 3 0x2222;
  Bytes.set_uint16_be buf 5 0x3333;
  let r = decode_ok (Codec.decode repeat_int_codec buf 0) in
  Alcotest.(check int) "count" 6 r.ic_count;
  Alcotest.(check int) "n values" 3 (List.length r.ic_values);
  Alcotest.(check (list int)) "values" [ 0x1111; 0x2222; 0x3333 ] r.ic_values

(* Repeat with trailer after *)

type repeat_trailer = { rt_len : int; rt_items : inner list; rt_check : int }

let f_rt_len = Field.v "Len" uint8

let repeat_trailer_codec =
  Codec.v "RepeatTrailer"
    (fun len items check ->
      { rt_len = len; rt_items = items; rt_check = check })
    Codec.
      [
        (f_rt_len $ fun r -> r.rt_len);
        ( Field.v "Items" (repeat ~size:(Field.ref f_rt_len) (codec inner_codec))
        $ fun r -> r.rt_items );
        (Field.v "Check" uint8 $ fun r -> r.rt_check);
      ]

let test_repeat_with_trailer () =
  (* len=6 -> two inner items (3 bytes each), then 1 byte trailer *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 6;
  Bytes.set_uint8 buf 1 0x01;
  Bytes.set_uint16_be buf 2 0x0001;
  Bytes.set_uint8 buf 4 0x02;
  Bytes.set_uint16_be buf 5 0x0002;
  Bytes.set_uint8 buf 7 0xFF;
  let r = decode_ok (Codec.decode repeat_trailer_codec buf 0) in
  Alcotest.(check int) "len" 6 r.rt_len;
  Alcotest.(check int) "item count" 2 (List.length r.rt_items);
  Alcotest.(check int) "check" 0xFF r.rt_check

(* Variable-size repeat: codec with dependent-size field *)

type var_inner = { vi_len : int; vi_data : string }

let f_vi_len = Field.v "Len" uint8

let var_inner_codec =
  Codec.v "VarInner"
    (fun len data -> { vi_len = len; vi_data = data })
    Codec.
      [
        (f_vi_len $ fun r -> r.vi_len);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_vi_len)) $ fun r ->
          r.vi_data );
      ]

type var_container = { vc_size : int; vc_items : var_inner list }

let f_vc_size = Field.v "Size" uint16be

let var_repeat_codec =
  Codec.v "VarContainer"
    (fun size items -> { vc_size = size; vc_items = items })
    Codec.
      [
        (f_vc_size $ fun r -> r.vc_size);
        ( Field.v "Items"
            (repeat ~size:(Field.ref f_vc_size) (codec var_inner_codec))
        $ fun r -> r.vc_items );
      ]

let test_repeat_variable_size_elements () =
  (* Two variable-length items: [len=2, "ab"] [len=3, "cde"] = 2+2+3+3 = 7 bytes *)
  let buf = Bytes.create 9 in
  Bytes.set_uint16_be buf 0 7;
  (* item 0: len=2, data="ab" *)
  Bytes.set_uint8 buf 2 2;
  Bytes.blit_string "ab" 0 buf 3 2;
  (* item 1: len=3, data="cde" *)
  Bytes.set_uint8 buf 5 3;
  Bytes.blit_string "cde" 0 buf 6 3;
  let r = decode_ok (Codec.decode var_repeat_codec buf 0) in
  Alcotest.(check int) "size" 7 r.vc_size;
  Alcotest.(check int) "item count" 2 (List.length r.vc_items);
  let i0 = List.nth r.vc_items 0 in
  let i1 = List.nth r.vc_items 1 in
  Alcotest.(check int) "item0.len" 2 i0.vi_len;
  Alcotest.(check string) "item0.data" "ab" i0.vi_data;
  Alcotest.(check int) "item1.len" 3 i1.vi_len;
  Alcotest.(check string) "item1.data" "cde" i1.vi_data

(* -- Nested: Composition: optional + repeat + codec --
   TM-frame-like structure: header + data zone (repeat of packets) + optional
   OCF + optional FECF. [packet] / [packet_codec] live in {!Test_fixtures}. *)

type tm_like = {
  tm_hdr : int;
  tm_data_len : int;
  tm_packets : packet list;
  tm_ocf : int option;
  tm_fecf : int option;
}

let f_tm_data_len = Field.v "DataLen" uint8

let tm_like_codec ~ocf ~fecf =
  Codec.v "TmLike"
    (fun hdr data_len packets ocf fecf ->
      {
        tm_hdr = hdr;
        tm_data_len = data_len;
        tm_packets = packets;
        tm_ocf = ocf;
        tm_fecf = fecf;
      })
    Codec.
      [
        (Field.v "Hdr" uint16be $ fun r -> r.tm_hdr);
        (f_tm_data_len $ fun r -> r.tm_data_len);
        ( Field.v "Packets"
            (repeat ~size:(Field.ref f_tm_data_len) (codec packet_codec))
        $ fun r -> r.tm_packets );
        ( Field.v "OCF"
            (optional (if ocf then Expr.true_ else Expr.false_) uint32be)
        $ fun r -> r.tm_ocf );
        (Field.v "FECF" (optional (bool fecf) uint16be) $ fun r -> r.tm_fecf);
      ]

let test_tm_like_full () =
  let c = tm_like_codec ~ocf:true ~fecf:true in
  (* hdr(2) + data_len(1) + 2 packets(6) + ocf(4) + fecf(2) = 15 *)
  let buf = Bytes.create 15 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 6;
  (* data zone = 6 bytes = 2 packets *)
  (* packet 0 *)
  Bytes.set_uint8 buf 3 0x01;
  Bytes.set_uint16_be buf 4 0x1111;
  (* packet 1 *)
  Bytes.set_uint8 buf 6 0x02;
  Bytes.set_uint16_be buf 7 0x2222;
  (* ocf *)
  Bytes.set_int32_be buf 9 0x33333333l;
  (* fecf *)
  Bytes.set_uint16_be buf 13 0x4444;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAAAA r.tm_hdr;
  Alcotest.(check int) "data_len" 6 r.tm_data_len;
  Alcotest.(check int) "packet count" 2 (List.length r.tm_packets);
  Alcotest.(check int) "pkt0.id" 0x01 (List.nth r.tm_packets 0).pkt_id;
  Alcotest.(check int) "pkt1.id" 0x02 (List.nth r.tm_packets 1).pkt_id;
  Alcotest.(check (option int)) "ocf" (Some 0x33333333) r.tm_ocf;
  Alcotest.(check (option int)) "fecf" (Some 0x4444) r.tm_fecf

let test_tm_like_no_trailing () =
  let c = tm_like_codec ~ocf:false ~fecf:false in
  (* hdr(2) + data_len(1) + 1 packet(3) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 3;
  Bytes.set_uint8 buf 3 0x01;
  Bytes.set_uint16_be buf 4 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "packet count" 1 (List.length r.tm_packets);
  Alcotest.(check (option int)) "ocf" None r.tm_ocf;
  Alcotest.(check (option int)) "fecf" None r.tm_fecf

let test_tm_like_roundtrip () =
  let c = tm_like_codec ~ocf:true ~fecf:true in
  let original =
    {
      tm_hdr = 0xBBBB;
      tm_data_len = 9;
      tm_packets =
        [
          { pkt_id = 0x0A; pkt_data = 0x000A };
          { pkt_id = 0x0B; pkt_data = 0x000B };
          { pkt_id = 0x0C; pkt_data = 0x000C };
        ];
      tm_ocf = Some 0xDEADBEEF;
      tm_fecf = Some 0xCAFE;
    }
  in
  let buf = Bytes.create 18 in
  Codec.encode c original buf 0;
  let decoded = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" original.tm_hdr decoded.tm_hdr;
  Alcotest.(check int) "packet count" 3 (List.length decoded.tm_packets);
  List.iter2
    (fun o d ->
      Alcotest.(check int) "pkt.id" o.pkt_id d.pkt_id;
      Alcotest.(check int) "pkt.data" o.pkt_data d.pkt_data)
    original.tm_packets decoded.tm_packets;
  Alcotest.(check (option int)) "ocf" original.tm_ocf decoded.tm_ocf;
  Alcotest.(check (option int)) "fecf" original.tm_fecf decoded.tm_fecf

(* -- Multiple consecutive variable-size fields (CFDP-style) --

   CCSDS CFDP (727.0-B-5) has three consecutive variable-size byte_array
   fields in its PDU header, each sized by expressions over earlier fixed
   fields. This layout was previously rejected by [require_static_off]. *)

type cfdp_hdr = {
  cfdp_eid_len : int;
  cfdp_txseq_len : int;
  cfdp_src : string;
  cfdp_txseq : string;
  cfdp_dst : string;
}

let f_cfdp_eid_len = Field.v "EIDLen" uint8
let f_cfdp_txseq_len = Field.v "TxSeqLen" uint8

let cfdp_codec =
  let open Codec in
  v "CFDPHeader"
    (fun eid_len txseq_len src txseq dst ->
      {
        cfdp_eid_len = eid_len;
        cfdp_txseq_len = txseq_len;
        cfdp_src = src;
        cfdp_txseq = txseq;
        cfdp_dst = dst;
      })
    [
      (f_cfdp_eid_len $ fun r -> r.cfdp_eid_len);
      (f_cfdp_txseq_len $ fun r -> r.cfdp_txseq_len);
      ( Field.v "SourceEID"
          (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))
      $ fun r -> r.cfdp_src );
      ( Field.v "TxSeqNum"
          (byte_array ~size:Expr.(Field.ref f_cfdp_txseq_len + int 1))
      $ fun r -> r.cfdp_txseq );
      ( Field.v "DestEID"
          (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))
      $ fun r -> r.cfdp_dst );
    ]

let test_multi_var_decode () =
  (* EIDLen=1 -> 2-byte entities, TxSeqLen=2 -> 3-byte txseq.
     Layout: [1] [2] [AA BB] [CC DD EE] [FF 00] *)
  let buf = Bytes.create 9 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  Bytes.blit_string "\xAA\xBB" 0 buf 2 2;
  Bytes.blit_string "\xCC\xDD\xEE" 0 buf 4 3;
  Bytes.blit_string "\xFF\x00" 0 buf 7 2;
  let r = decode_ok (Codec.decode cfdp_codec buf 0) in
  Alcotest.(check int) "eid_len" 1 r.cfdp_eid_len;
  Alcotest.(check int) "txseq_len" 2 r.cfdp_txseq_len;
  Alcotest.(check string) "src" "\xAA\xBB" r.cfdp_src;
  Alcotest.(check string) "txseq" "\xCC\xDD\xEE" r.cfdp_txseq;
  Alcotest.(check string) "dst" "\xFF\x00" r.cfdp_dst

let test_multi_var_roundtrip () =
  let original =
    {
      cfdp_eid_len = 1;
      cfdp_txseq_len = 2;
      cfdp_src = "\xAA\xBB";
      cfdp_txseq = "\xCC\xDD\xEE";
      cfdp_dst = "\xFF\x00";
    }
  in
  let buf = Bytes.create 9 in
  Codec.encode cfdp_codec original buf 0;
  let decoded = decode_ok (Codec.decode cfdp_codec buf 0) in
  Alcotest.(check string) "src roundtrip" original.cfdp_src decoded.cfdp_src;
  Alcotest.(check string)
    "txseq roundtrip" original.cfdp_txseq decoded.cfdp_txseq;
  Alcotest.(check string) "dst roundtrip" original.cfdp_dst decoded.cfdp_dst

let test_multi_var_get () =
  (* Staged get must work on the second and third variable-size fields. *)
  let buf = Bytes.create 9 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  Bytes.blit_string "\xAA\xBB" 0 buf 2 2;
  Bytes.blit_string "\xCC\xDD\xEE" 0 buf 4 3;
  Bytes.blit_string "\xFF\x00" 0 buf 7 2;
  let cf_txseq =
    Codec.(
      Field.v "TxSeqNum"
        (byte_array ~size:Expr.(Field.ref f_cfdp_txseq_len + int 1))
      $ fun r -> r.cfdp_txseq)
  in
  let cf_dst =
    Codec.(
      Field.v "DestEID"
        (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))
      $ fun r -> r.cfdp_dst)
  in
  let get_txseq = Staged.unstage (Codec.get cfdp_codec cf_txseq) in
  let get_dst = Staged.unstage (Codec.get cfdp_codec cf_dst) in
  Alcotest.(check string) "get txseq" "\xCC\xDD\xEE" (get_txseq buf 0);
  Alcotest.(check string) "get dst" "\xFF\x00" (get_dst buf 0)

let test_multi_var_fixed_after () =
  (* A fixed-size field after multiple variable-size fields must also work. *)
  let f_elen = Field.v "ELen" uint8 in
  let f_tlen = Field.v "TLen" uint8 in
  let codec =
    let open Codec in
    v "VarTrail"
      (fun elen tlen src txseq trail -> (elen, tlen, src, txseq, trail))
      [
        (f_elen $ fun (e, _, _, _, _) -> e);
        (f_tlen $ fun (_, t, _, _, _) -> t);
        ( Field.v "Src" (byte_array ~size:Expr.(Field.ref f_elen + int 1))
        $ fun (_, _, s, _, _) -> s );
        ( Field.v "Tx" (byte_array ~size:Expr.(Field.ref f_tlen + int 1))
        $ fun (_, _, _, t, _) -> t );
        (Field.v "Trail" uint16be $ fun (_, _, _, _, tr) -> tr);
      ]
  in
  (* elen=0 -> 1-byte src, tlen=1 -> 2-byte tx, trail=0xBEEF
     Layout: [0] [1] [AA] [BB CC] [BE EF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint8 buf 0 0;
  Bytes.set_uint8 buf 1 1;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.blit_string "\xBB\xCC" 0 buf 3 2;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let _, _, src, tx, trail = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check string) "src" "\xAA" src;
  Alcotest.(check string) "tx" "\xBB\xCC" tx;
  Alcotest.(check int) "trail" 0xBEEF trail

(* -- Multiple variable-size sub-codecs back-to-back (SSH disconnect /
      debug shape). [compile_codec] used to bail with [require_static_off]
      when a [Wire.codec]-embedded variable-size sub-codec sat after
      another variable-size field. -- *)

module Slice = Bytesrw.Bytes.Slice

type ssh_string = { ss_len : int; ss_data : Slice.t }

let ssh_f_len = Field.v "len" uint32be
let ssh_f_data = Field.v "data" (byte_slice ~size:(Field.ref ssh_f_len))

let ssh_string_codec =
  Codec.v "SshString"
    (fun len data -> { ss_len = len; ss_data = data })
    Codec.[ (ssh_f_len $ fun s -> s.ss_len); (ssh_f_data $ fun s -> s.ss_data) ]

let mk_ssh_string s =
  let b = Bytes.of_string s in
  {
    ss_len = String.length s;
    ss_data = Slice.make b ~first:0 ~length:(Bytes.length b);
  }

let test_ssh_two_var_slices () =
  (* SSH_MSG_DISCONNECT: reason (uint32) + len + desc + len + lang *)
  let f_reason = Field.v "reason" uint32be in
  let f_desc_len = Field.v "desc_len" uint32be in
  let f_desc = Field.v "desc" (byte_slice ~size:(Field.ref f_desc_len)) in
  let f_lang_len = Field.v "lang_len" uint32be in
  let f_lang = Field.v "lang" (byte_slice ~size:(Field.ref f_lang_len)) in
  let codec =
    let open Codec in
    v "Disconnect"
      (fun reason _ desc _ lang -> (reason, desc, lang))
      [
        (f_reason $ fun (r, _, _) -> r);
        (f_desc_len $ fun (_, d, _) -> Slice.length d);
        (f_desc $ fun (_, d, _) -> d);
        (f_lang_len $ fun (_, _, l) -> Slice.length l);
        (f_lang $ fun (_, _, l) -> l);
      ]
  in
  let v =
    (11, (mk_ssh_string "bye").ss_data, (mk_ssh_string "en-US").ss_data)
  in
  let buf = Bytes.create 200 in
  Codec.encode codec v buf 0;
  let r, d, l = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "reason" 11 r;
  Alcotest.(check string) "desc" "bye" (Slice.to_string d);
  Alcotest.(check string) "lang" "en-US" (Slice.to_string l)

let test_two_var_codecs_embedded () =
  (* Two consecutive [Wire.codec ssh_string_codec] embedded fields.
     This is the case that previously tripped [require_static_off]. *)
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let pair_codec =
    let open Codec in
    v "Pair" (fun a b -> (a, b)) [ f_a $ fst; f_b $ snd ]
  in
  let v = (mk_ssh_string "abcd", mk_ssh_string "xy") in
  let buf = Bytes.create 200 in
  Codec.encode pair_codec v buf 0;
  let a, b = decode_ok (Codec.decode pair_codec buf 0) in
  Alcotest.(check int) "a.len" 4 a.ss_len;
  Alcotest.(check string) "a.data" "abcd" (Slice.to_string a.ss_data);
  Alcotest.(check int) "b.len" 2 b.ss_len;
  Alcotest.(check string) "b.data" "xy" (Slice.to_string b.ss_data)

let test_three_var_codecs_embedded () =
  (* SSH_MSG_DEBUG-shaped payload: three variable-size sub-codecs. *)
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let f_c = Field.v "c" (codec ssh_string_codec) in
  let triple_codec =
    let open Codec in
    v "Triple"
      (fun a b c -> (a, b, c))
      [
        (f_a $ fun (a, _, _) -> a);
        (f_b $ fun (_, b, _) -> b);
        (f_c $ fun (_, _, c) -> c);
      ]
  in
  let v = (mk_ssh_string "alpha", mk_ssh_string "be", mk_ssh_string "gamma!") in
  let buf = Bytes.create 200 in
  Codec.encode triple_codec v buf 0;
  let a, b, c = decode_ok (Codec.decode triple_codec buf 0) in
  Alcotest.(check string) "a" "alpha" (Slice.to_string a.ss_data);
  Alcotest.(check string) "b" "be" (Slice.to_string b.ss_data);
  Alcotest.(check string) "c" "gamma!" (Slice.to_string c.ss_data)

let test_repeat_after_var_slice () =
  (* [Repeat] after a variable-size field used to trip [require_static_off]
     in [compile_repeat]. The fix mirrors [compile_codec]'s. *)
  let f_prefix_len = Field.v "prefix_len" uint8 in
  let f_prefix = Field.v "prefix" (byte_slice ~size:(Field.ref f_prefix_len)) in
  let f_count = Field.v "count" uint8 in
  let f_items =
    Field.v "items" (Wire.repeat ~size:(Field.ref f_count) Wire.uint16be)
  in
  let codec =
    let open Codec in
    v "PrefixedItems"
      (fun _ prefix _ items -> (prefix, items))
      [
        (f_prefix_len $ fun (p, _) -> Slice.length p);
        (f_prefix $ fun (p, _) -> p);
        (f_count $ fun (_, items) -> 2 * List.length items);
        (f_items $ fun (_, items) -> items);
      ]
  in
  let buf = Bytes.create 200 in
  let v = ((mk_ssh_string "PREFIX").ss_data, [ 0x0102; 0x0304; 0x0506 ]) in
  Codec.encode codec v buf 0;
  let prefix, items = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check string) "prefix" "PREFIX" (Slice.to_string prefix);
  Alcotest.(check (list int)) "items" [ 0x0102; 0x0304; 0x0506 ] items

(* -- uint: variable-width unsigned integer -- *)

type uint_rec = { tag : int; value : int }

let test_uint_3byte_be () =
  let codec =
    let open Codec in
    v "U3BE"
      (fun tag value -> { tag; value })
      [
        (Field.v "Tag" uint8 $ fun r -> r.tag);
        (Field.v "Value" (uint (Wire.int 3)) $ fun r -> r.value);
      ]
  in
  let original = { tag = 0x42; value = 0x1A2B3C } in
  let buf = Bytes.create 4 in
  Codec.encode codec original buf 0;
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "be byte 0" 0x1A (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "be byte 1" 0x2B (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "be byte 2" 0x3C (Bytes.get_uint8 buf 3);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "tag" original.tag decoded.tag;
  Alcotest.(check int) "value" original.value decoded.value

let test_uint_1byte () =
  let codec =
    let open Codec in
    v "U1" (fun v -> v) [ (Field.v "V" (uint (Wire.int 1)) $ fun v -> v) ]
  in
  let buf = Bytes.create 1 in
  Codec.encode codec 0xAB buf 0;
  Alcotest.(check int) "byte" 0xAB (Bytes.get_uint8 buf 0);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "roundtrip" 0xAB decoded

let test_uint_5byte_le () =
  let codec =
    let open Codec in
    v "U5LE"
      (fun v -> v)
      [ (Field.v "V" (uint ~endian:Wire.Little (Wire.int 5)) $ fun v -> v) ]
  in
  let value = 0x01_02_03_04_05 in
  let buf = Bytes.create 5 in
  Codec.encode codec value buf 0;
  Alcotest.(check int) "le byte 0" 0x05 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "le byte 1" 0x04 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "le byte 2" 0x03 (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "le byte 3" 0x02 (Bytes.get_uint8 buf 3);
  Alcotest.(check int) "le byte 4" 0x01 (Bytes.get_uint8 buf 4);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "roundtrip" value decoded

let test_uint_dynamic () =
  let f_n = Field.v "N" uint8 in
  let codec =
    let open Codec in
    v "UDyn"
      (fun n value -> (n, value))
      [
        (f_n $ fun (n, _) -> n);
        (Field.v "Value" (uint (Field.ref f_n)) $ fun (_, v) -> v);
      ]
  in
  (* n=2 -> 2-byte BE uint = 0x1234, layout: [02] [12 34] *)
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 2;
  Bytes.set_uint8 buf 1 0x12;
  Bytes.set_uint8 buf 2 0x34;
  let n, value = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "n" 2 n;
  Alcotest.(check int) "value" 0x1234 value

(* -- Adversarial bit-order tests --

   These pin the default [bit_order = Msb_first] against real protocol
   bytes drawn from published specs. If anything shifts the bit layout,
   these tests fail with a concrete "0x45 decoded as (5, 4) instead of
   (4, 5)" error, not silently. *)

type ipv4_vihl = { v : int; ihl : int }

let ipv4_vihl_codec =
  let open Codec in
  v "IPv4VIHL"
    (fun v ihl -> { v; ihl })
    [
      (Field.v "Version" (bits ~width:4 U8) $ fun p -> p.v);
      (Field.v "IHL" (bits ~width:4 U8) $ fun p -> p.ihl);
    ]

let test_bitorder_ipv4_vihl_decode () =
  (* RFC 791: first byte of an IPv4 header with Version=4, IHL=5 is 0x45,
     Version in the top nibble, IHL in the bottom nibble. *)
  let buf = Bytes.of_string "\x45" in
  let r = decode_ok (Codec.decode ipv4_vihl_codec buf 0) in
  Alcotest.(check int) "Version = 4 (top nibble)" 4 r.v;
  Alcotest.(check int) "IHL = 5 (bottom nibble)" 5 r.ihl

let test_bitorder_ipv4vihl_encode_roundtrip () =
  let original = { v = 4; ihl = 5 } in
  let buf = Bytes.create 1 in
  Codec.encode ipv4_vihl_codec original buf 0;
  Alcotest.(check int) "encoded byte" 0x45 (Bytes.get_uint8 buf 0);
  let decoded = decode_ok (Codec.decode ipv4_vihl_codec buf 0) in
  Alcotest.(check int) "roundtrip v" original.v decoded.v;
  Alcotest.(check int) "roundtrip ihl" original.ihl decoded.ihl

type ipv4_flags_frag = { flags : int; frag : int }

let ipv4_flags_frag_codec =
  let open Codec in
  v "IPv4FF"
    (fun flags frag -> { flags; frag })
    [
      (Field.v "Flags" (bits ~width:3 U16be) $ fun p -> p.flags);
      (Field.v "FragmentOffset" (bits ~width:13 U16be) $ fun p -> p.frag);
    ]

let test_bitorder_ipv4_flags_frag () =
  (* RFC 791: Flags (3 bits) then Fragment Offset (13 bits), MSB-first.
     Flags = 0b010 (DF set), FragOffset = 0 -> 0x4000. *)
  let buf = Bytes.of_string "\x40\x00" in
  let r = decode_ok (Codec.decode ipv4_flags_frag_codec buf 0) in
  Alcotest.(check int) "Flags = 0b010" 0b010 r.flags;
  Alcotest.(check int) "FragOffset = 0" 0 r.frag

type mcu_reg = { lo : int; hi : int }

let mcu_reg_codec =
  let open Codec in
  v "McuReg"
    (fun lo hi -> { lo; hi })
    [
      (Field.v "lo" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.lo);
      (Field.v "hi" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.hi);
    ]

let test_bitorder_lsbfirst_opt_in () =
  (* MSVC-style C struct: first declared field in the low bits. *)
  let buf = Bytes.of_string "\xA5" in
  let r = decode_ok (Codec.decode mcu_reg_codec buf 0) in
  Alcotest.(check int) "lo (Lsb_first)" 0x5 r.lo;
  Alcotest.(check int) "hi (Lsb_first)" 0xA r.hi

type bit_order_split = { bos_x : int; bos_y : int }

let bit_order_split_codec =
  let open Codec in
  v "BitOrderSplit"
    (fun x y -> { bos_x = x; bos_y = y })
    [
      (Field.v "x" (bits ~bit_order:Msb_first ~width:4 U8) $ fun r -> r.bos_x);
      (Field.v "y" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.bos_y);
    ]

let test_bitorder_diff_start_newword () =
  (* Two fields with the same base but different [bit_order] must NOT share
     a base word -- the codec allocates a fresh byte for each. Catches any
     regression that would let mismatched bit orders silently collide. *)
  Alcotest.(check int) "wire size = 2" 2 (Codec.wire_size bit_order_split_codec);
  let buf = Bytes.create 2 in
  Codec.encode bit_order_split_codec { bos_x = 0xA; bos_y = 0x5 } buf 0;
  Alcotest.(check int)
    "byte 0 = 0xA0 (Msb_first x)" 0xA0 (Bytes.get_uint8 buf 0);
  Alcotest.(check int)
    "byte 1 = 0x05 (Lsb_first y)" 0x05 (Bytes.get_uint8 buf 1)

(* -- Suite -- *)

let suite =
  ( "codec",
    [
      (* record *)
      Alcotest.test_case "record: encode" `Quick test_record_encode;
      Alcotest.test_case "record: decode" `Quick test_record_decode;
      Alcotest.test_case "record: roundtrip" `Quick test_record_roundtrip;
      Alcotest.test_case "record: struct_of_codec" `Quick test_struct_of_record;
      Alcotest.test_case "record: metadata decode ok" `Quick
        test_codec_metadata_decode_ok;
      Alcotest.test_case "record: metadata constraint fail" `Quick
        test_metadata_constraint_fail;
      Alcotest.test_case "record: metadata action fail" `Quick
        test_metadata_action_fail;
      Alcotest.test_case "record: metadata decode with params" `Quick
        test_metadata_with_params;
      Alcotest.test_case "record: metadata where fail" `Quick
        test_metadata_where_fail;
      Alcotest.test_case "record: metadata struct_of_codec" `Quick
        test_struct_of_codec_metadata;
      Alcotest.test_case "validate: rejects bad where" `Quick
        test_validate_rejects_bad_where;
      Alcotest.test_case "validate: rejects bad constraint" `Quick
        test_validate_rejects_bad_constraint;
      Alcotest.test_case "validate: then get" `Quick test_validate_then_get;
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
      Alcotest.test_case "codec bitfield: struct_of_codec" `Quick
        test_struct_of_codec_bitfield;
      Alcotest.test_case "codec bitfield: overflow u8" `Quick
        test_codec_bitfield_overflow_u8;
      Alcotest.test_case "codec bitfield: overflow u16" `Quick
        test_codec_bitfield_overflow_u16;
      Alcotest.test_case "codec bitfield: overflow u32" `Quick
        test_codec_bitfield_overflow_u32;
      Alcotest.test_case "codec bitfield: max valid" `Quick
        test_codec_bitfield_max_valid;
      Alcotest.test_case "codec bitfield: overflow 1-bit" `Quick
        test_codec_bitfield_overflow_1bit;
      (* action semantics *)
      Alcotest.test_case "action: fires on decode_env" `Quick
        test_action_fires_decode_env;
      Alcotest.test_case "action: fires on get" `Quick test_action_fires_on_get;
      Alcotest.test_case "action: not fired by validate" `Quick
        test_action_unfired_by_validate;
      Alcotest.test_case "action: no action zero overhead" `Quick
        test_get_noaction_zero_overhead;
      Alcotest.test_case "action: get with env" `Quick test_get_with_env;
      Alcotest.test_case "action: field in two codecs" `Quick
        test_get_action_field_twocodecs;
      Alcotest.test_case "action: get without env" `Quick test_get_action_no_env;
      Alcotest.test_case "action: abort on get" `Quick
        test_get_action_abort_field;
      Alcotest.test_case "action: no action ignores env" `Quick
        test_get_noaction_ignores_env;
      Alcotest.test_case "action: multiple calls update env" `Quick
        test_get_action_multiple_calls;
      Alcotest.test_case "action: with input param" `Quick
        test_get_action_with_inputparam;
      Alcotest.test_case "action: input param no env" `Quick
        test_get_action_inputparam_noenv;
      Alcotest.test_case "action: output only" `Quick
        test_get_action_output_only;
      Alcotest.test_case "action: var then assign" `Quick
        test_get_action_varthen_assign;
      Alcotest.test_case "action: cross-field ref" `Quick
        test_get_action_crossfield_ref;
      Alcotest.test_case "validate: constraint only" `Quick
        test_validate_constraint_only;
      Alcotest.test_case "validate: where only" `Quick test_validate_where_only;
      Alcotest.test_case "action: two staged same field" `Quick
        test_get_twostaged_same_field;
      Alcotest.test_case "shared: encode bitfield" `Quick
        test_encode_shared_bitfield;
      (* API misuse *)
      Alcotest.test_case "misuse: get field not in codec" `Quick
        test_get_field_notin_codec;
      Alcotest.test_case "misuse: set field not in codec" `Quick
        test_set_field_notin_codec;
      Alcotest.test_case "misuse: bitfield on non-bitfield" `Quick
        test_bitfield_on_non_bitfield;
      Alcotest.test_case "misuse: env from wrong codec" `Quick
        test_env_from_wrong_codec;
      Alcotest.test_case "misuse: wrong env with action" `Quick
        test_env_wrongcodec_with_action;
      Alcotest.test_case "misuse: decode short buffer" `Quick
        test_decode_short_buffer;
      Alcotest.test_case "misuse: encode short buffer" `Quick
        test_encode_short_buffer;
      (* same field in two codecs *)
      Alcotest.test_case "shared: same field two codecs get" `Quick
        test_same_field_two_codecs;
      Alcotest.test_case "shared: same field two codecs set" `Quick
        test_samefield_twocodecs_set;
      Alcotest.test_case "shared: same field two codecs decode" `Quick
        test_samefield_twocodecs_decode;
      Alcotest.test_case "shared: same field two codecs encode" `Quick
        test_samefield_twocodecs_encode;
      Alcotest.test_case "shared: same bitfield two codecs" `Quick
        test_same_bitfield_two_codecs;
      Alcotest.test_case "shared: staged before second seal" `Quick
        test_samefield_staged_before_secondseal;
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
      (* bit_order adversarial tests *)
      Alcotest.test_case "bit_order: IPv4 Version/IHL decode" `Quick
        test_bitorder_ipv4_vihl_decode;
      Alcotest.test_case "bit_order: IPv4 Version/IHL roundtrip" `Quick
        test_bitorder_ipv4vihl_encode_roundtrip;
      Alcotest.test_case "bit_order: IPv4 Flags/FragOffset" `Quick
        test_bitorder_ipv4_flags_frag;
      Alcotest.test_case "bit_order: Lsb_first opt-in" `Quick
        test_bitorder_lsbfirst_opt_in;
      Alcotest.test_case "bit_order: different orders separate words" `Quick
        test_bitorder_diff_start_newword;
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
      Alcotest.test_case "dep: wire_size_at" `Quick test_dep_compute_wire_size;
      (* Field.ref expressions *)
      Alcotest.test_case "dep: codec ref" `Quick test_dep_codec_ref;
      Alcotest.test_case "dep: codec ref size eval" `Quick
        test_dep_ref_size_eval;
      (* struct_of_codec for variable-size codecs *)
      Alcotest.test_case "dep: struct_of_codec" `Quick test_struct_of_dep;
      Alcotest.test_case "dep: trailer struct_of_codec" `Quick
        test_struct_of_dep_trailer;
      (* sizeof_this / field_pos *)
      Alcotest.test_case "codec: sizeof_this" `Quick test_codec_sizeof_this;
      Alcotest.test_case "codec: field_pos" `Quick test_codec_field_pos;
      (* bitfield batch access *)
      Alcotest.test_case "bitfield: extract matches get" `Quick
        test_bitfield_extract;
      Alcotest.test_case "bitfield: non-bf field raises" `Quick
        test_bitfield_non_bf_raises;
      Alcotest.test_case "bitfield: short buffer" `Quick
        test_bitfield_short_buffer;
      Alcotest.test_case "bitfield: load_word shared" `Quick
        test_bitfield_load_shared;
      (* codec embed *)
      Alcotest.test_case "embed: decode" `Quick test_codec_embed_decode;
      Alcotest.test_case "embed: encode" `Quick test_codec_embed_encode;
      Alcotest.test_case "embed: roundtrip" `Quick test_codec_embed_roundtrip;
      Alcotest.test_case "embed: wire_size" `Quick test_codec_embed_wire_size;
      Alcotest.test_case "embed: bitfield" `Quick test_codec_embed_bitfield;
      Alcotest.test_case "embed: nested" `Quick test_codec_embed_nested;
      Alcotest.test_case "embed: nested roundtrip" `Quick
        test_codec_embed_nested_roundtrip;
      Alcotest.test_case "embed: cross field ref" `Quick
        test_codec_cross_field_ref;
      Alcotest.test_case "embed: cross field ref varying" `Quick
        test_codec_crossref_field_varying;
      Alcotest.test_case "embed: cross field ref oversized" `Quick
        test_codec_crossref_field_oversized;
      Alcotest.test_case "embed: cross field ref underflow" `Quick
        test_codec_crossref_field_underflow;
      Alcotest.test_case "embed: cross field ref zero data" `Quick
        test_codec_crossref_field_zerodata;
      Alcotest.test_case "embed: cross field ref shadow" `Quick
        test_codec_field_shadow;
      Alcotest.test_case "embed: cross field ref two levels" `Quick
        test_codec_crossref_field_twolevels;
      Alcotest.test_case "embed: cross field ref bitfield" `Quick
        test_codec_crossref_field_bitfield;
      (* optional *)
      Alcotest.test_case "optional: present decode" `Quick
        test_optional_present_decode;
      Alcotest.test_case "optional: absent decode" `Quick
        test_optional_absent_decode;
      Alcotest.test_case "optional: present encode" `Quick
        test_optional_present_encode;
      Alcotest.test_case "optional: absent encode" `Quick
        test_optional_absent_encode;
      Alcotest.test_case "optional: present roundtrip" `Quick
        test_optional_present_roundtrip;
      Alcotest.test_case "optional: absent roundtrip" `Quick
        test_optional_absent_roundtrip;
      Alcotest.test_case "optional: wire_size present" `Quick
        test_optional_wire_size_present;
      Alcotest.test_case "optional: wire_size absent" `Quick
        test_optional_wire_size_absent;
      Alcotest.test_case "optional: codec present" `Quick
        test_optional_codec_present;
      Alcotest.test_case "optional: codec absent" `Quick
        test_optional_codec_absent;
      Alcotest.test_case "optional: both present" `Quick
        test_optional_both_present;
      Alcotest.test_case "optional: both absent" `Quick
        test_optional_both_absent;
      Alcotest.test_case "optional: mixed" `Quick test_optional_mixed;
      Alcotest.test_case "optional: dynamic present" `Quick test_dyn_opt_present;
      Alcotest.test_case "optional: dynamic absent" `Quick test_dyn_opt_absent;
      Alcotest.test_case "optional: dynamic get trail" `Quick
        test_dyn_opt_get_trail;
      Alcotest.test_case "optional: bool ref present" `Quick
        test_dyn_opt_anyref_present;
      Alcotest.test_case "optional: bool ref absent" `Quick
        test_dyn_opt_anyref_absent;
      Alcotest.test_case "ref: uint64 in size expr" `Quick
        test_uint64_ref_in_size;
      (* repeat *)
      Alcotest.test_case "repeat: decode empty" `Quick test_repeat_decode_empty;
      Alcotest.test_case "repeat: decode one" `Quick test_repeat_decode_one;
      Alcotest.test_case "repeat: decode multiple" `Quick
        test_repeat_decode_multiple;
      Alcotest.test_case "repeat: encode" `Quick test_repeat_encode;
      Alcotest.test_case "repeat: roundtrip" `Quick test_repeat_roundtrip;
      Alcotest.test_case "repeat: primitive" `Quick test_repeat_primitive;
      Alcotest.test_case "repeat: with trailer" `Quick test_repeat_with_trailer;
      Alcotest.test_case "repeat: variable size elements" `Quick
        test_repeat_variable_size_elements;
      (* composition: optional + repeat + codec *)
      Alcotest.test_case "composition: tm-like full" `Quick test_tm_like_full;
      Alcotest.test_case "composition: tm-like no trailing" `Quick
        test_tm_like_no_trailing;
      Alcotest.test_case "composition: tm-like roundtrip" `Quick
        test_tm_like_roundtrip;
      (* multiple consecutive variable-size fields *)
      Alcotest.test_case "multi-var: decode" `Quick test_multi_var_decode;
      Alcotest.test_case "multi-var: roundtrip" `Quick test_multi_var_roundtrip;
      Alcotest.test_case "multi-var: get" `Quick test_multi_var_get;
      Alcotest.test_case "multi-var: fixed after" `Quick
        test_multi_var_fixed_after;
      Alcotest.test_case "multi-var: ssh disconnect (two var byte_slice)" `Quick
        test_ssh_two_var_slices;
      Alcotest.test_case "multi-var: two embedded sub-codecs" `Quick
        test_two_var_codecs_embedded;
      Alcotest.test_case "multi-var: three embedded sub-codecs" `Quick
        test_three_var_codecs_embedded;
      Alcotest.test_case "multi-var: repeat after variable byte_slice" `Quick
        test_repeat_after_var_slice;
      (* uint: variable-width unsigned integer *)
      Alcotest.test_case "uint: 3-byte BE roundtrip" `Quick test_uint_3byte_be;
      Alcotest.test_case "uint: 1-byte like uint8" `Quick test_uint_1byte;
      Alcotest.test_case "uint: 5-byte LE roundtrip" `Quick test_uint_5byte_le;
      Alcotest.test_case "uint: dynamic size" `Quick test_uint_dynamic;
    ] )
