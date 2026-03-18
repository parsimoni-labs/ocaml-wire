(* Tests for codec.ml: Codec.get/set/view *)

open Wire
open Wire.C

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let decode_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

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
  else Codec.decode codec (Bytes.of_string s) 0

(* ── Record codec tests ── *)

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
  let s = C.struct_of_codec simple_record_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a;" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b;" output);
  Alcotest.(check bool) "contains field c" true (contains ~sub:"c;" output)

type meta_record = { x : int }

let meta_codec =
  Codec.view "MetaRecord"
    ~where:Expr.(Wire.field_ref "x" = int 8)
    (fun x -> { x })
    Codec.
      [
        Codec.field "x"
          ~constraint_:Expr.(Wire.field_ref "x" <= int 10)
          ~action:
            (Action.on_success
               [
                 Action.return_bool Expr.(Wire.field_ref "x" mod int 2 = int 0);
               ])
          uint8
          (fun r -> r.x);
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

let projection_codec =
  let outx = Param.output "outx" uint8 in
  Codec.view "ProjectionCodec"
    ~params:[ Param.v (Param.input "limit" uint8); Param.v outx ]
    ~where:Expr.(Wire.field_ref "x" <= Wire.field_ref "limit")
    (fun x -> { x })
    Codec.
      [
        Codec.field "x"
          ~constraint_:Expr.(Wire.field_ref "x" <= int 8)
          ~action:
            (Action.on_success [ Action.assign outx (Wire.field_ref "x") ])
          uint8
          (fun r -> r.x);
      ]

let test_metadata_with_params () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let buf = Bytes.of_string "\x08" in
  let params =
    Param.empty |> fun env ->
    Param.bind env limit 10 |> fun env -> Param.init env outx 0
  in
  let v = decode_ok (Codec.decode ~env:params projection_codec buf 0) in
  Alcotest.(check int) "x" 8 v.x;
  Alcotest.(check int) "outx" 8 (Param.get params outx)

let test_metadata_where_fail () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let buf = Bytes.of_string "\x08" in
  let params =
    Param.empty |> fun env ->
    Param.bind env limit 7 |> fun env -> Param.init env outx 0
  in
  match Codec.decode ~env:params projection_codec buf 0 with
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_codec_metadata_to_struct () =
  let s = C.struct_of_codec projection_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains param" true (contains ~sub:"limit" output);
  Alcotest.(check bool)
    "contains mutable param" true
    (contains ~sub:"mutable" output);
  Alcotest.(check bool) "contains where" true (contains ~sub:"where" output);
  Alcotest.(check bool)
    "contains on-success" true
    (contains ~sub:":on-success" output)

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

(* ── Codec bitfield tests ── *)

type bf32_record = { bf_a : int; bf_b : int; bf_c : int; bf_d : int }

let bf32_codec =
  Codec.view "Bf32Test"
    (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
    Codec.
      [
        Codec.field "a" (bits ~width:3 U32be) (fun t -> t.bf_a);
        Codec.field "b" (bits ~width:5 U32be) (fun t -> t.bf_b);
        Codec.field "c" (bits ~width:16 U32be) (fun t -> t.bf_c);
        Codec.field "d" (bits ~width:8 U32be) (fun t -> t.bf_d);
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
        Codec.field "ver" (bits ~width:3 U16be) (fun t -> t.bf_ver);
        Codec.field "flags" (bits ~width:2 U16be) (fun t -> t.bf_flags);
        Codec.field "id" (bits ~width:11 U16be) (fun t -> t.bf_id);
        Codec.field "count" (bits ~width:14 U16be) (fun t -> t.bf_count);
        Codec.field "len" (bits ~width:2 U16be) (fun t -> t.bf_len);
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
  (* Two U16be groups: (3+2+11=16) + (14+2=16) = 32 bits = 4 bytes *)
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
  let s = C.struct_of_codec bf32_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT32BE" true
    (contains ~sub:"UINT32BE" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b" output)

(* ── Zero-copy view tests ── *)

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
    let f_a = Codec.field "a" (bits ~width:3 U32be) (fun t -> t.bf_a) in
    let f_d = Codec.field "d" (bits ~width:8 U32be) (fun t -> t.bf_d) in
    let codec =
      Codec.view "ViewBf"
        (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
        Codec.
          [
            f_a;
            Codec.field "b" (bits ~width:5 U32be) (fun t -> t.bf_b);
            Codec.field "c" (bits ~width:16 U32be) (fun t -> t.bf_c);
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
    let f_flag = Codec.field "flag" (bool (bits ~width:1 U8)) fst in
    let codec =
      Codec.view "ViewBool"
        (fun flag code -> (flag, code))
        Codec.[ f_flag; Codec.field "code" (bits ~width:7 U8) snd ]
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
    let f_a = Codec.field "a" (bits ~width:3 U32be) (fun t -> t.bf_a) in
    let f_d = Codec.field "d" (bits ~width:8 U32be) (fun t -> t.bf_d) in
    let codec =
      Codec.view "ViewSetBf"
        (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
        Codec.
          [
            f_a;
            Codec.field "b" (bits ~width:5 U32be) (fun t -> t.bf_b);
            Codec.field "c" (bits ~width:16 U32be) (fun t -> t.bf_c);
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
  let r = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "b preserved" 20 r.bf_b;
  Alcotest.(check int) "c preserved" 0x1234 r.bf_c;
  Alcotest.(check int) "d preserved" 0xAB r.bf_d;
  (Staged.unstage (Codec.set codec f_d)) buf 0 0x42;
  Alcotest.(check int)
    "get d after set" 0x42
    ((Staged.unstage (Codec.get codec f_d)) buf 0);
  let r = decode_ok (Codec.decode codec buf 0) in
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
  | Error (Unexpected_eof _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

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
    let f_flag = Codec.field "flag" (bool (bits ~width:1 U8)) fst in
    let codec =
      Codec.view "ViewSetBool"
        (fun flag code -> (flag, code))
        Codec.[ f_flag; Codec.field "code" (bits ~width:7 U8) snd ]
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

(* ── Field sharing tests -- same field spec used in two codecs ── *)

let test_view_shared_field_spec () =
  (* Two codecs with different layouts, each with their own field "x".
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
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
     Codec1: [3-bit a] [5-bit b]            -> a is top 3 bits
     Codec2: [5-bit pad] [3-bit a]           -> a is bottom 3 bits
     Each codec gets a fresh field object. *)
  let f1_a = Codec.field "a" (bits ~width:3 U8) (fun (a, _) -> a) in
  let codec1 =
    Codec.view "ShareBf1"
      (fun a b -> (a, b))
      Codec.[ f1_a; Codec.field "b" (bits ~width:5 U8) (fun (_, b) -> b) ]
  in
  let f2_a = Codec.field "a" (bits ~width:3 U8) (fun (a, _) -> a) in
  let codec2 =
    Codec.view "ShareBf2"
      (fun _pad a -> (a, 0))
      Codec.[ Codec.field "pad" (bits ~width:5 U8) (fun _ -> 0); f2_a ]
  in
  (* 0xE3 = 0b_111_00011
     codec1 reads top 3 bits -> 7
     codec2 reads bottom 3 bits -> 3 *)
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
  let f1 = Codec.field "v" (bits ~width:4 U8) (fun (v, _) -> v) in
  let codec1 =
    Codec.view "SetShare1"
      (fun v pad -> (v, pad))
      Codec.[ f1; Codec.field "pad" (bits ~width:4 U8) (fun (_, p) -> p) ]
  in
  let f2 = Codec.field "v" (bits ~width:4 U8) (fun (v, _) -> v) in
  let codec2 =
    Codec.view "SetShare2"
      (fun pad v -> (v, pad))
      Codec.[ Codec.field "pad" (bits ~width:4 U8) (fun (_, p) -> p); f2 ]
  in
  (* Start: 0x00. Set codec1's field (top nibble) to 0xA *)
  let buf = Bytes.create 1 in
  (Staged.unstage (Codec.set codec1 f1)) buf 0 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* codec2's field is bottom nibble -- should still be 0 *)
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
  let tag, payload = decode_ok (Codec.decode codec buf 0) in
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
  let payload_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec f_payload)) buf 0)
  in
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
  let f_hi = Codec.field "hi" (bits ~width:4 U8) (fun (h, _) -> h) in
  let f_lo = Codec.field "lo" (bits ~width:4 U8) (fun (_, l) -> l) in
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
  let f_hi = Codec.field "hi" (bits ~width:4 U8) (fun (h, _) -> h) in
  let f_lo = Codec.field "lo" (bits ~width:4 U8) (fun (_, l) -> l) in
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
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec f_payload)) buf 0)
  in
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
  let mid_off = Bs.first ((Staged.unstage (Codec.get outer f_body)) buf 0) in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get mid f_mid_payload)) buf mid_off)
  in
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

(* ── FFI stub generation tests ── *)

let test_c_stubs () =
  let s =
    struct_ "SimpleHeader"
      [ field "version" uint8; field "length" uint16; field "flags" uint8 ]
  in
  let stubs = Wire_c.to_c_stubs [ s ] in
  Alcotest.(check bool)
    "contains check stub" true
    (contains ~sub:"caml_wire_simpleheader_check" stubs);
  Alcotest.(check bool)
    "contains error handler" true
    (contains ~sub:"simpleheader_err" stubs)

(* ── Dependent-size byte_slice tests ── *)

type dep_slice_record = { ds_length : int; ds_payload : Bs.t }

let f_ds_length = Codec.field "Length" uint16be (fun r -> r.ds_length)

let f_ds_payload =
  Codec.field "Payload"
    (byte_slice ~size:(Codec.field_ref f_ds_length))
    (fun r -> r.ds_payload)

let dep_slice_codec =
  Codec.view "DepSlice"
    (fun length payload -> { ds_length = length; ds_payload = payload })
    Codec.[ f_ds_length; f_ds_payload ]

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
  let off =
    Bs.first ((Staged.unstage (Codec.get dep_slice_codec f_ds_payload)) buf 0)
  in
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
    (byte_array ~size:(Codec.field_ref f_da_length))
    (fun r -> r.da_payload)

let dep_array_codec =
  Codec.view "DepArray"
    (fun length payload -> { da_length = length; da_payload = payload })
    Codec.[ f_da_length; f_da_payload ]

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
    (Staged.unstage (Codec.get dep_array_codec f_da_payload)) buf 0
  in
  Alcotest.(check string) "get payload" "xyz" payload

(* ── Fixed field after variable field tests ── *)

type trailer_record = { tr_length : int; tr_payload : Bs.t; tr_checksum : int }

let f_tr_length = Codec.field "Length" uint16be (fun r -> r.tr_length)

let f_tr_payload =
  Codec.field "Payload"
    (byte_slice ~size:(Codec.field_ref f_tr_length))
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

(* ── Codec.field_ref expression tests ── *)

let test_dep_codec_ref () =
  (* Codec.field_ref produces a valid expression used as byte_slice size *)
  let f_len = Codec.field "Len" uint8 (fun (l, _) -> l) in
  let f_data =
    Codec.field "Data"
      (byte_slice ~size:(Codec.field_ref f_len))
      (fun (_, d) -> d)
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
  let f_sz = Codec.field "Size" uint8 (fun (s, _) -> s) in
  let f_body =
    Codec.field "Body"
      (byte_slice ~size:(Codec.field_ref f_sz))
      (fun (_, b) -> b)
  in
  let codec =
    Codec.view "RefSizeEval" (fun sz body -> (sz, body)) Codec.[ f_sz; f_body ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_uint8 buf 0 10;
  Alcotest.(check int) "compute size" 11 (Codec.wire_size_at codec buf 0);
  Bytes.set_uint8 buf 0 0;
  Alcotest.(check int) "compute size 0" 1 (Codec.wire_size_at codec buf 0)

(* ── struct_of_codec for variable-size codecs ── *)

let test_dep_to_struct () =
  (* struct_of_codec should produce a valid struct for variable-size codecs *)
  let s = C.struct_of_codec dep_slice_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output);
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output)

let test_dep_trailer_to_struct () =
  let s = C.struct_of_codec trailer_codec in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool)
    "contains Checksum" true
    (contains ~sub:"Checksum" output)

(* ── Suite ── *)

let suite =
  ( "codec",
    [
      (* record *)
      Alcotest.test_case "record: encode" `Quick test_record_encode;
      Alcotest.test_case "record: decode" `Quick test_record_decode;
      Alcotest.test_case "record: roundtrip" `Quick test_record_roundtrip;
      Alcotest.test_case "record: struct_of_codec" `Quick test_record_to_struct;
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
        test_codec_metadata_to_struct;
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
      Alcotest.test_case "dep: wire_size_at" `Quick test_dep_compute_wire_size;
      (* Codec.field_ref expressions *)
      Alcotest.test_case "dep: codec ref" `Quick test_dep_codec_ref;
      Alcotest.test_case "dep: codec ref size eval" `Quick
        test_dep_ref_size_eval;
      (* struct_of_codec for variable-size codecs *)
      Alcotest.test_case "dep: struct_of_codec" `Quick test_dep_to_struct;
      Alcotest.test_case "dep: trailer struct_of_codec" `Quick
        test_dep_trailer_to_struct;
    ] )
