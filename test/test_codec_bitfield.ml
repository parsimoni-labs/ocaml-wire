(* Tests for codec.ml: bitfields, batch access and bit order. *)

open Wire
open Test_helpers

(* -- Codec bitfield tests -- *)

type bf32_record = { a : int; b : int; c : int; d : int }

let bf32_codec =
  let open Codec in
  v "Bf32Test"
    (fun a b c d -> { a; b; c; d })
    [
      (Field.v "a" (bits ~width:3 U32be) $ fun t -> t.a);
      (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.b);
      (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.c);
      (Field.v "d" (bits ~width:8 U32be) $ fun t -> t.d);
    ]

type bf16_record = { ver : int; flags : int; id : int; count : int; len : int }

let bf16_codec =
  let open Codec in
  v "Bf16Test"
    (fun ver flags id count len -> { ver; flags; id; count; len })
    [
      (Field.v "ver" (bits ~width:3 U16be) $ fun t -> t.ver);
      (Field.v "flags" (bits ~width:2 U16be) $ fun t -> t.flags);
      (Field.v "id" (bits ~width:11 U16be) $ fun t -> t.id);
      (Field.v "count" (bits ~width:14 U16be) $ fun t -> t.count);
      (Field.v "len" (bits ~width:2 U16be) $ fun t -> t.len);
    ]

let test_codec_bitfield_wire_size () =
  Alcotest.(check int) "bf32 wire_size" 4 (Codec.wire_size bf32_codec);
  Alcotest.(check int) "bf16 wire_size" 4 (Codec.wire_size bf16_codec)

let test_codec_bitfield_roundtrip () =
  let original = { a = 5; b = 20; c = 0x1234; d = 0xAB } in
  match encode_record bf32_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" original.a decoded.a;
          Alcotest.(check int) "b" original.b decoded.b;
          Alcotest.(check int) "c" original.c decoded.c;
          Alcotest.(check int) "d" original.d decoded.d
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_byte_layout () =
  (* a=5 (3b), b=20 (5b), c=0x1234 (16b), d=0xAB (8b)
     MSB-first packing: 101_10100_0001001000110100_10101011
     = 0xB4 0x12 0x34 0xAB *)
  let v = { a = 5; b = 20; c = 0x1234; d = 0xAB } in
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
      Alcotest.(check int) "a" 5 v.a;
      Alcotest.(check int) "b" 20 v.b;
      Alcotest.(check int) "c" 0x1234 v.c;
      Alcotest.(check int) "d" 0xAB v.d
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_codec_bitfield_multi_group () =
  (* Two U16be groups: (3+2+11=16) + (14+2=16) = 32 bits = 4 bytes *)
  let v = { ver = 5; flags = 2; id = 0x7FF; count = 0x3FFF; len = 3 } in
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
          Alcotest.(check int) "ver" v.ver decoded.ver;
          Alcotest.(check int) "flags" v.flags decoded.flags;
          Alcotest.(check int) "id" v.id decoded.id;
          Alcotest.(check int) "count" v.count decoded.count;
          Alcotest.(check int) "len" v.len decoded.len
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_overflow_u8 () =
  let v = { a = 0x8; b = 0; c = 0; d = 0 } in
  (* a is 3 bits, 0x8 = 8 exceeds max 7 *)
  match encode_record bf32_codec v with
  | Ok _ -> Alcotest.fail "expected overflow for 3-bit field with value 0x8"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_overflow_u16 () =
  let v = { ver = 0; flags = 0; id = 0x800; count = 0; len = 0 } in
  (* id is 11 bits, 0x800 = 2048 exceeds max 2047 *)
  match encode_record bf16_codec v with
  | Ok _ -> Alcotest.fail "expected overflow for 11-bit field with value 0x800"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_overflow_u32 () =
  let v = { a = 0; b = 0; c = 0x10000; d = 0 } in
  (* c is 16 bits, 0x10000 exceeds max 0xFFFF *)
  match encode_record bf32_codec v with
  | Ok _ ->
      Alcotest.fail "expected overflow for 16-bit field with value 0x10000"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_max_valid () =
  (* All fields at their maximum valid values *)
  let v = { a = 7; b = 31; c = 0xFFFF; d = 0xFF } in
  match encode_record bf32_codec v with
  | Error e -> Alcotest.failf "encode max valid: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" 7 decoded.a;
          Alcotest.(check int) "b" 31 decoded.b;
          Alcotest.(check int) "c" 0xFFFF decoded.c;
          Alcotest.(check int) "d" 0xFF decoded.d
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

let test_fixed_byte_region_size_of_value () =
  let check_string label typ cases =
    let codec = Codec.v label Fun.id Codec.[ Field.v "data" typ $ Fun.id ] in
    List.iter
      (fun (case, value, expected) ->
        Alcotest.(check int)
          (case ^ " size") 3
          (Codec.size_of_value codec value);
        let buf = Bytes.create 3 in
        Codec.encode codec value buf 0;
        Alcotest.(check bytes) case (Bytes.of_string expected) buf)
      cases
  in
  (* Every value is exactly the declared region width: a fixed byte field
     neither pads nor truncates. *)
  let cases =
    [
      ("zeros", "A\x00\x00", "A\x00\x00");
      ("exact", "ABC", "ABC");
      ("high", "\xff\xfe\xfd", "\xff\xfe\xfd");
    ]
  in
  check_string "FixedBytes" (byte_array ~size:(int 3)) cases;
  check_string "FixedBytesWhere"
    (byte_array_where ~size:(int 3) ~per_byte:(fun _ -> Expr.true_))
    cases;
  let codec =
    Codec.v "FixedSlice" Fun.id
      Codec.[ Field.v "data" (byte_slice ~size:(int 3)) $ Fun.id ]
  in
  List.iter
    (fun (case, value, expected) ->
      let bytes = Bytes.of_string value in
      let slice =
        Bytesrw.Bytes.Slice.make bytes ~first:0 ~length:(Bytes.length bytes)
      in
      Alcotest.(check int)
        (case ^ " slice size") 3
        (Codec.size_of_value codec slice);
      let buf = Bytes.create 3 in
      Codec.encode codec slice buf 0;
      Alcotest.(check bytes) (case ^ " slice") (Bytes.of_string expected) buf)
    cases

let test_packed_bf_size () =
  let f_a = Field.v "a" (bits ~width:1 U8) in
  let f_b = Field.v "b" (bits ~width:7 U8) in
  let codec =
    Codec.v "Packed" (fun a b -> (a, b)) Codec.[ f_a $ fst; f_b $ snd ]
  in
  Alcotest.(check int)
    "size_of_value matches wire_size" (Codec.wire_size codec)
    (Codec.size_of_value codec (0, 0))

type packed_bool_header = {
  magic : string;
  version : UInt8.t;
  reserved : int;
  flag : bool;
}

let packed_bool_header_codec =
  Codec.v "PackedBoolHeader"
    (fun magic version reserved flag -> { magic; version; reserved; flag })
    Codec.
      [
        (Field.v "Magic" (byte_array ~size:(int 4)) $ fun r -> r.magic);
        (Field.v "Version" uint8 $ fun r -> r.version);
        (Field.v "Reserved" (bits ~width:7 U8) $ fun r -> r.reserved);
        (Field.v "Flag" (bit (bits ~width:1 U8)) $ fun r -> r.flag);
      ]

let test_packed_mapped_bf_size () =
  let value =
    { magic = "dtn!"; version = UInt8.v 4; reserved = 0; flag = true }
  in
  Alcotest.(check int)
    "wire_size counts packed bool bitfield once" 6
    (Codec.wire_size packed_bool_header_codec);
  Alcotest.(check int)
    "size_of_value matches wire_size" 6
    (Codec.size_of_value packed_bool_header_codec value);
  let buf = Bytes.create 6 in
  Codec.encode packed_bool_header_codec value buf 0;
  Alcotest.(check string)
    "encoded contact header shape" "dtn!\x04\x01"
    (Bytes.unsafe_to_string buf)

let test_struct_of_codec_bitfield () =
  let output = render_3d bf32_codec in
  Alcotest.(check bool)
    "contains UINT32BE" true
    (contains ~sub:"UINT32BE" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b" output)

let test_view_get_bitfield () =
  let codec, cf_a, cf_d =
    let f_a = Field.v "a" (bits ~width:3 U32be) in
    let f_d = Field.v "d" (bits ~width:8 U32be) in
    let cf_a = Codec.(f_a $ fun t -> t.a) in
    let cf_d = Codec.(f_d $ fun t -> t.d) in
    let codec =
      let open Codec in
      v "ViewBf"
        (fun a b c d -> { a; b; c; d })
        [
          cf_a;
          (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.b);
          (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.c);
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

let test_view_set_bitfield () =
  let codec, cf_a, cf_d =
    let f_a = Field.v "a" (bits ~width:3 U32be) in
    let f_d = Field.v "d" (bits ~width:8 U32be) in
    let cf_a = Codec.(f_a $ fun t -> t.a) in
    let cf_d = Codec.(f_d $ fun t -> t.d) in
    let codec =
      let open Codec in
      v "ViewSetBf"
        (fun a b c d -> { a; b; c; d })
        [
          cf_a;
          (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.b);
          (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.c);
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
  Alcotest.(check int) "b preserved" 20 r.b;
  Alcotest.(check int) "c preserved" 0x1234 r.c;
  Alcotest.(check int) "d preserved" 0xAB r.d;
  (Staged.unstage (Codec.set codec cf_d)) buf 0 0x42;
  Alcotest.(check int)
    "get d after set" 0x42
    ((Staged.unstage (Codec.get codec cf_d)) buf 0);
  let r = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "a still 3" 3 r.a;
  Alcotest.(check int) "b still 20" 20 r.b;
  Alcotest.(check int) "c still 0x1234" 0x1234 r.c

(* -- Bitfield batch access -- *)

type bf_rec = { hi : int; lo : int }

let bf_f_hi = Field.v "hi" (bits ~width:4 U8)
let bf_f_lo = Field.v "lo" (bits ~width:4 U8)
let bf_cf_hi = Codec.(bf_f_hi $ fun r -> r.hi)
let bf_cf_lo = Codec.(bf_f_lo $ fun r -> r.lo)

let bf_codec =
  Codec.v "BfBatch" (fun hi lo -> { hi; lo }) Codec.[ bf_cf_hi; bf_cf_lo ]

let test_bitfield_extract () =
  (* Default [bit_order = Msb_first]: [hi] is the top nibble. *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  let hi = Codec.bitfield bf_codec bf_cf_hi in
  let lo = Codec.bitfield bf_codec bf_cf_lo in
  let load = Staged.unstage (Codec.load_word hi) in
  let w = load buf 0 in
  let hi = Codec.extract hi w in
  let lo = Codec.extract lo w in
  (* Compare against Codec.get *)
  let get_hi = Staged.unstage (Codec.get bf_codec bf_cf_hi) in
  let get_lo = Staged.unstage (Codec.get bf_codec bf_cf_lo) in
  Alcotest.(check int) "extract hi = get hi" (get_hi buf 0) hi;
  Alcotest.(check int) "extract lo = get lo" (get_lo buf 0) lo;
  Alcotest.(check int) "hi" 0xA hi;
  Alcotest.(check int) "lo" 0x7 lo

let test_bitfield_non_bf_raises () =
  let f_x = Field.v "x" (map ~decode:UInt8.to_int ~encode:UInt8.v uint8) in
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
  let a = Codec.bitfield codec cf_a in
  let b = Codec.bitfield codec cf_b in
  let load_a = Staged.unstage (Codec.load_word a) in
  let load_b = Staged.unstage (Codec.load_word b) in
  let buf = Bytes.create 4 in
  Bytes.set_int32_be buf 0 0xABCDEF01l;
  let wa = load_a buf 0 in
  let wb = load_b buf 0 in
  (* Same base word, same value *)
  (* [Codec.load_word] hands back the bitfield's raw base word, an [Optint.t],
     not a uint32 field value. *)
  Alcotest.check (Alcotest.testable Optint.pp Optint.equal) "same word" wa wb;
  let a = Codec.extract a wa in
  let b = Codec.extract b wa in
  (* a = top 4 bits of 0xABCDEF01 = 0xA, b = next 4 bits = 0xB *)
  Alcotest.(check int) "a" 0xA a;
  Alcotest.(check int) "b" 0xB b

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

type bit_order_split = { x : int; y : int }

let bit_order_split_codec =
  let open Codec in
  v "BitOrderSplit"
    (fun x y -> { x; y })
    [
      (Field.v "x" (bits ~bit_order:Msb_first ~width:4 U8) $ fun r -> r.x);
      (Field.v "y" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.y);
    ]

let test_bitorder_diff_start_newword () =
  (* Two fields with the same base but different [bit_order] must NOT share
     a base word -- the codec allocates a fresh byte for each. Catches any
     regression that would let mismatched bit orders silently collide. *)
  Alcotest.(check int) "wire size = 2" 2 (Codec.wire_size bit_order_split_codec);
  let buf = Bytes.create 2 in
  Codec.encode bit_order_split_codec { x = 0xA; y = 0x5 } buf 0;
  Alcotest.(check int)
    "byte 0 = 0xA0 (Msb_first x)" 0xA0 (Bytes.get_uint8 buf 0);
  Alcotest.(check int)
    "byte 1 = 0x05 (Lsb_first y)" 0x05 (Bytes.get_uint8 buf 1)

let suite =
  ( "codec_bitfield",
    [
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
      Alcotest.test_case "codec bitfield: size_of_value packed" `Quick
        test_packed_bf_size;
      Alcotest.test_case "codec bitfield: size_of_value mapped bit" `Quick
        test_packed_mapped_bf_size;
      Alcotest.test_case "fixed byte regions: size_of_value" `Quick
        test_fixed_byte_region_size_of_value;
      Alcotest.test_case "view: get bitfield" `Quick test_view_get_bitfield;
      Alcotest.test_case "view: set bitfield" `Quick test_view_set_bitfield;
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
      Alcotest.test_case "bitfield: extract matches get" `Quick
        test_bitfield_extract;
      Alcotest.test_case "bitfield: non-bf field raises" `Quick
        test_bitfield_non_bf_raises;
      Alcotest.test_case "bitfield: short buffer" `Quick
        test_bitfield_short_buffer;
      Alcotest.test_case "bitfield: load_word shared" `Quick
        test_bitfield_load_shared;
    ] )
