module Bitfield = Wire__Bitfield
module Types = Wire__Types

let bf_u8 = Types.BF_U8
let bf_u16_le = Types.BF_U16 Little
let bf_u16_be = Types.BF_U16 Big
let bf_u32_le = Types.BF_U32 Little
let bf_u32_be = Types.BF_U32 Big

let test_byte_size () =
  Alcotest.(check int) "BF_U8" 1 (Bitfield.byte_size bf_u8);
  Alcotest.(check int) "BF_U16 LE" 2 (Bitfield.byte_size bf_u16_le);
  Alcotest.(check int) "BF_U16 BE" 2 (Bitfield.byte_size bf_u16_be);
  Alcotest.(check int) "BF_U32 LE" 4 (Bitfield.byte_size bf_u32_le);
  Alcotest.(check int) "BF_U32 BE" 4 (Bitfield.byte_size bf_u32_be)

let test_total_bits () =
  Alcotest.(check int) "BF_U8" 8 (Bitfield.total_bits bf_u8);
  Alcotest.(check int) "BF_U16 LE" 16 (Bitfield.total_bits bf_u16_le);
  Alcotest.(check int) "BF_U16 BE" 16 (Bitfield.total_bits bf_u16_be);
  Alcotest.(check int) "BF_U32 LE" 32 (Bitfield.total_bits bf_u32_le);
  Alcotest.(check int) "BF_U32 BE" 32 (Bitfield.total_bits bf_u32_be)

let test_equal () =
  Alcotest.(check bool) "same" true (Bitfield.equal bf_u8 bf_u8);
  Alcotest.(check bool) "same endian" true (Bitfield.equal bf_u16_le bf_u16_le);
  Alcotest.(check bool) "diff endian" false (Bitfield.equal bf_u16_le bf_u16_be);
  Alcotest.(check bool) "diff size" false (Bitfield.equal bf_u8 bf_u16_le);
  Alcotest.(check bool) "u32 same" true (Bitfield.equal bf_u32_be bf_u32_be);
  Alcotest.(check bool) "u32 diff" false (Bitfield.equal bf_u32_le bf_u32_be)

let test_read_write_roundtrip () =
  let buf = Bytes.create 4 in
  (* U8 roundtrip *)
  Bitfield.write_word bf_u8 buf 0 0xAB;
  Alcotest.(check int) "u8 roundtrip" 0xAB (Bitfield.read_word bf_u8 buf 0);
  (* U16 LE roundtrip *)
  Bitfield.write_word bf_u16_le buf 0 0x1234;
  Alcotest.(check int)
    "u16le roundtrip" 0x1234
    (Bitfield.read_word bf_u16_le buf 0);
  (* U16 BE roundtrip *)
  Bitfield.write_word bf_u16_be buf 0 0x5678;
  Alcotest.(check int)
    "u16be roundtrip" 0x5678
    (Bitfield.read_word bf_u16_be buf 0);
  (* U32 LE roundtrip *)
  Bitfield.write_word bf_u32_le buf 0 0x12345678;
  Alcotest.(check int)
    "u32le roundtrip" 0x12345678
    (Bitfield.read_word bf_u32_le buf 0);
  (* U32 BE roundtrip *)
  Bitfield.write_word bf_u32_be buf 0 0x12345678;
  Alcotest.(check int)
    "u32be roundtrip" 0x12345678
    (Bitfield.read_word bf_u32_be buf 0)

let test_native_bit_order () =
  Alcotest.(check bool)
    "u8 native lsb" true
    (Bitfield.native_bit_order bf_u8 = Types.Lsb_first);
  Alcotest.(check bool)
    "u16le native lsb" true
    (Bitfield.native_bit_order bf_u16_le = Types.Lsb_first);
  Alcotest.(check bool)
    "u16be native msb" true
    (Bitfield.native_bit_order bf_u16_be = Types.Msb_first);
  Alcotest.(check bool)
    "u32le native lsb" true
    (Bitfield.native_bit_order bf_u32_le = Types.Lsb_first);
  Alcotest.(check bool)
    "u32be native msb" true
    (Bitfield.native_bit_order bf_u32_be = Types.Msb_first)

let check_extract label ~expected ~bit_order ~total ~bits_used ~width word =
  Alcotest.(check int)
    label expected
    (Bitfield.extract ~bit_order ~total ~bits_used ~width word)

let test_extract_lsb_first () =
  let word = 0xD6 in
  check_extract "lsb bits 0..3" ~expected:6 ~bit_order:Types.Lsb_first ~total:8
    ~bits_used:0 ~width:4 word;
  check_extract "lsb bits 4..7" ~expected:13 ~bit_order:Types.Lsb_first ~total:8
    ~bits_used:4 ~width:4 word

let test_extract_msb_first () =
  let word = 0xD600 in
  check_extract "msb bits 0..3" ~expected:13 ~bit_order:Types.Msb_first
    ~total:16 ~bits_used:0 ~width:4 word;
  check_extract "msb bits 4..7" ~expected:6 ~bit_order:Types.Msb_first ~total:16
    ~bits_used:4 ~width:4 word

(* Independence: bit_order is orthogonal to the base's byte order. *)
let test_extract_bit_order_independent () =
  let word = 0xD6 in
  check_extract "lsb-first on u16be base" ~expected:6 ~bit_order:Types.Lsb_first
    ~total:16 ~bits_used:0 ~width:4 word;
  check_extract "msb-first on u8 base" ~expected:13 ~bit_order:Types.Msb_first
    ~total:8 ~bits_used:0 ~width:4 word

let suite =
  ( "bitfield",
    [
      Alcotest.test_case "byte_size" `Quick test_byte_size;
      Alcotest.test_case "total_bits" `Quick test_total_bits;
      Alcotest.test_case "equal" `Quick test_equal;
      Alcotest.test_case "read_word/write_word roundtrip" `Quick
        test_read_write_roundtrip;
      Alcotest.test_case "native_bit_order" `Quick test_native_bit_order;
      Alcotest.test_case "extract LSB-first" `Quick test_extract_lsb_first;
      Alcotest.test_case "extract MSB-first" `Quick test_extract_msb_first;
      Alcotest.test_case "bit_order independent of base endian" `Quick
        test_extract_bit_order_independent;
    ] )
