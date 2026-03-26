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

let test_is_lsb_first () =
  Alcotest.(check bool) "u8 is lsb" true (Bitfield.is_lsb_first bf_u8);
  Alcotest.(check bool) "u16le is lsb" true (Bitfield.is_lsb_first bf_u16_le);
  Alcotest.(check bool) "u16be is msb" false (Bitfield.is_lsb_first bf_u16_be);
  Alcotest.(check bool) "u32le is lsb" true (Bitfield.is_lsb_first bf_u32_le);
  Alcotest.(check bool) "u32be is msb" false (Bitfield.is_lsb_first bf_u32_be)

let test_extract_lsb_first () =
  (* LSBFirst: first declared field at bit 0 *)
  (* Word 0b11010110 = 0xD6, extract 4 bits at offset 0 -> 0b0110 = 6 *)
  let word = 0xD6 in
  Alcotest.(check int)
    "lsb bits 0..3" 6
    (Bitfield.extract ~base:bf_u8 ~total:8 ~bits_used:0 ~width:4 word);
  (* Extract 4 bits at offset 4 -> 0b1101 = 13 *)
  Alcotest.(check int)
    "lsb bits 4..7" 13
    (Bitfield.extract ~base:bf_u8 ~total:8 ~bits_used:4 ~width:4 word)

let test_extract_msb_first () =
  (* MSBFirst: first declared field at MSB *)
  (* Word 0xD600 = 0b1101011000000000 in 16-bit BE context *)
  let word = 0xD600 in
  (* Extract 4 bits at offset 0 (MSB side) -> 0b1101 = 13 *)
  Alcotest.(check int)
    "msb bits 0..3" 13
    (Bitfield.extract ~base:bf_u16_be ~total:16 ~bits_used:0 ~width:4 word);
  (* Extract 4 bits at offset 4 -> 0b0110 = 6 *)
  Alcotest.(check int)
    "msb bits 4..7" 6
    (Bitfield.extract ~base:bf_u16_be ~total:16 ~bits_used:4 ~width:4 word)

let suite =
  ( "bitfield",
    [
      Alcotest.test_case "byte_size" `Quick test_byte_size;
      Alcotest.test_case "total_bits" `Quick test_total_bits;
      Alcotest.test_case "equal" `Quick test_equal;
      Alcotest.test_case "read_word/write_word roundtrip" `Quick
        test_read_write_roundtrip;
      Alcotest.test_case "is_lsb_first" `Quick test_is_lsb_first;
      Alcotest.test_case "extract LSBFirst" `Quick test_extract_lsb_first;
      Alcotest.test_case "extract MSBFirst" `Quick test_extract_msb_first;
    ] )
