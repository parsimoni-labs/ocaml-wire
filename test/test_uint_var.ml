module Uint_var = Wire__Uint_var
module Types = Wire__Types

let roundtrip ~endian ~size value () =
  let buf = Bytes.create size in
  Uint_var.write endian buf 0 size value;
  let got = Uint_var.read endian buf 0 size in
  Alcotest.(check int) "roundtrip" value got

let test_1_byte_be () = roundtrip ~endian:Big ~size:1 0xAB ()
let test_1_byte_le () = roundtrip ~endian:Little ~size:1 0xAB ()
let test_2_byte_be () = roundtrip ~endian:Big ~size:2 0x1234 ()
let test_2_byte_le () = roundtrip ~endian:Little ~size:2 0x1234 ()
let test_3_byte_be () = roundtrip ~endian:Big ~size:3 0x1A2B3C ()
let test_3_byte_le () = roundtrip ~endian:Little ~size:3 0x1A2B3C ()
let test_7_byte_be () = roundtrip ~endian:Big ~size:7 0x01_02_03_04_05_06_07 ()

let test_7_byte_le () =
  roundtrip ~endian:Little ~size:7 0x01_02_03_04_05_06_07 ()

let test_byte_order () =
  let buf = Bytes.create 3 in
  Uint_var.write Big buf 0 3 0x1A2B3C;
  Alcotest.(check int) "BE byte 0" 0x1A (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "BE byte 1" 0x2B (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "BE byte 2" 0x3C (Bytes.get_uint8 buf 2);
  let buf = Bytes.create 3 in
  Uint_var.write Little buf 0 3 0x1A2B3C;
  Alcotest.(check int) "LE byte 0" 0x3C (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "LE byte 1" 0x2B (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "LE byte 2" 0x1A (Bytes.get_uint8 buf 2)

let test_zero () =
  let buf = Bytes.create 4 in
  Uint_var.write Big buf 0 4 0;
  Alcotest.(check int) "zero" 0 (Uint_var.read Big buf 0 4)

let suite =
  ( "uint_var",
    [
      Alcotest.test_case "1-byte BE" `Quick test_1_byte_be;
      Alcotest.test_case "1-byte LE" `Quick test_1_byte_le;
      Alcotest.test_case "2-byte BE" `Quick test_2_byte_be;
      Alcotest.test_case "2-byte LE" `Quick test_2_byte_le;
      Alcotest.test_case "3-byte BE" `Quick test_3_byte_be;
      Alcotest.test_case "3-byte LE" `Quick test_3_byte_le;
      Alcotest.test_case "7-byte BE" `Quick test_7_byte_be;
      Alcotest.test_case "7-byte LE" `Quick test_7_byte_le;
      Alcotest.test_case "byte order" `Quick test_byte_order;
      Alcotest.test_case "zero" `Quick test_zero;
    ] )
