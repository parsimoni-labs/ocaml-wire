(* Tests for Unchecked: each read gives what the checked [Bytes] read gives at
   every offset of a buffer whose bytes have the top bit set. *)

open Wire.Private

let buf = Bytes.of_string "\x01\x82\xf3\x04\xa5\x96"

let check name ~unchecked ~checked width =
  for off = 0 to Bytes.length buf - width do
    Alcotest.(check bool)
      (Fmt.str "%s at %d" name off)
      true
      (unchecked buf off = checked buf off)
  done

let test_u8 () = check "u8" ~unchecked:Unchecked.u8 ~checked:Bytes.get_uint8 1

let test_u16 () =
  check "u16le" ~unchecked:Unchecked.u16le ~checked:Bytes.get_uint16_le 2;
  check "u16be" ~unchecked:Unchecked.u16be ~checked:Bytes.get_uint16_be 2

let test_i32 () =
  check "i32le" ~unchecked:Unchecked.i32le ~checked:Bytes.get_int32_le 4;
  check "i32be" ~unchecked:Unchecked.i32be ~checked:Bytes.get_int32_be 4

let suite =
  ( "unchecked",
    [
      Alcotest.test_case "u8" `Quick test_u8;
      Alcotest.test_case "u16" `Quick test_u16;
      Alcotest.test_case "i32" `Quick test_i32;
    ] )
