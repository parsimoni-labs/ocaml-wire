(* Tests for Unchecked: each read gives what the checked [Bytes] read gives at
   every offset of a buffer whose bytes have the top bit set, and each write
   leaves the bytes the checked [Bytes] write leaves. *)

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

let check_set name ~unchecked ~checked width v =
  for off = 0 to Bytes.length buf - width do
    let a = Bytes.copy buf and b = Bytes.copy buf in
    unchecked a off v;
    checked b off v;
    Alcotest.(check string)
      (Fmt.str "%s at %d" name off)
      (Bytes.to_string b) (Bytes.to_string a)
  done

let test_set () =
  check_set "set_u8" ~unchecked:Unchecked.set_u8 ~checked:Bytes.set_uint8 1 0xa7;
  check_set "set_u16le" ~unchecked:Unchecked.set_u16le
    ~checked:Bytes.set_uint16_le 2 0xbeef;
  check_set "set_u16be" ~unchecked:Unchecked.set_u16be
    ~checked:Bytes.set_uint16_be 2 0xbeef;
  check_set "set_i32le" ~unchecked:Unchecked.set_i32le
    ~checked:Bytes.set_int32_le 4 (-559038737l);
  check_set "set_i32be" ~unchecked:Unchecked.set_i32be
    ~checked:Bytes.set_int32_be 4 (-559038737l)

let suite =
  ( "unchecked",
    [
      Alcotest.test_case "u8" `Quick test_u8;
      Alcotest.test_case "u16" `Quick test_u16;
      Alcotest.test_case "i32" `Quick test_i32;
      Alcotest.test_case "writes" `Quick test_set;
    ] )
