(* Tests for UInt32: unsigned 32-bit get/set over bytes. *)

open Wire.Private

let test_roundtrip_le () =
  let buf = Bytes.create 4 in
  let v = 0xDEAD_BEEF in
  UInt32.set_le buf 0 v;
  Alcotest.(check int) "le roundtrip" (v land 0xFFFF_FFFF) (UInt32.get_le buf 0)

let test_roundtrip_be () =
  let buf = Bytes.create 4 in
  let v = 0xCAFE_BABE in
  UInt32.set_be buf 0 v;
  Alcotest.(check int) "be roundtrip" (v land 0xFFFF_FFFF) (UInt32.get_be buf 0)

let test_of_int_masks () =
  Alcotest.(check int) "mask" 0xFF (UInt32.of_int 0xFF);
  Alcotest.(check int) "identity" 42 (UInt32.to_int (UInt32.of_int 42))

let suite =
  ( "uint32",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "of_int masks" `Quick test_of_int_masks;
    ] )
