(* Tests for UInt32: unsigned 32-bit get/set over bytes. *)

open Wire.Private

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 4 in
  set buf 0 v;
  Alcotest.(check int) name (v land 0xFFFF_FFFF) (get buf 0)

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:UInt32.set_le ~get:UInt32.le 0xDEAD_BEEF

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:UInt32.set_be ~get:UInt32.be 0xCAFE_BABE

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
