(* Tests for UInt63: unsigned 63-bit get/set over bytes (8-byte slot). *)

open Wire.Private

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 8 in
  set buf 0 v;
  Alcotest.(check int) name v (get buf 0)

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:UInt63.set_le ~get:UInt63.le
    0x1234_5678_9ABC

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:UInt63.set_be ~get:UInt63.be
    0x1234_5678_9ABC

let test_of_int_identity () =
  Alcotest.(check int) "identity" 42 (UInt63.to_int (UInt63.of_int 42))

let suite =
  ( "uint63",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "of_int identity" `Quick test_of_int_identity;
    ] )
