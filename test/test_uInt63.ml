(* Tests for UInt63: unsigned 63-bit get/set over bytes (8-byte slot). *)

open Wire.Private

let test_roundtrip_le () =
  let buf = Bytes.create 8 in
  let v = 0x1234_5678_9ABC in
  UInt63.set_le buf 0 v;
  Alcotest.(check int) "le roundtrip" v (UInt63.get_le buf 0)

let test_roundtrip_be () =
  let buf = Bytes.create 8 in
  let v = 0x1234_5678_9ABC in
  UInt63.set_be buf 0 v;
  Alcotest.(check int) "be roundtrip" v (UInt63.get_be buf 0)

let test_of_int_identity () =
  Alcotest.(check int) "identity" 42 (UInt63.to_int (UInt63.of_int 42))

let suite =
  ( "uint63",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "of_int identity" `Quick test_of_int_identity;
    ] )
