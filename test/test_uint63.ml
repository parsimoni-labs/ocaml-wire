(* Tests for UInt63: unsigned 63-bit get/set over bytes (8-byte slot). *)

open Wire.Private

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 8 in
  set buf 0 (UInt63.of_int v);
  Alcotest.(check int) name v (UInt63.to_int (get buf 0))

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:UInt63.set_le ~get:UInt63.le
    0x1234_5678_9ABC

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:UInt63.set_be ~get:UInt63.be
    0x1234_5678_9ABC

let test_of_int_identity () =
  Alcotest.(check int) "identity" 42 (UInt63.to_int (UInt63.of_int 42))

(* Pin the wire byte order across all eight bytes so the read/write paths
   cannot drift. *)
let test_byte_layout () =
  let buf = Bytes.of_string "\x01\x02\x03\x04\x05\x06\x07\x08" in
  Alcotest.(check int)
    "le read" 0x0807_0605_0403_0201
    (UInt63.to_int (UInt63.le buf 0));
  Alcotest.(check int)
    "be read" 0x0102_0304_0506_0708
    (UInt63.to_int (UInt63.be buf 0));
  let out = Bytes.create 8 in
  UInt63.set_le out 0 (UInt63.of_int 0x0807_0605_0403_0201);
  Alcotest.(check string)
    "le write" "\x01\x02\x03\x04\x05\x06\x07\x08" (Bytes.to_string out);
  UInt63.set_be out 0 (UInt63.of_int 0x0102_0304_0506_0708);
  Alcotest.(check string)
    "be write" "\x01\x02\x03\x04\x05\x06\x07\x08" (Bytes.to_string out)

let test_boundaries () =
  List.iter
    (fun v ->
      check_roundtrip "le" ~set:UInt63.set_le ~get:UInt63.le v;
      check_roundtrip "be" ~set:UInt63.set_be ~get:UInt63.be v)
    [ 0x0; 0x1; 0xFFFF; 0x1_0000_0000; 0x0102_0304_0506_0708; max_int ]

let suite =
  ( "uint63",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "of_int identity" `Quick test_of_int_identity;
      Alcotest.test_case "byte layout" `Quick test_byte_layout;
      Alcotest.test_case "boundaries" `Quick test_boundaries;
    ] )
