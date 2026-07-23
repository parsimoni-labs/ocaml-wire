(* Tests for UInt63: unsigned 63-bit get/set over bytes (8-byte slot).
   Values above 31 bits are written as [Int64] literals and compared through
   [Optint.Int63.to_int64]: an int literal would itself truncate on a
   narrow-int target (wasm_of_ocaml, js_of_ocaml), and these tests run there
   too. *)

open Wire.Private

let of_int64 = Optint.Int63.of_int64
let to_int64 = Optint.Int63.to_int64

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 8 in
  set buf 0 (of_int64 v);
  Alcotest.(check int64) name v (to_int64 (get buf 0))

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:UInt63.set_le ~get:UInt63.le
    0x1234_5678_9ABCL

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:UInt63.set_be ~get:UInt63.be
    0x1234_5678_9ABCL

let test_of_int_identity () =
  Alcotest.(check int) "identity" 42 (UInt63.to_int (UInt63.of_int 42))

(* Pin the wire byte order across all eight bytes so the read/write paths
   cannot drift. *)
let test_byte_layout () =
  let buf = Bytes.of_string "\x01\x02\x03\x04\x05\x06\x07\x08" in
  Alcotest.(check int64)
    "le read" 0x0807_0605_0403_0201L
    (to_int64 (UInt63.le buf 0));
  Alcotest.(check int64)
    "be read" 0x0102_0304_0506_0708L
    (to_int64 (UInt63.be buf 0));
  let out = Bytes.create 8 in
  UInt63.set_le out 0 (of_int64 0x0807_0605_0403_0201L);
  Alcotest.(check string)
    "le write" "\x01\x02\x03\x04\x05\x06\x07\x08" (Bytes.to_string out);
  UInt63.set_be out 0 (of_int64 0x0102_0304_0506_0708L);
  Alcotest.(check string)
    "be write" "\x01\x02\x03\x04\x05\x06\x07\x08" (Bytes.to_string out)

let test_boundaries () =
  List.iter
    (fun v ->
      check_roundtrip "le" ~set:UInt63.set_le ~get:UInt63.le v;
      check_roundtrip "be" ~set:UInt63.set_be ~get:UInt63.be v)
    [
      0x0L;
      0x1L;
      0xFFFFL;
      0x1_0000_0000L;
      0x0102_0304_0506_0708L;
      0x3FFF_FFFF_FFFF_FFFFL (* 63-bit max *);
    ]

let suite =
  ( "uint63",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "of_int identity" `Quick test_of_int_identity;
      Alcotest.test_case "byte layout" `Quick test_byte_layout;
      Alcotest.test_case "boundaries" `Quick test_boundaries;
    ] )
