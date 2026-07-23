(* Tests for UInt32: unsigned 32-bit get/set over bytes. Values with bit 31
   set are written as [Int32] literals and compared through [to_int32]: an
   int literal would itself truncate on a narrow-int target (wasm_of_ocaml,
   js_of_ocaml), and these tests run there too. *)

open Wire.Private

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 4 in
  set buf 0 (UInt32.of_int32 v);
  Alcotest.(check int32) name v (UInt32.to_int32 (get buf 0))

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:UInt32.set_le ~get:UInt32.le 0xDEAD_BEEFl

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:UInt32.set_be ~get:UInt32.be 0xCAFE_BABEl

let test_of_int_masks () =
  Alcotest.(check int) "mask" 0xFF (UInt32.to_int (UInt32.of_int 0xFF));
  Alcotest.(check int) "identity" 42 (UInt32.to_int (UInt32.of_int 42));
  (* The mask keeps the low 32 bits on every int width: [-1] is the u32
     all-ones there, and the no-op mask where the int is narrower. *)
  Alcotest.(check int32)
    "of_int (-1) is all-ones" (-1l)
    (UInt32.to_int32 (UInt32.of_int (-1)));
  Alcotest.(check int64)
    "mask32 low 32 bits" 0xFFFF_FFFFL
    (Int64.logand (Int64.of_int UInt32.mask32) 0xFFFF_FFFFL)

(* Pin the wire byte order so the read/write paths cannot drift. *)
let test_byte_layout () =
  let buf = Bytes.of_string "\x01\x02\x03\x04" in
  Alcotest.(check int) "le read" 0x04030201 (UInt32.to_int (UInt32.le buf 0));
  Alcotest.(check int) "be read" 0x01020304 (UInt32.to_int (UInt32.be buf 0));
  let out = Bytes.create 4 in
  UInt32.set_le out 0 (UInt32.of_int 0x04030201);
  Alcotest.(check string) "le write" "\x01\x02\x03\x04" (Bytes.to_string out);
  UInt32.set_be out 0 (UInt32.of_int 0x01020304);
  Alcotest.(check string) "be write" "\x01\x02\x03\x04" (Bytes.to_string out)

(* A value with bit 31 set keeps it: its int32 bit pattern survives even where a
   native int could not hold it. This is the TCP sequence number that regressed. *)
let test_high_bit () =
  let buf = Bytes.create 4 in
  UInt32.set_be buf 0 (UInt32.of_int32 0xA695_853Bl);
  Alcotest.(check int32)
    "seq bit 31" 0xA695_853Bl
    (UInt32.to_int32 (UInt32.be buf 0))

let test_boundaries () =
  List.iter
    (fun v ->
      check_roundtrip "le" ~set:UInt32.set_le ~get:UInt32.le v;
      check_roundtrip "be" ~set:UInt32.set_be ~get:UInt32.be v)
    [ 0x0l; 0x1l; 0xFFFFl; 0x1_0000l; 0x7FFF_FFFFl; 0x8000_0000l; 0xFFFF_FFFFl ]

let suite =
  ( "uint32",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "of_int masks" `Quick test_of_int_masks;
      Alcotest.test_case "byte layout" `Quick test_byte_layout;
      Alcotest.test_case "high bit preserved" `Quick test_high_bit;
      Alcotest.test_case "boundaries" `Quick test_boundaries;
    ] )
