(* Tests for UInt64: unsigned 64-bit get/set over bytes. Values above 2^63 - 1
   are written as [Int64] literals and compared through [to_int64]: they have no
   [int] literal on any target, and the carrier reads them as negative. *)

open Wire

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 8 in
  set buf 0 (UInt64.of_int64 v);
  Alcotest.(check int64) name v (UInt64.to_int64 (get buf 0))

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:UInt64.set_le ~get:UInt64.le
    0x0123_4567_89AB_CDEFL

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:UInt64.set_be ~get:UInt64.be
    0xFEDC_BA98_7654_3210L

(* Pin the wire byte order so the read and write paths cannot drift apart. *)
let test_byte_layout () =
  let buf = Bytes.of_string "\x01\x02\x03\x04\x05\x06\x07\x08" in
  Alcotest.(check int64)
    "le read" 0x0807_0605_0403_0201L
    (UInt64.to_int64 (UInt64.le buf 0));
  Alcotest.(check int64)
    "be read" 0x0102_0304_0506_0708L
    (UInt64.to_int64 (UInt64.be buf 0));
  let out = Bytes.create 8 in
  UInt64.set_le out 0 (UInt64.of_int64 0x0807_0605_0403_0201L);
  Alcotest.(check string)
    "le write" "\x01\x02\x03\x04\x05\x06\x07\x08" (Bytes.to_string out);
  UInt64.set_be out 0 (UInt64.of_int64 0x0102_0304_0506_0708L);
  Alcotest.(check string)
    "be write" "\x01\x02\x03\x04\x05\x06\x07\x08" (Bytes.to_string out)

let test_boundaries () =
  List.iter
    (fun v ->
      check_roundtrip "le" ~set:UInt64.set_le ~get:UInt64.le v;
      check_roundtrip "be" ~set:UInt64.set_be ~get:UInt64.be v)
    Int64.[ zero; one; 0xFFFFL; max_int; min_int; minus_one; add min_int one ]

(* Every bit pattern is a legal value here, so [of_int64] is total and there is
   no range to refuse. The ends are the two the carrier reads as negative. *)
let test_bounds () =
  Alcotest.(check int64) "zero" 0L (UInt64.to_int64 UInt64.zero);
  Alcotest.(check int64)
    "max_int is all ones" (-1L)
    (UInt64.to_int64 UInt64.max_int)

(* [of_int] refuses a negative number instead of reinterpreting it as the large
   value its two's complement spells; [of_int64] is where a bit pattern is
   reinterpreted on purpose. *)
let test_of_int_refuses_negative () =
  List.iter
    (fun n ->
      match UInt64.of_int n with
      | v -> Alcotest.failf "of_int %d built %a" n UInt64.pp v
      | exception Invalid_argument _ -> ())
    [ -1; min_int ];
  List.iter
    (fun n ->
      Fmt.kstr
        (fun msg -> Alcotest.(check int) msg n)
        "of_int %d round-trips" n
        (UInt64.to_int (UInt64.of_int n)))
    [ 0; 1; 0xFFFF; max_int ]

(* [to_int_opt] answers whether the native int held the value, so a caller that
   must not silently drop bits has something total to ask. *)
let test_to_int_opt () =
  Alcotest.(check (option int))
    "zero fits" (Some 0)
    (UInt64.to_int_opt UInt64.zero);
  Alcotest.(check (option int))
    "all ones does not fit any int" None
    (UInt64.to_int_opt UInt64.max_int);
  Alcotest.(check (option int))
    "max_int fits" (Some max_int)
    (UInt64.to_int_opt (UInt64.of_int max_int))

let suite =
  ( "uint64",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "byte layout" `Quick test_byte_layout;
      Alcotest.test_case "boundaries" `Quick test_boundaries;
      Alcotest.test_case "bounds" `Quick test_bounds;
      Alcotest.test_case "of_int refuses negative" `Quick
        test_of_int_refuses_negative;
      Alcotest.test_case "to_int_opt" `Quick test_to_int_opt;
    ] )
