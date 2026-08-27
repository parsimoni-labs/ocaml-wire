(* Tests for UInt16, the unsigned 16-bit carrier behind [uint16] and
   [uint16be]. The range lives in the type, so what is left to check is the
   constructor's refusal and the two byte layouts: every value of the type is
   one an unsigned 16-bit field holds, and no encoder downstream has to ask
   again. *)

open Wire

(* [v] refuses a number the field cannot hold rather than masking it to a legal
   one the caller did not mean: 70000 masks to a perfectly good 4464 that no
   decoder, [validate] or generated C validator could tell from the number the
   caller wrote. *)
let test_v_refuses_out_of_range () =
  List.iter
    (fun n ->
      match UInt16.v n with
      | x -> Alcotest.failf "v %d built %a" n UInt16.pp x
      | exception Invalid_argument _ -> ())
    [ 0x1_0000; 70000; -1; -0x8000; max_int; min_int ]

let test_v_accepts_the_whole_range () =
  for n = 0 to 0xFFFF do
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "v %d round-trips" n
      (UInt16.to_int (UInt16.v n))
  done

let test_v_opt () =
  Alcotest.(check (option int))
    "in range" (Some 0xFFFF)
    (Option.map UInt16.to_int (UInt16.v_opt 0xFFFF));
  Alcotest.(check bool) "out of range" true (UInt16.v_opt 0x1_0000 = None)

(* [min_int] and [max_int] are the field's ends, not the carrier's. *)
let test_bounds () =
  Alcotest.(check int) "min_int" 0 (UInt16.to_int UInt16.min_int);
  Alcotest.(check int) "max_int" 0xFFFF (UInt16.to_int UInt16.max_int);
  Alcotest.(check int) "zero" 0 (UInt16.to_int UInt16.zero)

let test_roundtrip () =
  let buf = Bytes.create 2 in
  for n = 0 to 0xFFFF do
    UInt16.set_le buf 0 (UInt16.v n);
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "%d survives two little-endian bytes" n
      (UInt16.to_int (UInt16.le buf 0));
    UInt16.set_be buf 0 (UInt16.v n);
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "%d survives two big-endian bytes" n
      (UInt16.to_int (UInt16.be buf 0))
  done

(* Pin both byte orders so the read and write paths cannot drift apart, and so
   one endianness cannot quietly stand in for the other: a pattern with the top
   bit set is the large positive number it spells, not a negative one. *)
let test_byte_layout () =
  Alcotest.(check int)
    "0xFE 0xFF reads 65534 little-endian" 0xFFFE
    (UInt16.to_int (UInt16.le (Bytes.of_string "\xFE\xFF") 0));
  Alcotest.(check int)
    "0xFF 0xFE reads 65534 big-endian" 0xFFFE
    (UInt16.to_int (UInt16.be (Bytes.of_string "\xFF\xFE") 0));
  Alcotest.(check int)
    "0x00 0x80 reads 32768 little-endian" 0x8000
    (UInt16.to_int (UInt16.le (Bytes.of_string "\x00\x80") 0));
  Alcotest.(check int)
    "0x80 0x00 reads 32768 big-endian" 0x8000
    (UInt16.to_int (UInt16.be (Bytes.of_string "\x80\x00") 0));
  let out = Bytes.create 2 in
  UInt16.set_le out 0 (UInt16.v 0x8000);
  Alcotest.(check string)
    "32768 writes 0x00 0x80 little-endian" "\x00\x80" (Bytes.to_string out);
  UInt16.set_be out 0 (UInt16.v 0x8000);
  Alcotest.(check string)
    "32768 writes 0x80 0x00 big-endian" "\x80\x00" (Bytes.to_string out)

let test_equal () =
  Alcotest.(check bool)
    "reflexive" true
    (UInt16.equal (UInt16.v 4242) (UInt16.v 4242));
  Alcotest.(check bool)
    "min_int <> max_int" false
    (UInt16.equal UInt16.min_int UInt16.max_int)

let test_pp () =
  Alcotest.(check string)
    "max_int" "65535"
    (Fmt.str "%a" UInt16.pp UInt16.max_int)

let suite =
  ( "uint16",
    [
      Alcotest.test_case "v refuses out of range" `Quick
        test_v_refuses_out_of_range;
      Alcotest.test_case "v accepts the whole range" `Quick
        test_v_accepts_the_whole_range;
      Alcotest.test_case "v_opt" `Quick test_v_opt;
      Alcotest.test_case "bounds" `Quick test_bounds;
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "byte layout" `Quick test_byte_layout;
      Alcotest.test_case "equal" `Quick test_equal;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
