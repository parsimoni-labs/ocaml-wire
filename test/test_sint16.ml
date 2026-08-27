(* Tests for SInt16, the signed 16-bit carrier behind [int16] and [int16be].
   The range lives in the type, so what is left to check is the constructor's
   refusal and the two byte layouts: every value of the type is one a signed
   16-bit field holds, and no encoder downstream has to ask again. *)

open Wire

(* [v] refuses a number the field cannot hold rather than masking it to a legal
   one the caller did not mean: 40000 masks to a perfectly good -25536 that no
   decoder, [validate] or generated C validator could tell from the number the
   caller wrote. *)
let test_v_refuses_out_of_range () =
  List.iter
    (fun n ->
      match SInt16.v n with
      | x -> Alcotest.failf "v %d built %a" n SInt16.pp x
      | exception Invalid_argument _ -> ())
    [ 0x8000; 40000; 0xFFFF; 0x1_0000; -0x8001; -40000; max_int; min_int ]

let test_v_accepts_the_whole_range () =
  for n = -0x8000 to 0x7FFF do
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "v %d round-trips" n
      (SInt16.to_int (SInt16.v n))
  done

let test_v_opt () =
  Alcotest.(check (option int))
    "in range" (Some 0x7FFF)
    (Option.map SInt16.to_int (SInt16.v_opt 0x7FFF));
  Alcotest.(check bool) "out of range" true (SInt16.v_opt 0x8000 = None)

(* [min_int] and [max_int] are the field's ends, not the carrier's. *)
let test_bounds () =
  Alcotest.(check int) "min_int" (-0x8000) (SInt16.to_int SInt16.min_int);
  Alcotest.(check int) "max_int" 0x7FFF (SInt16.to_int SInt16.max_int);
  Alcotest.(check int) "zero" 0 (SInt16.to_int SInt16.zero)

let test_roundtrip () =
  let buf = Bytes.create 2 in
  for n = -0x8000 to 0x7FFF do
    SInt16.set_le buf 0 (SInt16.v n);
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "%d survives two little-endian bytes" n
      (SInt16.to_int (SInt16.le buf 0));
    SInt16.set_be buf 0 (SInt16.v n);
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "%d survives two big-endian bytes" n
      (SInt16.to_int (SInt16.be buf 0))
  done

(* Pin both byte orders so the read and write paths cannot drift apart, and so
   one endianness cannot quietly stand in for the other: a pattern with the top
   bit set is the negative number it spells, not the unsigned one. *)
let test_byte_layout () =
  Alcotest.(check int)
    "0xFE 0xFF reads -2 little-endian" (-2)
    (SInt16.to_int (SInt16.le (Bytes.of_string "\xFE\xFF") 0));
  Alcotest.(check int)
    "0xFF 0xFE reads -2 big-endian" (-2)
    (SInt16.to_int (SInt16.be (Bytes.of_string "\xFF\xFE") 0));
  Alcotest.(check int)
    "0x00 0x80 reads min_int little-endian" (-0x8000)
    (SInt16.to_int (SInt16.le (Bytes.of_string "\x00\x80") 0));
  Alcotest.(check int)
    "0x80 0x00 reads min_int big-endian" (-0x8000)
    (SInt16.to_int (SInt16.be (Bytes.of_string "\x80\x00") 0));
  let out = Bytes.create 2 in
  SInt16.set_le out 0 SInt16.min_int;
  Alcotest.(check string)
    "min_int writes 0x00 0x80 little-endian" "\x00\x80" (Bytes.to_string out);
  SInt16.set_be out 0 SInt16.min_int;
  Alcotest.(check string)
    "min_int writes 0x80 0x00 big-endian" "\x80\x00" (Bytes.to_string out)

let test_equal () =
  Alcotest.(check bool)
    "reflexive" true
    (SInt16.equal (SInt16.v 4242) (SInt16.v 4242));
  Alcotest.(check bool)
    "min_int <> max_int" false
    (SInt16.equal SInt16.min_int SInt16.max_int)

let test_pp () =
  Alcotest.(check string)
    "min_int" "-32768"
    (Fmt.str "%a" SInt16.pp SInt16.min_int)

let suite =
  ( "sint16",
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
