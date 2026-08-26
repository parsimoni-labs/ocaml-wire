(* Tests for SInt8, the signed 8-bit carrier behind [int8]. The range lives in
   the type, so what is left to check is the constructor's refusal and the byte
   layout: every value of the type is one a signed byte holds, and no encoder
   downstream has to ask again. *)

open Wire

(* [v] refuses a number the field cannot hold rather than masking it to a legal
   one the caller did not mean: 200 masks to a perfectly good -56 that no
   decoder, [validate] or generated C validator could tell from the number the
   caller wrote. *)
let test_v_refuses_out_of_range () =
  List.iter
    (fun n ->
      match SInt8.v n with
      | x -> Alcotest.failf "v %d built %a" n SInt8.pp x
      | exception Invalid_argument _ -> ())
    [ 128; 200; 255; -129; -1000; max_int; min_int ]

let test_v_accepts_the_whole_range () =
  for n = -128 to 127 do
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "v %d round-trips" n
      (SInt8.to_int (SInt8.v n))
  done

let test_v_opt () =
  Alcotest.(check (option int))
    "in range" (Some 127)
    (Option.map SInt8.to_int (SInt8.v_opt 127));
  Alcotest.(check bool) "out of range" true (SInt8.v_opt 128 = None)

(* [min_int] and [max_int] are the field's ends, not the carrier's. *)
let test_bounds () =
  Alcotest.(check int) "min_int" (-128) (SInt8.to_int SInt8.min_int);
  Alcotest.(check int) "max_int" 127 (SInt8.to_int SInt8.max_int);
  Alcotest.(check int) "zero" 0 (SInt8.to_int SInt8.zero)

let test_roundtrip () =
  let buf = Bytes.create 1 in
  for n = -128 to 127 do
    SInt8.set buf 0 (SInt8.v n);
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "%d survives the byte" n
      (SInt8.to_int (SInt8.get buf 0))
  done

(* Pin the byte so the read and write paths cannot drift apart: a pattern with
   the top bit set is the negative number it spells, not the unsigned one. *)
let test_byte_layout () =
  Alcotest.(check int)
    "0xFE reads -2" (-2)
    (SInt8.to_int (SInt8.get (Bytes.of_string "\xFE") 0));
  Alcotest.(check int)
    "0x80 reads min_int" (-128)
    (SInt8.to_int (SInt8.get (Bytes.of_string "\x80") 0));
  let out = Bytes.create 1 in
  SInt8.set out 0 SInt8.min_int;
  Alcotest.(check string) "min_int writes 0x80" "\x80" (Bytes.to_string out)

let test_equal () =
  Alcotest.(check bool) "reflexive" true (SInt8.equal (SInt8.v 42) (SInt8.v 42));
  Alcotest.(check bool)
    "min_int <> max_int" false
    (SInt8.equal SInt8.min_int SInt8.max_int)

let test_pp () =
  Alcotest.(check string) "min_int" "-128" (Fmt.str "%a" SInt8.pp SInt8.min_int)

let suite =
  ( "sint8",
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
