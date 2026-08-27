(* Tests for UInt8, the unsigned 8-bit carrier behind [uint8]. The range lives
   in the type, so what is left to check is the constructor's refusal and the
   byte layout: every value of the type is one an unsigned byte holds, and no
   encoder downstream has to ask again. *)

open Wire

(* [v] refuses a number the field cannot hold rather than masking it to a legal
   one the caller did not mean: 511 masks to a perfectly good 255 that no
   decoder, [validate] or generated C validator could tell from the number the
   caller wrote. *)
let test_v_refuses_out_of_range () =
  List.iter
    (fun n ->
      match UInt8.v n with
      | x -> Alcotest.failf "v %d built %a" n UInt8.pp x
      | exception Invalid_argument _ -> ())
    [ 0x100; 511; -1; -0x80; max_int; min_int ]

let test_v_accepts_the_whole_range () =
  for n = 0 to 0xFF do
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "v %d round-trips" n
      (UInt8.to_int (UInt8.v n))
  done

let test_v_opt () =
  Alcotest.(check (option int))
    "in range" (Some 0xFF)
    (Option.map UInt8.to_int (UInt8.v_opt 0xFF));
  Alcotest.(check bool) "out of range" true (UInt8.v_opt 0x100 = None)

(* [min_int] and [max_int] are the field's ends, not the carrier's. *)
let test_bounds () =
  Alcotest.(check int) "min_int" 0 (UInt8.to_int UInt8.min_int);
  Alcotest.(check int) "max_int" 0xFF (UInt8.to_int UInt8.max_int);
  Alcotest.(check int) "zero" 0 (UInt8.to_int UInt8.zero)

let test_roundtrip () =
  let buf = Bytes.create 1 in
  for n = 0 to 0xFF do
    UInt8.set buf 0 (UInt8.v n);
    Fmt.kstr
      (fun msg -> Alcotest.(check int) msg n)
      "%d survives the byte" n
      (UInt8.to_int (UInt8.get buf 0))
  done

(* Pin the byte so the read and write paths cannot drift apart, and so a
   pattern with the top bit set is the large positive number it spells rather
   than the negative one the same byte would spell signed. *)
let test_byte_layout () =
  Alcotest.(check int)
    "0xFE reads 254" 0xFE
    (UInt8.to_int (UInt8.get (Bytes.of_string "\xFE") 0));
  Alcotest.(check int)
    "0x80 reads 128" 0x80
    (UInt8.to_int (UInt8.get (Bytes.of_string "\x80") 0));
  let out = Bytes.create 1 in
  UInt8.set out 0 UInt8.max_int;
  Alcotest.(check string) "max_int writes 0xFF" "\xFF" (Bytes.to_string out)

let test_equal () =
  Alcotest.(check bool) "reflexive" true (UInt8.equal (UInt8.v 42) (UInt8.v 42));
  Alcotest.(check bool)
    "min_int <> max_int" false
    (UInt8.equal UInt8.min_int UInt8.max_int)

let test_pp () =
  Alcotest.(check string) "max_int" "255" (Fmt.str "%a" UInt8.pp UInt8.max_int)

let suite =
  ( "uint8",
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
