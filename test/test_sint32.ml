(* Tests for SInt32: signed 32-bit get/set over bytes. Values are written as
   [Int32] literals and compared through [to_int32]: an int literal spanning the
   full range would itself truncate on a narrow-int target (wasm_of_ocaml,
   js_of_ocaml), and these tests run there too. *)

open Wire

let check_roundtrip name ~set ~get v =
  let buf = Bytes.create 4 in
  set buf 0 (SInt32.of_int32 v);
  Alcotest.(check int32) name v (SInt32.to_int32 (get buf 0))

let test_roundtrip_le () =
  check_roundtrip "le roundtrip" ~set:SInt32.set_le ~get:SInt32.le 0xDEAD_BEEFl

let test_roundtrip_be () =
  check_roundtrip "be roundtrip" ~set:SInt32.set_be ~get:SInt32.be 0xCAFE_BABEl

(* Pin the wire byte order so the read and write paths cannot drift apart. *)
let test_byte_layout () =
  let buf = Bytes.of_string "\x01\x02\x03\x04" in
  Alcotest.(check int32)
    "le read" 0x04030201l
    (SInt32.to_int32 (SInt32.le buf 0));
  Alcotest.(check int32)
    "be read" 0x01020304l
    (SInt32.to_int32 (SInt32.be buf 0));
  let out = Bytes.create 4 in
  SInt32.set_le out 0 (SInt32.of_int32 0x04030201l);
  Alcotest.(check string) "le write" "\x01\x02\x03\x04" (Bytes.to_string out);
  SInt32.set_be out 0 (SInt32.of_int32 0x01020304l);
  Alcotest.(check string) "be write" "\x01\x02\x03\x04" (Bytes.to_string out)

(* The defect this type exists to close: a pattern with bit 31 set is a negative
   number, and it has to survive the read where the native int is narrower than
   the field rather than coming back as a legal smaller one. *)
let test_sign_bit () =
  let buf = Bytes.of_string "\x80\x00\x00\x00" in
  Alcotest.(check int32)
    "min_int be" Int32.min_int
    (SInt32.to_int32 (SInt32.be buf 0));
  let out = Bytes.create 4 in
  SInt32.set_be out 0 (SInt32.of_int32 Int32.min_int);
  Alcotest.(check string)
    "min_int re-encodes" "\x80\x00\x00\x00" (Bytes.to_string out)

let test_boundaries () =
  List.iter
    (fun v ->
      check_roundtrip "le" ~set:SInt32.set_le ~get:SInt32.le v;
      check_roundtrip "be" ~set:SInt32.set_be ~get:SInt32.be v)
    Int32.
      [
        min_int;
        add min_int one;
        minus_one;
        zero;
        one;
        0xFFFFl;
        0x1_0000l;
        sub max_int one;
        max_int;
      ]

(* [compare] must order as a signed 32-bit integer on both carriers: a native
   int compares as itself, and the boxed fallback is [Int32], whose comparison
   is already signed. The unsigned sibling is the one that cannot inherit it. *)
let test_compare_is_signed () =
  let s = SInt32.of_int32 in
  Alcotest.(check bool)
    "min_int < 0" true
    (SInt32.compare (s Int32.min_int) SInt32.zero < 0);
  Alcotest.(check bool)
    "-1 < 0" true
    (SInt32.compare (s Int32.minus_one) SInt32.zero < 0);
  Alcotest.(check bool)
    "max_int > 0" true
    (SInt32.compare (s Int32.max_int) SInt32.zero > 0);
  Alcotest.(check bool) "equal reflexive" true (SInt32.equal (s 42l) (s 42l));
  Alcotest.(check bool)
    "min_int <> max_int" false
    (SInt32.equal (s Int32.min_int) (s Int32.max_int))

(* [min_int] and [max_int] are the field's ends, not the carrier's. *)
let test_bounds () =
  Alcotest.(check int32)
    "min_int" Int32.min_int
    (SInt32.to_int32 SInt32.min_int);
  Alcotest.(check int32)
    "max_int" Int32.max_int
    (SInt32.to_int32 SInt32.max_int);
  Alcotest.(check int32) "zero" 0l (SInt32.to_int32 SInt32.zero)

(* [of_int] refuses a number the field cannot hold instead of masking it to a
   legal one the caller did not mean. Only a native int wider than the field can
   supply such a number, so the refusal is reachable only there. *)
let test_of_int_refuses_out_of_range () =
  if Sys.int_size > 32 then begin
    let limit = 1 lsl 31 in
    List.iter
      (fun n ->
        match SInt32.of_int n with
        | v -> Alcotest.failf "of_int %d built %a" n SInt32.pp v
        | exception Invalid_argument _ -> ())
      [ limit; -limit - 1; max_int; min_int ]
  end;
  List.iter
    (fun n ->
      Fmt.kstr
        (fun msg -> Alcotest.(check int32) msg (Int32.of_int n))
        "of_int %d round-trips" n
        (SInt32.to_int32 (SInt32.of_int n)))
    [ -1; 0; 1; 0xFFFF ]

(* [to_int_opt] answers whether the native int held the value, so a caller that
   must not silently drop the top bits has something total to ask. *)
let test_to_int_opt () =
  let ends = SInt32.[ min_int; max_int ] in
  List.iter
    (fun v ->
      match SInt32.to_int_opt v with
      | Some n when Sys.int_size > 32 ->
          Alcotest.(check int32)
            "wide int holds it" (SInt32.to_int32 v) (Int32.of_int n)
      | Some _ -> ()
      | None ->
          Alcotest.(check bool)
            "None only where the int is too narrow" true (Sys.int_size <= 32))
    ends;
  Alcotest.(check (option int))
    "zero fits" (Some 0)
    (SInt32.to_int_opt SInt32.zero)

let suite =
  ( "sint32",
    [
      Alcotest.test_case "roundtrip le" `Quick test_roundtrip_le;
      Alcotest.test_case "roundtrip be" `Quick test_roundtrip_be;
      Alcotest.test_case "byte layout" `Quick test_byte_layout;
      Alcotest.test_case "sign bit preserved" `Quick test_sign_bit;
      Alcotest.test_case "boundaries" `Quick test_boundaries;
      Alcotest.test_case "compare is signed" `Quick test_compare_is_signed;
      Alcotest.test_case "bounds" `Quick test_bounds;
      Alcotest.test_case "of_int refuses out of range" `Quick
        test_of_int_refuses_out_of_range;
      Alcotest.test_case "to_int_opt" `Quick test_to_int_opt;
    ] )
