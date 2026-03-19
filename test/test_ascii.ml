(* Tests for Ascii module: RFC-style bit layout diagrams. *)

open Wire
open Wire.C

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* ── Fixed-width fields ── *)

let test_simple_struct () =
  let s =
    struct_ "Simple" [ field "a" uint8; field "b" uint16be; field "c" uint8 ]
  in
  let output = Ascii.of_struct s in
  (* 8+16+8 = 32 bits = one row *)
  Alcotest.(check bool) "contains ruler" true (contains ~sub:"0 1 2 3" output);
  Alcotest.(check bool) "contains a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains b" true (contains ~sub:"b" output);
  Alcotest.(check bool) "contains c" true (contains ~sub:"c" output);
  Alcotest.(check bool) "contains separators" true (contains ~sub:"+-" output)

let test_two_row_struct () =
  let s =
    struct_ "TwoRow"
      [
        field "a" uint16be;
        field "b" uint16be;
        field "c" uint16be;
        field "d" uint16be;
      ]
  in
  let output = Ascii.of_struct s in
  (* 4 x 16 = 64 bits = two rows *)
  Alcotest.(check bool) "contains a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains d" true (contains ~sub:"d" output);
  (* Should have 3 separator lines (top, middle, bottom) *)
  let seps =
    String.split_on_char '\n' output
    |> List.filter (fun l -> contains ~sub:"+-" l)
    |> List.length
  in
  Alcotest.(check int) "3 separator lines" 3 seps

let test_bitfield_struct () =
  let s =
    struct_ "BF"
      [
        field "version" (bits ~width:4 U32);
        field "ihl" (bits ~width:4 U32);
        field "dscp" (bits ~width:6 U32);
        field "ecn" (bits ~width:2 U32);
        field "total_length" (bits ~width:16 U32);
      ]
  in
  let output = Ascii.of_struct s in
  (* 4+4+6+2+16 = 32 bits = one row *)
  Alcotest.(check bool) "contains version" true (contains ~sub:"version" output);
  Alcotest.(check bool) "contains ihl" true (contains ~sub:"ihl" output);
  Alcotest.(check bool)
    "contains total_length" true
    (contains ~sub:"total_length" output)

(* ── Variable-length fields ── *)

let test_variable_length () =
  let f_len = field "len" uint16be in
  let s =
    struct_ "VarLen"
      [
        field "len" uint16be; field "data" (byte_array ~size:(field_ref f_len));
      ]
  in
  let output = Ascii.of_struct s in
  Alcotest.(check bool) "contains len" true (contains ~sub:"len" output);
  Alcotest.(check bool)
    "contains data annotation" true
    (contains ~sub:"data" output);
  Alcotest.(check bool) "shows dependency" true (contains ~sub:"len" output)

(* ── Codec rendering ── *)

type simple_record = { x : int; y : int }

let simple_codec =
  Codec.view "Simple"
    (fun x y -> { x; y })
    Codec.
      [
        Codec.field "x" uint16be (fun r -> r.x);
        Codec.field "y" uint16be (fun r -> r.y);
      ]

let test_of_codec () =
  let output = Ascii.of_codec simple_codec in
  Alcotest.(check bool) "contains x" true (contains ~sub:"x" output);
  Alcotest.(check bool) "contains y" true (contains ~sub:"y" output)

(* ── Empty struct ── *)

let test_empty_struct () =
  let s = struct_ "Empty" [] in
  let output = Ascii.of_struct s in
  Alcotest.(check string) "empty output" "" output

(* ── Large field spanning rows ── *)

let test_large_field () =
  let s = struct_ "Large" [ field "big" uint64be ] in
  let output = Ascii.of_struct s in
  (* 64 bits = 2 rows *)
  Alcotest.(check bool) "contains big" true (contains ~sub:"big" output);
  let content_lines =
    String.split_on_char '\n' output
    |> List.filter (fun l -> contains ~sub:"|" l && not (contains ~sub:"+-" l))
    |> List.length
  in
  Alcotest.(check int) "2 content rows" 2 content_lines

(* ── Suite ── *)

let suite =
  ( "ascii",
    [
      Alcotest.test_case "simple struct" `Quick test_simple_struct;
      Alcotest.test_case "two-row struct" `Quick test_two_row_struct;
      Alcotest.test_case "bitfield struct" `Quick test_bitfield_struct;
      Alcotest.test_case "variable length" `Quick test_variable_length;
      Alcotest.test_case "of_codec" `Quick test_of_codec;
      Alcotest.test_case "empty struct" `Quick test_empty_struct;
      Alcotest.test_case "large field (64-bit)" `Quick test_large_field;
    ] )
