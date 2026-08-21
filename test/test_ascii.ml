(* Tests for Ascii module: RFC-style bit layout diagrams.

   Each test compares the exact rendered output against an expected string. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

let check name s expected =
  let output = Ascii.of_struct s in
  Alcotest.(check string) name expected output

let ruler =
  "  0               1               2               3              \n\
  \  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1\n"

(* -- Fixed-width fields -- *)

let test_simple () =
  check "32-bit row"
    (struct_ "S" [ field "a" uint8; field "b" uint16be; field "c" uint8 ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |       a       |               b               |       c       |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n")

let test_two_rows () =
  check "two rows"
    (struct_ "T"
       [
         field "a" uint16be;
         field "b" uint16be;
         field "c" uint16be;
         field "d" uint16be;
       ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |               a               |               b               |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |               c               |               d               |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n")

let test_bitfields () =
  check "bitfields"
    (struct_ "B"
       [
         field "version" (bits ~width:4 U32);
         field "ihl" (bits ~width:4 U32);
         field "dscp" (bits ~width:6 U32);
         field "ecn" (bits ~width:2 U32);
         field "total_length" (bits ~width:16 U32);
       ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |version|  ihl  |   dscp    |ecn|         total_length          |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n")

let test_single_field () =
  check "single uint32be"
    (struct_ "F" [ field "x" uint32be ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |                               x                               |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n")

let test_uint64 () =
  check "uint64be spans 2 rows"
    (struct_ "L" [ field "big" uint64be ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |                              big                              |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |                                                               |\n\
     \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n")

let test_single_bit () =
  check "1-bit field"
    (struct_ "B"
       [ field "F" (bits ~width:1 U8); field "rest" (bits ~width:7 U8) ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |F|    rest     |\n\
     \ +-+-+-+-+-+-+-+-+\n")

let test_non_aligned () =
  check "8-bit struct"
    (struct_ "N" [ field "x" uint8 ])
    (ruler
   ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
     \ |       x       |\n\
     \ +-+-+-+-+-+-+-+-+\n")

let test_empty () = check "empty" (struct_ "E" []) ""

(* -- Variable-length fields -- *)

let test_variable () =
  let f_len = field "len" uint16be in
  let s =
    struct_ "V"
      [
        field "len" uint16be; field "data" (byte_array ~size:(field_ref f_len));
      ]
  in
  let output = Ascii.of_struct s in
  (* Variable-length output has a full-width row for the data field *)
  Alcotest.(check bool)
    "contains len" true
    (Re.execp (Re.compile (Re.str "len")) output);
  Alcotest.(check bool)
    "contains data" true
    (Re.execp (Re.compile (Re.str "data")) output)

(* -- Expression rendering --

   Every expression constructor must reach the diagram. A "?" in the output
   means the printer dropped one, which is what the old catch-all did to masks,
   shifts, [sizeof] and parameter references. *)

let renders name s expect =
  let output = Ascii.of_struct s in
  if String.contains output '?' then
    Alcotest.failf "%s: expression dropped from diagram:@\n%s" name output;
  Alcotest.(check bool)
    (Fmt.str "%s renders %S" name expect)
    true
    (Re.execp (Re.compile (Re.str expect)) output)

let len_ref = field_ref (field "len" uint16be)

let sized_by size =
  struct_ "Sized" [ field "len" uint16be; field "data" (byte_array ~size) ]

let expr_cases =
  [
    ("div", Expr.(len_ref / int 4), "len / 4");
    ("mod", Expr.(len_ref mod int 4), "len % 4");
    ("land", Expr.(len_ref land int 255), "len & 255");
    ("lor", Expr.(len_ref lor int 1), "len | 1");
    ("lxor", Expr.(len_ref lxor int 1), "len ^ 1");
    ("lnot", Expr.(lnot len_ref), "~len");
    ("lsl", Expr.(len_ref lsl int 2), "len << 2");
    ("lsr", Expr.(len_ref lsr int 2), "len >> 2");
    ("cast", Expr.to_uint8 len_ref, "(u8) len");
    ( "if_then_else",
      Expr.(if_then_else (len_ref = int 0) (int 8) len_ref),
      "if len == 0 then 8 else len" );
    ("sizeof", sizeof uint32be, "data (4 bytes)");
    ("sizeof of a variable type", sizeof (byte_array ~size:len_ref), "sizeof");
    ("sizeof_this", Expr.(len_ref - sizeof_this), "len - sizeof(this)");
    ("field_pos", Expr.(len_ref + field_pos), "len + field_pos");
    ("negative literal", Expr.(len_ref + int (-1)), "-1");
  ]

let test_expr_operators () =
  List.iter
    (fun (name, e, expect) -> renders name (sized_by e) expect)
    expr_cases

(* [Wire.rest_bytes] is the everyday source of both constructors: the trailing
   payload of a record sized by an input parameter. *)
let test_param_ref () =
  let total = Param.input "total" int32be in
  let s =
    param_struct "Trailing"
      [ Param.decl total ]
      [ field "hdr" uint32be; field "rest" (rest_bytes total) ]
  in
  renders "rest_bytes" s "total - sizeof(this)"

let wide self_int64 =
  struct_ "Wide" [ Field.Named (Field.v "mask" ~self_int64 uint64be) ]

let test_int64_operators () =
  renders "land64"
    (wide (fun self -> Expr.(land64 self (int64 255L) = int64 0L)))
    "mask & 255";
  renders "lsr64"
    (wide (fun self -> Expr.(lsr64 self (int 56) = int64 0L)))
    "mask >> 56"

(* -- Codec rendering --
   [multi_record_codec] lives in {!Test_helpers}. *)

let test_of_codec () =
  let output = Ascii.of_codec multi_record_codec in
  let expected =
    ruler
    ^ " +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\
      \ |               x               |               y               |\n\
      \ +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n"
  in
  Alcotest.(check string) "of_codec" expected output

(* -- pp formatters -- *)

let test_pp_codec () =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Ascii.pp_codec ppf multi_record_codec;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buf in
  Alcotest.(check bool) "pp_codec non-empty" true (String.length output > 0);
  Alcotest.(check string)
    "pp_codec matches of_codec"
    (Ascii.of_codec multi_record_codec)
    output

let test_pp_struct () =
  let s = struct_ "P" [ field "a" uint8; field "b" uint8 ] in
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Ascii.pp_struct ppf s;
  Format.pp_print_flush ppf ();
  let output = Buffer.contents buf in
  Alcotest.(check string)
    "pp_struct matches of_struct" (Ascii.of_struct s) output

(* -- Suite -- *)

let suite =
  ( "ascii",
    [
      Alcotest.test_case "simple 32-bit row" `Quick test_simple;
      Alcotest.test_case "two rows" `Quick test_two_rows;
      Alcotest.test_case "bitfields" `Quick test_bitfields;
      Alcotest.test_case "single field" `Quick test_single_field;
      Alcotest.test_case "uint64 spans rows" `Quick test_uint64;
      Alcotest.test_case "single bit" `Quick test_single_bit;
      Alcotest.test_case "non-aligned" `Quick test_non_aligned;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "variable length" `Quick test_variable;
      Alcotest.test_case "expression operators" `Quick test_expr_operators;
      Alcotest.test_case "parameter reference" `Quick test_param_ref;
      Alcotest.test_case "64-bit operators" `Quick test_int64_operators;
      Alcotest.test_case "of_codec" `Quick test_of_codec;
      Alcotest.test_case "pp_codec" `Quick test_pp_codec;
      Alcotest.test_case "pp_struct" `Quick test_pp_struct;
    ] )
