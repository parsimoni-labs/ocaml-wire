(* Which fault a refused record reports. EverParse checks a record field by
   field: each field must fit, then meet its refinement, before the next one
   is read, so the first field in declaration order that does not fit or does
   not pass names the error. A failed check is reported as such even when a
   later field runs past the end of the input. The expectations below are what
   the validators 3d.exe generates from these codecs return on these bytes. *)

open Wire

let summary (e : parse_error) =
  let where = Fmt.str "%s@%d" (String.concat "." e.field) e.at in
  match e.kind with
  | Unexpected_eof _ -> "eof"
  | Constraint_failed { which = Field; _ } -> where ^ " field"
  | Constraint_failed { which = Where; _ } -> where ^ " where"
  | Constraint_failed { which = Action | Per_byte; _ } -> where ^ " other"
  | Invalid_enum _ -> where ^ " enum"
  | kind -> Fmt.str "%s %a" where pp_error_kind kind

let result = function Ok _ -> "ok" | Error e -> summary e

let check ?env codec bytes expected =
  let buf = Bytes.of_string bytes in
  let name = Fmt.str "%S" bytes in
  Alcotest.(check string)
    (name ^ " decode") expected
    (result (Codec.decode ?env codec buf 0));
  Alcotest.(check string)
    (name ^ " validate") expected
    (result (Codec.validate ?env codec buf 0))

let le10 s = Expr.(s <= int 10)

let first =
  Codec.v "First"
    (fun x y z -> (x, y, z))
    Codec.
      [
        (Field.v "x" ~self_constraint:le10 uint8 $ fun (x, _, _) -> x);
        (Field.v "y" uint8 $ fun (_, y, _) -> y);
        (Field.v "z" uint16be $ fun (_, _, z) -> z);
      ]

let test_first_field () =
  check first "\x0b" "x@0 field";
  check first "\x0b\x00\x00" "x@0 field";
  check first "\x0b\x00\x00\x00" "x@0 field";
  check first "\x01\x00" "eof"

let middle =
  Codec.v "Middle"
    (fun y x w -> (y, x, w))
    Codec.
      [
        (Field.v "y" uint16be $ fun (y, _, _) -> y);
        (Field.v "x" ~self_constraint:le10 uint8 $ fun (_, x, _) -> x);
        (Field.v "w" uint32be $ fun (_, _, w) -> w);
      ]

let test_middle_field () =
  check middle "\x00\x00\x0b" "x@2 field";
  check middle "\x00\x00\x0b\x00" "x@2 field";
  check middle "\x00" "eof";
  check middle "\x00\x00" "eof"

let f_a = Field.v "a" uint8

let dependent =
  Codec.v "Dependent"
    (fun a b c -> (a, b, c))
    Codec.
      [
        (f_a $ fun (a, _, _) -> a);
        ( Field.v "b" ~self_constraint:(fun s -> Expr.(s <= Field.ref f_a)) uint8
        $ fun (_, b, _) -> b );
        (Field.v "c" uint32be $ fun (_, _, c) -> c);
      ]

let test_dependent () = check dependent "\x01\x05" "b@1 field"

let bitfields =
  Codec.v "Bitfields"
    (fun a b c -> (a, b, c))
    Codec.
      [
        ( Field.v "a"
            ~self_constraint:(fun s -> Expr.(s <= int 3))
            (bits ~width:4 U8)
        $ fun (a, _, _) -> a );
        (Field.v "b" (bits ~width:4 U8) $ fun (_, b, _) -> b);
        (Field.v "c" uint16be $ fun (_, _, c) -> c);
      ]

let test_bitfield () =
  check bitfields "\xf0" "a@0 field";
  check bitfields "\x0f" "eof"

let outer =
  Codec.v "Outer"
    (fun h t -> (h, t))
    Codec.[ Field.v "h" (codec first) $ fst; Field.v "t" uint8 $ snd ]

let test_sub_codec () =
  check outer "\x0b" "h.x@0 field";
  check outer "\x0b\x00\x00\x00" "h.x@0 field";
  check outer "\x01\x00\x00\x00" "eof"

let f_len = Field.v "len" uint8

let before_span =
  Codec.v "BeforeSpan"
    (fun l x b -> (l, x, b))
    Codec.
      [
        (f_len $ fun (l, _, _) -> l);
        (Field.v "x" ~self_constraint:le10 uint8 $ fun (_, x, _) -> x);
        (Field.v "b" (byte_array ~size:(Field.ref f_len)) $ fun (_, _, b) -> b);
      ]

let test_before_span () =
  check before_span "\x05\x0b" "x@1 field";
  check before_span "\x05\x0b\x00" "x@1 field";
  check before_span "\x05\x01\x00" "eof"

let f_size = Field.v "len" ~self_constraint:le10 uint8

let sized =
  Codec.v "Sized"
    (fun l b -> (l, b))
    Codec.
      [ f_size $ fst; Field.v "b" (byte_array ~size:(Field.ref f_size)) $ snd ]

let test_size_field () =
  check sized "\xff\x00\x00\x00" "len@0 field";
  check sized "\x0b" "len@0 field";
  check sized "\x05\x00" "eof"

let enum_first =
  Codec.v "EnumFirst"
    (fun k v -> (k, v))
    Codec.
      [
        Field.v "k" (enum "K" [ ("A", 1); ("B", 2) ] uint8) $ fst;
        Field.v "v" uint32be $ snd;
      ]

let test_enum () =
  check enum_first "\x07" "k@0 enum";
  check enum_first "\x07\x00\x00" "k@0 enum"

let f_flag = Field.v "flag" uint8

let gated =
  Codec.v "Gated"
    (fun f o t -> (f, o, t))
    Codec.
      [
        (f_flag $ fun (f, _, _) -> f);
        ( Field.optional "o"
            ~present:Expr.(Field.ref f_flag <> int 0)
            (codec first)
        $ fun (_, o, _) -> o );
        (Field.v "t" uint8 $ fun (_, _, t) -> t);
      ]

(* 3D sizes the optional record's region first, so a region that does not fit
   is the error, whatever the bytes in it. *)
let test_optional () =
  check gated "\x01\x0b" "eof";
  check gated "\x01\x0b\x00\x00\x00" "o.x@1 field"

let f_w = Field.v "w" uint8

let where_field =
  Codec.v "WhereField"
    ~where:Expr.(Field.ref f_w <= int 10)
    (fun w y -> (w, y))
    Codec.[ f_w $ fst; Field.v "y" uint32be $ snd ]

(* A [where] clause over fields is checked as a refinement of the last field
   it reads. *)
let test_where_field () =
  check where_field "\x0b" "@0 where";
  check where_field "\x0b\x00" "@0 where";
  check where_field "\x01\x00" "eof"

let p_on = Param.input "on" uint8

let where_param =
  Codec.v "WhereParam"
    ~where:Expr.(Param.expr p_on <> int 0)
    (fun x -> x)
    Codec.[ (Field.v "x" uint32be $ fun x -> x) ]

(* A [where] clause over parameters only is checked before any field. *)
let test_where_param () =
  let env = Codec.env where_param |> Param.bind p_on (UInt8.v 0) in
  check ~env where_param "" "@0 where";
  check ~env where_param "\x00\x00" "@0 where";
  let env = Codec.env where_param |> Param.bind p_on (UInt8.v 1) in
  check ~env where_param "\x00\x00" "eof"

let f_rlen = Field.v "len" uint8

let region =
  Codec.v "Region"
    (fun l o -> (l, o))
    Codec.
      [
        f_rlen $ fst;
        Field.v "o" (nested ~size:(Field.ref f_rlen) (codec first)) $ snd;
      ]

(* The record in a region is checked in the bytes of the region: a field that
   fails there is the error, even when the record does not fit. *)
let test_region () =
  check region "\x01\x0b" "o.x@1 field";
  check region "\x02\x01\x00\x00\x00" "eof";
  check region "\x04\x0b" "eof";
  check region "\x04\x0b\x00\x00\x00" "o.x@1 field"

(* A field after a variable-size one sits at an offset read from the input. Its
   error names that offset, as a field at a fixed offset does, whichever check
   refuses it. *)
let f_sa = Field.v "a" ~self_constraint:(fun s -> Expr.(s = int 1)) uint8
let f_slen = Field.v "len" uint8

let after_span =
  Codec.v "AfterSpan"
    (fun a l d t -> (a, l, d, t))
    Codec.
      [
        (f_sa $ fun (a, _, _, _) -> a);
        (f_slen $ fun (_, l, _, _) -> l);
        ( Field.v "data" (byte_slice ~size:(Field.ref f_slen))
        $ fun (_, _, d, _) -> d );
        ( Field.v "t" ~self_constraint:(fun s -> Expr.(s = int 2)) uint8
        $ fun (_, _, _, t) -> t );
      ]

let span_then field =
  Codec.v "SpanThen"
    (fun l d x -> (l, d, x))
    Codec.
      [
        (f_slen $ fun (l, _, _) -> l);
        ( Field.v "data" (byte_slice ~size:(Field.ref f_slen)) $ fun (_, d, _) ->
          d );
        (field $ fun (_, _, x) -> x);
      ]

let test_after_span () =
  check after_span "\001\001a\003" "t@3 field";
  check after_span "\001\002ab\003" "t@4 field";
  check after_span "\002\001a\002" "a@0 field";
  let k = Field.v "k" (enum "K" [ ("A", 1); ("B", 2) ] uint8) in
  check (span_then k) "\001a\007" "k@2 enum";
  check (span_then k) "\002ab\007" "k@3 enum";
  let v = Field.v "v" (variants "V" [ ("A", `A) ] uint8) in
  check (span_then v) "\001a\007" "v@2 enum";
  let l = Field.v "l" (lookup [ `A; `B ] uint8) in
  check (span_then l) "\001a\007" "l@2 invalid tag: 7";
  let w = Field.v "w" (where Expr.false_ uint16be) in
  check (span_then w) "\001a\000\000" "w@2 field";
  let c = Field.v "c" ~constraint_:Expr.(Field.ref f_slen = int 5) uint8 in
  check (span_then c) "\001a\000" "c@2 field";
  let u = Field.v "u" ~self_int64:(fun s -> Expr.(s = int64 5L)) uint64 in
  check (span_then u) "\001a\000\000\000\000\000\000\000\000" "u@2 field";
  let x = Field.v "x" ~action:(Action.on_success [ Action.abort ]) uint8 in
  check (span_then x) "\001a\000" "x@2 other"

(* [at] is absolute: a record decoded at offset 2 reports its field at 2 more. *)
let test_after_span_base () =
  let buf = Bytes.of_string "\255\255\001\001a\003" in
  Alcotest.(check string)
    "decode" "t@5 field"
    (result (Codec.decode after_span buf 2));
  Alcotest.(check string)
    "validate" "t@5 field"
    (result (Codec.validate after_span buf 2))

let suite =
  ( "codec_fault",
    [
      Alcotest.test_case "first field" `Quick test_first_field;
      Alcotest.test_case "middle field" `Quick test_middle_field;
      Alcotest.test_case "dependent" `Quick test_dependent;
      Alcotest.test_case "bitfield" `Quick test_bitfield;
      Alcotest.test_case "sub-codec" `Quick test_sub_codec;
      Alcotest.test_case "before a span" `Quick test_before_span;
      Alcotest.test_case "size field" `Quick test_size_field;
      Alcotest.test_case "enum" `Quick test_enum;
      Alcotest.test_case "optional" `Quick test_optional;
      Alcotest.test_case "where over a field" `Quick test_where_field;
      Alcotest.test_case "where over a param" `Quick test_where_param;
      Alcotest.test_case "region" `Quick test_region;
      Alcotest.test_case "after a span" `Quick test_after_span;
      Alcotest.test_case "after a span, at an offset" `Quick
        test_after_span_base;
    ] )
