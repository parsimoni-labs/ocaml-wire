(* Tests for everparse.ml: declaration names, their clashes and escaping. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

(* Two declarations under one name are two types with one name, and every
   reference resolves to whichever was reached first: two same-named sub-codecs
   of different widths gave a validator reading a different number of bytes than
   the codec, with no error anywhere. [Everparse.write] refuses this across
   schemas; within one schema nothing did. *)
let test_duplicate_declaration_names_rejected () =
  let two name a b =
    Codec.v name
      (fun x y -> (x, y))
      Codec.[ Field.v "x" a $ fst; Field.v "y" b $ snd ]
  in
  let refuses what c =
    match
      to_3d ~enum_as_type:true (Everparse.project ~mode:`Standalone c).module_
    with
    | _ -> Alcotest.failf "%s: expected the collision to be refused" what
    | exception Invalid_argument msg ->
        Alcotest.(check bool)
          (what ^ ": names the colliding declaration")
          true
          (contains ~sub:"named" msg)
  in
  refuses "sub-codecs"
    (two "Clash"
       (codec (two "Dk" uint8 uint8))
       (codec (two "Dk" uint16 uint16)));
  refuses "enums"
    (Codec.v "EClash"
       (fun x y -> (x, y))
       Codec.
         [
           Field.v "x" (enum "Dup" [ ("A", 1) ] uint8) $ fst;
           Field.v "y" (enum "Dup" [ ("B", 9) ] uint8) $ snd;
         ])

(* A case body was rendered through the type suffix alone, never through the
   refinement machinery a field of the same type goes through, so a closed
   enum's membership and a lookup's index bound vanished from the generated
   validator while the codec kept enforcing them. *)

(* Two different casetypes under one name are two types with one name: every
   reference would resolve to the first, so the projection refuses them as it
   refuses two different sub-codecs. The same casetype reached twice is one
   declaration. *)
let body second =
  casetype "Body" uint8
    [
      case ~index:(UInt8.v 1) uint8
        ~inject:(fun v -> `A v)
        ~project:(function `A v -> Some v | `B _ -> None);
      case ~index:(UInt8.v 2) second
        ~inject:(fun v -> `B v)
        ~project:(function `B v -> Some v | `A _ -> None);
    ]

let two_bodies name b1 b2 =
  Codec.v name
    (fun x y -> (x, y))
    Codec.[ Field.v "x" b1 $ fst; Field.v "y" b2 $ snd ]

let test_3d_casetype_name_collision () =
  (match
     Everparse.project ~mode:`Ffi
       (two_bodies "TwoBodies" (body uint8) (body uint16be))
   with
  | _ -> Alcotest.fail "two different casetypes named Body were projected"
  | exception Invalid_argument _ -> ());
  let b = body uint8 in
  let out =
    to_3d (Everparse.project ~mode:`Ffi (two_bodies "SameBody" b b)).module_
  in
  let count sub = List.length (Re.all (Re.compile (Re.str sub)) out) in
  Alcotest.(check int) "one dispatch" 1 (count "casetype _Body_Body");
  Alcotest.(check int) "one wrapper" 1 (count "typedef struct WireBody")

(* -- Reserved word escaping -- *)

let test_reserved_word_escaping () =
  let f_type = field "type" uint8 in
  let f_case = field "case" uint16be in
  let s =
    struct_ "Reserved"
      [ field "type" uint8; field "case" uint16be; field "value" uint32 ]
  in
  let m =
    module_
      [
        typedef ~entrypoint:true
          (param_struct "Reserved"
             [ param "return" uint8 ]
             [
               field "type" uint8;
               field "case"
                 ~constraint_:
                   Expr.(field_ref f_type + field_ref f_case <= int 10)
                 uint16be;
             ]);
      ]
  in
  ignore s;
  let output = to_3d m in
  Alcotest.(check bool) "type escaped" true (contains ~sub:"UINT8 type_" output);
  Alcotest.(check bool)
    "case escaped" true
    (contains ~sub:"UINT16BE case_" output);
  Alcotest.(check bool)
    "return escaped in param" true
    (contains ~sub:"UINT8 return_" output);
  Alcotest.(check bool)
    "type_ in constraint" true
    (contains ~sub:"type_" output);
  Alcotest.(check bool)
    "no bare reserved word as field" false
    (contains ~sub:"UINT8 type;" output)

(* Every name below is a keyword of 3D or of the F* EverParse writes from it,
   checked against 3d.exe: a field so named either fails the 3D parser or,
   once a constraint reads it or a sub-codec types it, gets into the F* as a
   bare binder and fails there with a syntax error. *)
let reserved_3d_or_fstar =
  [
    "end";
    "in";
    "of";
    "opaque";
    "quote";
    "unfoldable";
    "aligned";
    "all_bytes";
    "all_zeros";
    "field_pos";
    "field_ptr";
    "field_ptr_after";
    "is_range_okay";
    "pointer";
    "probe";
    "refining";
    "specialize";
    "string";
    "UINT8BE";
  ]

let test_reserved_word_escaping_fstar () =
  List.iter
    (fun name ->
      let f = field name uint8 in
      let s =
        struct_ "Kw"
          [ f; field "y" ~constraint_:Expr.(field_ref f <= int 10) uint8 ]
      in
      let out = to_3d (module_ [ typedef ~entrypoint:true s ]) in
      Alcotest.(check bool)
        (Fmt.str "%s escaped" name)
        true
        (contains ~sub:(Fmt.str "UINT8 %s_;" name) out);
      Alcotest.(check bool)
        (Fmt.str "%s escaped where read" name)
        true
        (contains ~sub:(Fmt.str "(%s_ <= 10)" name) out))
    reserved_3d_or_fstar

let invalid_arg_mentions what subs f =
  match f () with
  | _ -> Alcotest.failf "%s: accepted" what
  | exception Invalid_argument msg ->
      List.iter
        (fun sub ->
          if not (contains ~sub msg) then
            Alcotest.failf "%s: %S does not mention %S" what msg sub)
        subs

(* A keyword is escaped by a trailing underscore, which can spell another
   field's name: [end] and [end_] would both be [end_] in 3D. A struct's
   parameters share its fields' scope, so the [ctx] parameter of the FFI
   projection clashes with a field of that name. *)
let test_3d_escaped_names_clash () =
  let module_of s = module_ [ typedef ~entrypoint:true s ] in
  invalid_arg_mentions "end and end_" [ "\"end\""; "\"end_\"" ] (fun () ->
      to_3d (module_of (struct_ "Kw" [ field "end" uint8; field "end_" uint8 ])));
  invalid_arg_mentions "type and type_" [ "\"type\""; "\"type_\"" ] (fun () ->
      to_3d
        (module_of (struct_ "Kw" [ field "type_" uint8; field "type" uint8 ])));
  invalid_arg_mentions "param and field" [ "\"n\"" ] (fun () ->
      to_3d
        (module_of (param_struct "Kw" [ param "n" uint8 ] [ field "n" uint8 ])));
  invalid_arg_mentions "field ctx in FFI" [ "\"ctx\"" ] (fun () ->
      Everparse.Raw.project_struct ~mode:`Ffi
        (struct_ "Kw" [ field "ctx" uint8 ])
      |> fun s -> to_3d s.Everparse.module_);
  (* Distinct names still project, the escaped one under its escape. *)
  let out =
    to_3d (module_of (struct_ "Kw" [ field "end" uint8; field "end2" uint8 ]))
  in
  Alcotest.(check bool)
    "end_ and end2" true
    (contains ~sub:"UINT8 end_;" out && contains ~sub:"UINT8 end2;" out)

(* 3D has no spelling for a name that is not an identifier: [a-b] reached
   3d.exe as a syntax error. *)
let test_3d_names_are_identifiers () =
  let module_of s = module_ [ typedef ~entrypoint:true s ] in
  List.iter
    (fun name ->
      invalid_arg_mentions name
        [ Fmt.str "%S" name; "identifier" ]
        (fun () -> to_3d (module_of (struct_ "Kw" [ field name uint8 ])));
      invalid_arg_mentions (name ^ " (parameter)")
        [ Fmt.str "%S" name; "identifier" ]
        (fun () ->
          to_3d
            (module_of
               (param_struct "Kw" [ param name uint8 ] [ field "x" uint8 ]))))
    [ "a-b"; "a b"; "a.b"; "1a"; ""; "é" ];
  let out =
    to_3d (module_of (struct_ "Kw" [ field "_a1" uint8; field "A_b" uint8 ]))
  in
  Alcotest.(check bool)
    "identifiers project" true
    (contains ~sub:"UINT8 _a1;" out && contains ~sub:"UINT8 A_b;" out)

(* EverParse's C names a field's local after its 3D name less any trailing
   underscore, so [typeof], or [typeof_], escaped the 3D way still reached C as
   GNU C's [typeof] keyword. Both are spelt [typeof_3d], and so clash. *)
let test_3d_typeof_reaches_c_escaped () =
  let module_of s = module_ [ typedef ~entrypoint:true s ] in
  let f = field "typeof" uint8 in
  let s =
    struct_ "T" [ f; field "y" ~constraint_:Expr.(field_ref f <= int 3) uint8 ]
  in
  let out = to_3d (module_of s) in
  Alcotest.(check bool)
    "typeof escaped" true
    (contains ~sub:"UINT8 typeof_3d;" out);
  Alcotest.(check bool)
    "escape read" true
    (contains ~sub:"(typeof_3d <= 3)" out);
  invalid_arg_mentions "typeof and typeof_" [ "\"typeof\""; "\"typeof_\"" ]
    (fun () ->
      to_3d
        (module_of
           (struct_ "T" [ field "typeof" uint8; field "typeof_" uint8 ])))

(* The 3D a codec projects to, with its enums declared as types, as the
   EverParse pipeline writes it. *)
let codec_3d c =
  to_3d ~enum_as_type:true (Everparse.project c).Everparse.module_

let one_field_codec name typ =
  let f = Field.v "k" typ in
  Codec.v name Fun.id Codec.[ f $ Fun.id ]

(* An enum's type and case names are 3D declarations like any other: a keyword
   among them is escaped the way a field's is, wherever it is written. *)
let test_3d_enum_keyword_names () =
  let out =
    codec_3d (one_field_codec "Kw" (enum "end" [ ("in", 1); ("of", 2) ] uint8))
  in
  Alcotest.(check bool) "type escaped" true (contains ~sub:"enum end_ {" out);
  Alcotest.(check bool)
    "cases escaped" true
    (contains ~sub:"in_ = 1" out && contains ~sub:"of_ = 2" out);
  Alcotest.(check bool)
    "field typed as escape" true
    (contains ~sub:"end_ k;" out);
  let kw = enum "end" [ ("in", 1) ] uint8 in
  let out = codec_3d (one_field_codec "Arr" (array ~len:(int 2) kw)) in
  Alcotest.(check bool)
    "array names the escape" true
    (contains ~sub:"end_ k[" out)

(* Enum cases live in the module's namespace, beside its types: two cases that
   escape onto each other, or two enums of one codec naming the same case, are
   one 3D declaration twice, which EverParse rejects. *)
let test_3d_enum_case_clash () =
  invalid_arg_mentions "end and end_" [ "\"end\""; "\"end_\"" ] (fun () ->
      codec_3d
        (one_field_codec "Cl" (enum "E" [ ("end", 1); ("end_", 2) ] uint8)));
  let a = Field.v "a" (enum "Color" [ ("Red", 1) ] uint8) in
  let b = Field.v "b" (enum "Fruit" [ ("Red", 3) ] uint8) in
  invalid_arg_mentions "Red in two enums" [ "\"Color\""; "\"Fruit\""; "Red" ]
    (fun () ->
      codec_3d (Codec.v "Two" (fun a b -> (a, b)) Codec.[ a $ fst; b $ snd ]));
  invalid_arg_mentions "case named as the struct" [ "\"Two\"" ] (fun () ->
      codec_3d (one_field_codec "Two" (enum "Kind" [ ("Two", 1) ] uint8)));
  invalid_arg_mentions "case named as its enum" [ "\"end\"" ] (fun () ->
      codec_3d (one_field_codec "Kw" (enum "end" [ ("end", 1) ] uint8)))

(* An enum used only inside a sub-codec is declared like one the entrypoint
   names; the 3D otherwise referred to a type it never declared. Its cases share
   the module's namespace with the entrypoint's. *)
let test_3d_nested_enum_declared () =
  let inner = one_field_codec "Inner" (enum "Color" [ ("Red", 1) ] uint8) in
  let f = Field.v "i" (codec inner) in
  let out = codec_3d (Codec.v "Outer" Fun.id Codec.[ f $ Fun.id ]) in
  Alcotest.(check bool) "enum declared" true (contains ~sub:"enum Color {" out);
  let a = Field.v "a" (enum "Color" [ ("Red", 1) ] uint8) in
  let other = one_field_codec "Other" (enum "Fruit" [ ("Red", 3) ] uint8) in
  let b = Field.v "b" (codec other) in
  invalid_arg_mentions "Red in an enum and a sub-codec's enum"
    [ "\"Color\""; "\"Fruit\"" ] (fun () ->
      codec_3d (Codec.v "Mix" (fun a b -> (a, b)) Codec.[ a $ fst; b $ snd ]));
  (* An enum over a [where]-refined base declares its bare base: 3D has no
     refined enum base, and [UINT8 { e } enum E] is a syntax error. The
     refinement stays on the field. *)
  let kind = enum "Kind" [ ("A", 1) ] (where Expr.(int 0 = int 0) uint8) in
  let inner = one_field_codec "InnerW" kind in
  let f = Field.v "i" (codec inner) in
  let out = codec_3d (Codec.v "OuterW" Fun.id Codec.[ f $ Fun.id ]) in
  Alcotest.(check bool)
    "bare base declared" true
    (contains ~sub:"UINT8 enum Kind {" out);
  Alcotest.(check bool) "where on the field" true (contains ~sub:"(0 == 0)" out)

(* Reversing a bit group into its base's native order pads the word with an
   anonymous field, which 3D needs a name for. The name it gets must not be a
   field's. *)
let test_3d_padding_name_is_fresh () =
  let a = Field.v "_anon_0" uint8 in
  let b = Field.v "b" (bits ~width:3 U8) in
  let out =
    codec_3d (Codec.v "Pad" (fun a b -> (a, b)) Codec.[ a $ fst; b $ snd ])
  in
  Alcotest.(check bool) "field kept" true (contains ~sub:"UINT8 _anon_0;" out);
  Alcotest.(check bool)
    "padding named apart" false
    (contains ~sub:"_anon_0 :" out)

let suite =
  ( "everparse_names",
    [
      Alcotest.test_case "3d: casetype name collision" `Quick
        test_3d_casetype_name_collision;
      Alcotest.test_case "3d: reserved word escaping" `Quick
        test_reserved_word_escaping;
      Alcotest.test_case "3d: duplicate declaration names rejected" `Quick
        test_duplicate_declaration_names_rejected;
      Alcotest.test_case "3d: F* and 3D keywords are escaped" `Quick
        test_reserved_word_escaping_fstar;
      Alcotest.test_case "3d: names that escape onto each other" `Quick
        test_3d_escaped_names_clash;
      Alcotest.test_case "3d: names are identifiers" `Quick
        test_3d_names_are_identifiers;
      Alcotest.test_case "3d: typeof reaches C escaped" `Quick
        test_3d_typeof_reaches_c_escaped;
      Alcotest.test_case "3d: enum keywords are escaped" `Quick
        test_3d_enum_keyword_names;
      Alcotest.test_case "3d: enum cases that clash" `Quick
        test_3d_enum_case_clash;
      Alcotest.test_case "3d: padding takes a fresh name" `Quick
        test_3d_padding_name_is_fresh;
      Alcotest.test_case "3d: an enum in a sub-codec is declared" `Quick
        test_3d_nested_enum_declared;
    ] )
