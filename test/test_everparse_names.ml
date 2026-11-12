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

let suite =
  ( "everparse_names",
    [
      Alcotest.test_case "3d: casetype name collision" `Quick
        test_3d_casetype_name_collision;
      Alcotest.test_case "3d: reserved word escaping" `Quick
        test_reserved_word_escaping;
      Alcotest.test_case "3d: duplicate declaration names rejected" `Quick
        test_duplicate_declaration_names_rejected;
    ] )
