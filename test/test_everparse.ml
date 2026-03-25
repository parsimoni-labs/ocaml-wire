(* Tests for c.ml: 3D generation and projection-facing syntax. *)

open Wire
open Wire.Everparse.Raw

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let test_bitfields () =
  let f_y = field "y" (bits ~width:10 U32) in
  let f_z = field "z" (bits ~width:16 U32) in
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 U32);
        field "y"
          ~constraint_:Expr.(field_ref f_y <= int 900)
          (bits ~width:10 U32);
        field "z"
          ~constraint_:Expr.(field_ref f_y + field_ref f_z <= int 60000)
          (bits ~width:16 U32);
      ]
  in
  let m = module_ [ typedef bf ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains BF" true (contains ~sub:"BF" output)

let test_enumerations () =
  let m =
    module_
      [
        enum_decl "Enum8"
          [ ("Enum8_1", 0); ("Enum8_2", 1); ("Enum8_3", 2) ]
          uint8;
      ]
  in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains enum" true (contains ~sub:"enum" output);
  Alcotest.(check bool) "contains Enum8_1" true (contains ~sub:"Enum8_1" output)

let test_field_dependence () =
  let t_struct = param_struct "t" [ param "a" uint32 ] [ field "x" uint32 ] in
  let f_a = field "a" uint32 in
  let s_struct =
    struct_ "s"
      [ field "a" uint32; field "b" (apply (type_ref "t") [ field_ref f_a ]) ]
  in
  let m = module_ [ typedef t_struct; typedef s_struct ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains param" true (contains ~sub:"UINT32 a" output)

let test_casetype () =
  let d_casetype =
    casetype_decl "_D"
      [ param "key" uint32 ]
      uint32
      [ decl_case 1 uint16; decl_case 2 uint32 ]
  in
  let m = module_ [ d_casetype ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool)
    "contains casetype" true
    (contains ~sub:"casetype" output);
  Alcotest.(check bool)
    "contains switch" true
    (contains ~sub:"switch (key)" output);
  Alcotest.(check bool) "public name is D" true (contains ~sub:"} D;" output)

let test_pretty_print () =
  let simple =
    struct_ "Simple" [ field "a" uint8; field "b" uint16be; field "c" uint32 ]
  in
  let m = module_ [ typedef simple ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains typedef" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output)

let suite =
  ( "c",
    [
      Alcotest.test_case "generation: bitfields" `Quick test_bitfields;
      Alcotest.test_case "generation: enumerations" `Quick test_enumerations;
      Alcotest.test_case "generation: field dependence" `Quick
        test_field_dependence;
      Alcotest.test_case "generation: casetype" `Quick test_casetype;
      Alcotest.test_case "generation: pretty print" `Quick test_pretty_print;
    ] )
