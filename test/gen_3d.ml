(* Generate .3d files for EverParse integration tests *)

open D3t

let bitfields () =
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 bf_uint32);
        field "y"
          ~constraint_:Expr.(ref "y" <= int 900)
          (bits ~width:10 bf_uint32);
        field "z"
          ~constraint_:Expr.(ref "y" + ref "z" <= int 60000)
          (bits ~width:16 bf_uint32);
      ]
  in
  let bf2 =
    param_struct "BF2"
      [ mutable_param "outx" uint32 ]
      [
        field "x"
          ~action:
            (on_success [ assign "outx" (ref "x"); return_bool (bool true) ])
          (type_ref "BF");
      ]
  in
  let bf3 =
    struct_ "BF3"
      [
        field "a" uint8;
        field "x" ~constraint_:Expr.(ref "x" = int 0) (bits ~width:6 bf_uint32);
        field "y" (bits ~width:10 bf_uint32);
        field "z" (bits ~width:16 bf_uint32);
      ]
  in
  module_ "Bitfields" [ typedef bf; typedef ~entrypoint:true bf2; typedef bf3 ]

let enumerations () =
  module_ "Enumerations"
    [
      enum_decl "Enum8" [ ("Enum8_1", 0); ("Enum8_2", 1); ("Enum8_3", 2) ] uint8;
      enum_decl "Enum32"
        [
          ("Enum32_0", 0); ("Enum32_1", 1); ("Enum32_2", 2); ("Enum32_Next", 100);
        ]
        uint32;
      typedef ~entrypoint:true
        (struct_ "Dummy"
           [ field "e_8" (type_ref "Enum8"); field "e_32" (type_ref "Enum32") ]);
    ]

let field_dependence () =
  let t_struct = param_struct "t" [ param "a" uint32 ] [ field "x" uint32 ] in
  let s_struct =
    struct_ "s"
      [ field "a" uint32; field "b" (apply (type_ref "t") [ ref "a" ]) ]
  in
  let d_casetype =
    casetype_decl "_D"
      [ param "key" uint32 ]
      uint32
      [
        decl_case 1 uint16;
        decl_case 2 uint32;
        decl_case 3 all_bytes;
        decl_case 4 all_zeros;
      ]
  in
  let s2 =
    struct_ "s2"
      [
        field "nondep" uint16;
        field "length" uint32;
        field "key" uint32;
        field "pl"
          (single_elem_array_at_most ~size:(ref "length")
             (apply (type_ref "D") [ ref "key" ]));
        field "pl_array2"
          (single_elem_array ~size:(ref "length")
             (apply (type_ref "D") [ ref "key" ]));
        field "payload" (apply (type_ref "D") [ ref "key" ]);
      ]
  in
  module_ "FieldDependence"
    [
      typedef t_struct;
      typedef ~entrypoint:true s_struct;
      d_casetype;
      typedef ~entrypoint:true s2;
    ]

let () =
  to_3d_file "Bitfields.3d" (bitfields ());
  to_3d_file "Enumerations.3d" (enumerations ());
  to_3d_file "FieldDependence.3d" (field_dependence ())
