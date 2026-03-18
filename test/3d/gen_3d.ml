(* Generate .3d files for EverParse integration tests *)

open Wire
open Wire.C

let bitfields () =
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 U32);
        field "y"
          ~constraint_:Expr.(field_ref "y" <= int 900)
          (bits ~width:10 U32);
        field "z"
          ~constraint_:Expr.(field_ref "y" + field_ref "z" <= int 60000)
          (bits ~width:16 U32);
      ]
  in
  let bf2 =
    param_struct "BF2"
      [ mutable_param "outx" uint32 ]
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.assign "outx" (field_ref "x");
                 Action.return_bool Expr.true_;
               ])
          (type_ref "BF");
      ]
  in
  let bf3 =
    struct_ "BF3"
      [
        field "a" uint8;
        field "x" ~constraint_:Expr.(field_ref "x" = int 0) (bits ~width:6 U32);
        field "y" (bits ~width:10 U32);
        field "z" (bits ~width:16 U32);
      ]
  in
  module_ [ typedef bf; typedef ~entrypoint:true bf2; typedef bf3 ]

let enumerations () =
  module_
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
      [ field "a" uint32; field "b" (apply (type_ref "t") [ field_ref "a" ]) ]
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
          (single_elem_array_at_most ~size:(field_ref "length")
             (apply (type_ref "D") [ field_ref "key" ]));
        field "pl_array2"
          (single_elem_array ~size:(field_ref "length")
             (apply (type_ref "D") [ field_ref "key" ]));
        field "payload" (apply (type_ref "D") [ field_ref "key" ]);
      ]
  in
  module_
    [
      typedef t_struct;
      typedef ~entrypoint:true s_struct;
      d_casetype;
      typedef ~entrypoint:true s2;
    ]

let gen_struct name s =
  let m = module_ [ typedef ~entrypoint:true s ] in
  to_3d_file (name ^ ".3d") m

let () =
  to_3d_file "Bitfields.3d" (bitfields ());
  to_3d_file "Enumerations.3d" (enumerations ());
  to_3d_file "FieldDependence.3d" (field_dependence ());

  (* Demo schemas *)
  gen_struct "Minimal" Demo.minimal_struct;
  gen_struct "AllInts" Demo.all_ints_struct;
  gen_struct "Bitfield8" Demo.bf8_struct;
  gen_struct "Bitfield16" Demo.bf16_struct;
  gen_struct "Bitfield32" Demo.bf32_struct;
  gen_struct "BoolFields" Demo.bool_fields_struct;
  gen_struct "LargeMixed" Demo.large_mixed_struct;

  (* Space schemas *)
  gen_struct "CLCW" Space.clcw_struct;
  gen_struct "SpacePacket" Space.packet_struct;
  gen_struct "TMFrame" Space.tm_frame_struct;

  (* Net schemas — also exercises all_schemas and all_structs *)
  List.iter (fun s -> gen_struct (struct_name s) s) Net.all_structs;
  assert (List.length Net.all_schemas = List.length Net.all_structs)
