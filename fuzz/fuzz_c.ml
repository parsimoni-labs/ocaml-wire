(** Fuzz tests for 3D/C projection: pretty-printing and code generation. *)

open Alcobar

(** {1 Pretty-printing Tests} *)

let test_pp_uint8 () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint8 in
  ()

let test_pp_uint16 () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint16 in
  ()

let test_pp_uint16be () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint16be in
  ()

let test_pp_uint32 () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint32 in
  ()

let test_pp_uint32be () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint32be in
  ()

let test_pp_uint63 () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint63 in
  ()

let test_pp_uint63be () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint63be in
  ()

let test_pp_uint64 () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint64 in
  ()

let test_pp_uint64be () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.uint64be in
  ()

let test_pp_bitfield width =
  if width > 0 && width <= 32 then begin
    let t = Wire.bits ~width Wire.U32 in
    let _ = Fmt.str "%a" Wire.C.pp_typ t in
    ()
  end

let test_pp_bf_uint8 width =
  let width = (abs width mod 8) + 1 in
  let t = Wire.bits ~width Wire.U8 in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_bf_uint16 width =
  let width = (abs width mod 16) + 1 in
  let t = Wire.bits ~width Wire.U16 in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_bf_uint16be width =
  let width = (abs width mod 16) + 1 in
  let t = Wire.bits ~width Wire.U16be in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_bf_uint32be width =
  let width = (abs width mod 32) + 1 in
  let t = Wire.bits ~width Wire.U32be in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_map () =
  let t =
    Wire.map ~decode:(fun n -> n * 2) ~encode:(fun n -> n / 2) Wire.uint8
  in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_bool () =
  let t = Wire.bool (Wire.bits ~width:1 Wire.U8) in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_variants () =
  let t =
    Wire.variants "Test" [ ("A", "a"); ("B", "b"); ("C", "c") ] Wire.uint8
  in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_unit () =
  let _ = Fmt.str "%a" Wire.C.pp_typ Wire.empty in
  ()

(** Test pp_module doesn't crash on valid modules. *)
let test_pp_module_simple () =
  let s =
    Wire.C.struct_ "Test"
      [ Wire.C.field "a" Wire.uint8; Wire.C.field "b" Wire.uint16 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** {1 3D Code Generation Tests} *)

(** Test struct with random field count. *)
let test_struct_random_fields n =
  let n = (n mod 10) + 1 in
  let fields =
    List.init n (fun i -> Wire.C.field (Fmt.str "f%d" i) Wire.uint8)
  in
  let s = Wire.C.struct_ "Random" fields in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test enum with random cases. *)
let test_enum_random_cases n =
  let n = (n mod 10) + 1 in
  let cases = List.init n (fun i -> (Fmt.str "C%d" i, i)) in
  let e = Wire.C.enum_decl "RandEnum" cases Wire.uint8 in
  let m = Wire.C.module_ [ e ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test casetype with random cases. *)
let test_casetype_random n =
  let n = (n mod 5) + 1 in
  let cases = List.init n (fun i -> Wire.C.decl_case i Wire.uint8) in
  let ct =
    Wire.C.casetype_decl "_RandCase"
      [ Wire.C.param "tag" Wire.uint8 ]
      Wire.uint8 cases
  in
  let m = Wire.C.module_ [ ct ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test inline casetype. *)
let test_casetype_inline () =
  let t =
    Wire.casetype "Tag" Wire.uint8
      [
        Wire.case 0 Wire.uint16;
        Wire.case 1 Wire.uint32;
        Wire.default Wire.uint8;
      ]
  in
  let s =
    Wire.C.struct_ "WithCase"
      [ Wire.C.field "tag" Wire.uint8; Wire.C.field "data" t ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test constraint expression generation. *)
let test_constraint_expr a =
  let v = abs a mod 1000 in
  let f_x = Wire.C.field "x" Wire.uint16 in
  let cond = Wire.Expr.(Wire.C.field_ref f_x <= Wire.int v) in
  let s =
    Wire.C.struct_ "Constrained"
      [ Wire.C.field "x" ~constraint_:cond Wire.uint16 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test bitfield constraints. *)
let test_bitfield_constraint width =
  let width = (width mod 16) + 1 in
  let t = Wire.bits ~width Wire.U16 in
  let f_x = Wire.C.field "x" t in
  let cond = Wire.Expr.(Wire.C.field_ref f_x <= Wire.int 100) in
  let s =
    Wire.C.struct_ "BFConstrained" [ Wire.C.field "x" ~constraint_:cond t ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test bitwise expression operators in 3D output. *)
let test_bitwise_expr a =
  let v = abs a mod 256 in
  let f_x = Wire.C.field "x" Wire.uint16 in
  let open Wire.Expr in
  let cond = Wire.C.field_ref f_x land Wire.int 0xFF <= Wire.int v in
  let s =
    Wire.C.struct_ "Bitwise" [ Wire.C.field "x" ~constraint_:cond Wire.uint16 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test logical expression operators. *)
let test_logical_expr () =
  let f_x = Wire.C.field "x" Wire.uint8 in
  let open Wire.Expr in
  let cond =
    Wire.C.field_ref f_x <= Wire.int 100 && Wire.C.field_ref f_x >= Wire.int 0
  in
  let s =
    Wire.C.struct_ "Logical" [ Wire.C.field "x" ~constraint_:cond Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test all bitwise/shift operators. *)
let test_bitwise_ops () =
  let f_x = Wire.C.field "x" Wire.uint8 in
  let open Wire.Expr in
  let _ = Wire.C.field_ref f_x lor Wire.int 1 in
  let _ = Wire.C.field_ref f_x lxor Wire.int 0xFF in
  let _ = lnot (Wire.C.field_ref f_x) in
  let _ = Wire.C.field_ref f_x lsl Wire.int 2 in
  let _ = Wire.C.field_ref f_x lsr Wire.int 3 in
  ()

(** Test logical operators. *)
let test_logical_ops () =
  let f_x = Wire.C.field "x" Wire.uint8 in
  let open Wire.Expr in
  let _ = Wire.Expr.true_ || Wire.Expr.false_ in
  let _ = Wire.Expr.not Wire.Expr.true_ in
  let _ =
    Wire.C.field_ref f_x = Wire.int 0 || Wire.C.field_ref f_x <> Wire.int 1
  in
  ()

(** Test cast operators in 3D output. *)
let test_cast_expr () =
  let f_x = Wire.C.field "x" Wire.uint16 in
  let open Wire.Expr in
  let cond = to_uint8 (Wire.C.field_ref f_x) <= Wire.int 100 in
  let s =
    Wire.C.struct_ "Cast" [ Wire.C.field "x" ~constraint_:cond Wire.uint16 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test all cast variants. *)
let test_cast_variants () =
  let open Wire.Expr in
  let _ = to_uint8 (Wire.int 42) in
  let _ = to_uint16 (Wire.int 42) in
  let _ = to_uint32 (Wire.int 42) in
  let _ = to_uint64 (Wire.int 42) in
  ()

(** Test sizeof expression. *)
let test_sizeof_expr () =
  let _ = Wire.sizeof Wire.uint32 in
  let _ = Wire.sizeof_this in
  let _ = Wire.field_pos in
  ()

(** Test array types. *)
let test_array_type len =
  let len = (abs len mod 100) + 1 in
  let arr = Wire.array ~len:(Wire.int len) Wire.uint8 in
  let s = Wire.C.struct_ "WithArray" [ Wire.C.field "arr" arr ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test byte_array types. *)
let test_byte_array size =
  let size = (abs size mod 1000) + 1 in
  let ba = Wire.byte_array ~size:(Wire.int size) in
  let s = Wire.C.struct_ "WithByteArray" [ Wire.C.field "data" ba ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test nested. *)
let test_nested () =
  let t = Wire.nested ~size:(Wire.int 4) Wire.uint32 in
  let s = Wire.C.struct_ "WithSingle" [ Wire.C.field "x" t ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test nested_at_most. *)
let test_single_elem_at_most () =
  let t = Wire.nested_at_most ~size:(Wire.int 8) Wire.uint32 in
  let s = Wire.C.struct_ "WithAtMost" [ Wire.C.field "x" t ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test anon_field (padding). *)
let test_anon_field () =
  let s =
    Wire.C.struct_ "WithPadding"
      [
        Wire.C.field "x" Wire.uint8;
        Wire.C.anon_field Wire.uint8;
        Wire.C.field "y" Wire.uint16;
      ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test parameterized struct. *)
let test_param_struct n =
  let n = (n mod 5) + 1 in
  let params =
    List.init n (fun i -> Wire.C.param (Fmt.str "p%d" i) Wire.uint32)
  in
  let ps =
    Wire.C.param_struct "Parametric" params [ Wire.C.field "x" Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef ps ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test mutable_param. *)
let test_mutable_param () =
  let ps =
    Wire.C.param_struct "MutParam"
      [ Wire.C.mutable_param "out" Wire.uint32 ]
      [ Wire.C.field "x" Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef ps ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test apply (parameterized type application). *)
let test_apply () =
  let t = Wire.C.apply (Wire.C.type_ref "Param") [ Wire.int 42 ] in
  let s = Wire.C.struct_ "WithApply" [ Wire.C.field "x" t ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test type_ref. *)
let test_type_ref () =
  let t : int Wire.typ = Wire.C.type_ref "SomeType" in
  let s = Wire.C.struct_ "WithRef" [ Wire.C.field "x" t ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test qualified_ref. *)
let test_qualified_ref () =
  let t : int Wire.typ = Wire.C.qualified_ref "Other" "SomeType" in
  let s = Wire.C.struct_ "WithQRef" [ Wire.C.field "x" t ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test action generation. *)
let test_action () =
  let ptr = Wire.Param.output "ptr" Wire.uint32 in
  let act =
    Wire.Action.on_success
      [
        Wire.Action.assign ptr (Wire.int 42);
        Wire.Action.return_bool Wire.Expr.true_;
      ]
  in
  let s =
    Wire.C.param_struct "WithAction"
      [ Wire.Param.v ptr ]
      [ Wire.C.field "x" ~action:act Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test Action.on_act action. *)
let test_on_act () =
  let ptr = Wire.Param.output "ptr" Wire.uint32 in
  let act = Wire.Action.on_act [ Wire.Action.assign ptr (Wire.int 0) ] in
  let s =
    Wire.C.param_struct "WithOnAct"
      [ Wire.Param.v ptr ]
      [ Wire.C.field "x" ~action:act Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test Action.abort action. *)
let test_abort () =
  let act = Wire.Action.on_success [ Wire.Action.abort ] in
  let s =
    Wire.C.struct_ "WithAbort" [ Wire.C.field "x" ~action:act Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test Action.if_. *)
let test_action_if () =
  let ptr = Wire.Param.output "ptr" Wire.uint32 in
  let f_x = Wire.C.field "x" Wire.uint8 in
  let stmt =
    Wire.Action.if_
      Wire.Expr.(Wire.C.field_ref f_x > Wire.int 10)
      [ Wire.Action.assign ptr (Wire.int 1) ]
      (Some [ Wire.Action.assign ptr (Wire.int 0) ])
  in
  let act = Wire.Action.on_success [ stmt ] in
  let s =
    Wire.C.param_struct "WithIf"
      [ Wire.Param.v ptr ]
      [ Wire.C.field "x" ~action:act Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test Action.var action statement. *)
let test_var () =
  let ptr = Wire.Param.output "ptr" Wire.uint32 in
  let f_tmp = Wire.C.field "tmp" Wire.uint32 in
  let act =
    Wire.Action.on_success
      [
        Wire.Action.var "tmp" (Wire.int 42);
        Wire.Action.assign ptr (Wire.C.field_ref f_tmp);
      ]
  in
  let s =
    Wire.C.param_struct "WithVar"
      [ Wire.Param.v ptr ]
      [ Wire.C.field "x" ~action:act Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test define declaration. *)
let test_define () =
  let m =
    Wire.C.module_
      [
        Wire.C.define "MAX_SIZE" 1024;
        Wire.C.typedef (Wire.C.struct_ "S" [ Wire.C.field "x" Wire.uint8 ]);
      ]
  in
  let _ = Wire.C.to_3d m in
  ()

(** Test extern_fn declaration. *)
let test_extern_fn () =
  let m =
    Wire.C.module_
      [
        Wire.C.extern_fn "validate"
          [ Wire.C.param "len" Wire.uint32 ]
          Wire.uint8;
        Wire.C.typedef (Wire.C.struct_ "S" [ Wire.C.field "x" Wire.uint8 ]);
      ]
  in
  let _ = Wire.C.to_3d m in
  ()

(** Test extern_probe declaration. *)
let test_extern_probe () =
  let m =
    Wire.C.module_
      [
        Wire.C.extern_probe "my_probe";
        Wire.C.extern_probe ~init:true "my_init_probe";
        Wire.C.typedef (Wire.C.struct_ "S" [ Wire.C.field "x" Wire.uint8 ]);
      ]
  in
  let _ = Wire.C.to_3d m in
  ()

(** Test complex nested structure. *)
let test_complex_nested () =
  let inner = Wire.C.struct_ "Inner" [ Wire.C.field "a" Wire.uint8 ] in
  let outer =
    Wire.C.struct_ "Outer"
      [
        Wire.C.field "i" (Wire.C.struct_typ inner); Wire.C.field "b" Wire.uint16;
      ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef inner; Wire.C.typedef outer ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test big-endian struct with all BE types. *)
let test_be_struct () =
  let s =
    Wire.C.struct_ "BigEndian"
      [
        Wire.C.field "a" Wire.uint16be;
        Wire.C.field "b" Wire.uint32be;
        Wire.C.field "c" Wire.uint64be;
        Wire.C.field "d" Wire.uint63be;
      ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** {1 Test Registration} *)

let pp_tests =
  [
    test_case "pp uint8" [ const () ] test_pp_uint8;
    test_case "pp uint16" [ const () ] test_pp_uint16;
    test_case "pp uint16be" [ const () ] test_pp_uint16be;
    test_case "pp uint32" [ const () ] test_pp_uint32;
    test_case "pp uint32be" [ const () ] test_pp_uint32be;
    test_case "pp uint63" [ const () ] test_pp_uint63;
    test_case "pp uint63be" [ const () ] test_pp_uint63be;
    test_case "pp uint64" [ const () ] test_pp_uint64;
    test_case "pp uint64be" [ const () ] test_pp_uint64be;
    test_case "pp bitfield" [ range 33 ] test_pp_bitfield;
    test_case "pp U8" [ int ] test_pp_bf_uint8;
    test_case "pp U16" [ int ] test_pp_bf_uint16;
    test_case "pp U16be" [ int ] test_pp_bf_uint16be;
    test_case "pp U32be" [ int ] test_pp_bf_uint32be;
    test_case "pp map" [ const () ] test_pp_map;
    test_case "pp bool" [ const () ] test_pp_bool;
    test_case "pp cases" [ const () ] test_pp_variants;
    test_case "pp unit" [ const () ] test_pp_unit;
    test_case "pp module" [ const () ] test_pp_module_simple;
  ]

let codegen_tests =
  [
    test_case "struct random fields" [ range 100 ] test_struct_random_fields;
    test_case "enum random cases" [ range 100 ] test_enum_random_cases;
    test_case "casetype random" [ range 100 ] test_casetype_random;
    test_case "casetype inline" [ const () ] test_casetype_inline;
    test_case "constraint expr" [ int ] test_constraint_expr;
    test_case "bitfield constraint" [ range 32 ] test_bitfield_constraint;
    test_case "bitwise expr" [ int ] test_bitwise_expr;
    test_case "logical expr" [ const () ] test_logical_expr;
    test_case "bitwise ops" [ const () ] test_bitwise_ops;
    test_case "logical ops" [ const () ] test_logical_ops;
    test_case "cast expr" [ const () ] test_cast_expr;
    test_case "cast variants" [ const () ] test_cast_variants;
    test_case "sizeof expr" [ const () ] test_sizeof_expr;
    test_case "array type" [ int ] test_array_type;
    test_case "byte array" [ int ] test_byte_array;
    test_case "nested" [ const () ] test_nested;
    test_case "nested at most" [ const () ] test_single_elem_at_most;
    test_case "anon field" [ const () ] test_anon_field;
    test_case "param struct" [ range 20 ] test_param_struct;
    test_case "mutable param" [ const () ] test_mutable_param;
    test_case "apply" [ const () ] test_apply;
    test_case "type ref" [ const () ] test_type_ref;
    test_case "qualified ref" [ const () ] test_qualified_ref;
    test_case "action" [ const () ] test_action;
    test_case "Action.on_act" [ const () ] test_on_act;
    test_case "Action.abort" [ const () ] test_abort;
    test_case "Action.if_" [ const () ] test_action_if;
    test_case "Action.var" [ const () ] test_var;
    test_case "define" [ const () ] test_define;
    test_case "extern_fn" [ const () ] test_extern_fn;
    test_case "extern_probe" [ const () ] test_extern_probe;
    test_case "complex nested" [ const () ] test_complex_nested;
    test_case "be struct" [ const () ] test_be_struct;
  ]

let suite = ("c", pp_tests @ codegen_tests)
