(** Fuzz tests for wire library.

    Tests cover: pretty-printer crash safety, parse crash safety on arbitrary
    input, encode-then-parse roundtrip correctness, record codec roundtrip, and
    3D code generation for all DSL combinators. *)

open Crowbar
open Crowbar_util

(* Silence unused variable warnings for parse error handling *)
let _ = Wire.pp_parse_error

(* Helper: encode record to string using Codec API *)
let encode_record_to_string codec v =
  let ws = Wire.Codec.wire_size codec in
  let buf = Bytes.create ws in
  Wire.Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let decode_record_from_string codec s =
  let ws = Wire.Codec.wire_size codec in
  if String.length s < ws then
    Error (Wire.Unexpected_eof { expected = ws; got = String.length s })
  else Ok (Wire.Codec.decode codec (Bytes.of_string s) 0)

(** Truncate input to reasonable size for protocol messages. *)
let truncate buf =
  let max_len = 1024 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

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
  let t = Wire.map (fun n -> n * 2) (fun n -> n / 2) Wire.uint8 in
  let _ = Fmt.str "%a" Wire.C.pp_typ t in
  ()

let test_pp_bool () =
  let t = Wire.to_bool (Wire.bits ~width:1 Wire.U8) in
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
  let cond = Wire.Expr.(Wire.field_ref "x" <= Wire.int v) in
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
  let cond = Wire.Expr.(Wire.field_ref "x" <= Wire.int 100) in
  let s =
    Wire.C.struct_ "BFConstrained" [ Wire.C.field "x" ~constraint_:cond t ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test bitwise expression operators in 3D output. *)
let test_bitwise_expr a =
  let v = abs a mod 256 in
  let open Wire.Expr in
  let cond = Wire.field_ref "x" land Wire.int 0xFF <= Wire.int v in
  let s =
    Wire.C.struct_ "Bitwise" [ Wire.C.field "x" ~constraint_:cond Wire.uint16 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test logical expression operators. *)
let test_logical_expr () =
  let open Wire.Expr in
  let cond =
    Wire.field_ref "x" <= Wire.int 100 && Wire.field_ref "x" >= Wire.int 0
  in
  let s =
    Wire.C.struct_ "Logical" [ Wire.C.field "x" ~constraint_:cond Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test all bitwise/shift operators. *)
let test_bitwise_ops () =
  let open Wire.Expr in
  let _ = Wire.field_ref "x" lor Wire.int 1 in
  let _ = Wire.field_ref "x" lxor Wire.int 0xFF in
  let _ = lnot (Wire.field_ref "x") in
  let _ = Wire.field_ref "x" lsl Wire.int 2 in
  let _ = Wire.field_ref "x" lsr Wire.int 3 in
  ()

(** Test logical operators. *)
let test_logical_ops () =
  let open Wire.Expr in
  let _ = Wire.Expr.true_ || Wire.Expr.false_ in
  let _ = Wire.Expr.not Wire.Expr.true_ in
  let _ = Wire.field_ref "x" = Wire.int 0 || Wire.field_ref "x" <> Wire.int 1 in
  ()

(** Test cast operators in 3D output. *)
let test_cast_expr () =
  let open Wire.Expr in
  let cond = to_uint8 (Wire.field_ref "x") <= Wire.int 100 in
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

(** Test single_elem_array. *)
let test_single_elem_array () =
  let t = Wire.single_elem_array ~size:(Wire.int 4) Wire.uint32 in
  let s = Wire.C.struct_ "WithSingle" [ Wire.C.field "x" t ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test single_elem_array_at_most. *)
let test_single_elem_at_most () =
  let t = Wire.single_elem_array_at_most ~size:(Wire.int 8) Wire.uint32 in
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
  let act =
    Wire.Action.on_success
      [
        Wire.Action.assign "ptr" (Wire.int 42);
        Wire.Action.return_bool Wire.Expr.true_;
      ]
  in
  let s =
    Wire.C.struct_ "WithAction" [ Wire.C.field "x" ~action:act Wire.uint8 ]
  in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test Action.on_act action. *)
let test_on_act () =
  let act = Wire.Action.on_act [ Wire.Action.assign "ptr" (Wire.int 0) ] in
  let s =
    Wire.C.struct_ "WithOnAct" [ Wire.C.field "x" ~action:act Wire.uint8 ]
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
  let stmt =
    Wire.Action.if_
      Wire.Expr.(Wire.field_ref "x" > Wire.int 10)
      [ Wire.Action.assign "ptr" (Wire.int 1) ]
      (Some [ Wire.Action.assign "ptr" (Wire.int 0) ])
  in
  let act = Wire.Action.on_success [ stmt ] in
  let s = Wire.C.struct_ "WithIf" [ Wire.C.field "x" ~action:act Wire.uint8 ] in
  let m = Wire.C.module_ [ Wire.C.typedef s ] in
  let _ = Wire.C.to_3d m in
  ()

(** Test Action.var action statement. *)
let test_var () =
  let act =
    Wire.Action.on_success
      [
        Wire.Action.var "tmp" (Wire.int 42);
        Wire.Action.assign "ptr" (Wire.field_ref "tmp");
      ]
  in
  let s =
    Wire.C.struct_ "WithVar" [ Wire.C.field "x" ~action:act Wire.uint8 ]
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

(** {1 Parsing Tests} *)

let test_parse_uint8 buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint8 buf in
  ()

let test_parse_uint16 buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint16 buf in
  ()

let test_parse_uint16be buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint16be buf in
  ()

let test_parse_uint32 buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint32 buf in
  ()

let test_parse_uint32be buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint32be buf in
  ()

let test_parse_uint63 buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint63 buf in
  ()

let test_parse_uint63be buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint63be buf in
  ()

let test_parse_uint64 buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint64 buf in
  ()

let test_parse_uint64be buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.uint64be buf in
  ()

let test_parse_bitfield buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:6 Wire.U32 in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_bf_uint8 buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:3 Wire.U8 in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_bf_uint16 buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:10 Wire.U16 in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_bf_uint16be buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:10 Wire.U16be in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_bf_uint32be buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:20 Wire.U32be in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_map buf =
  let buf = truncate buf in
  let t = Wire.map (fun n -> n * 2) (fun n -> n / 2) Wire.uint8 in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_bool buf =
  let buf = truncate buf in
  let t = Wire.to_bool Wire.uint8 in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_unit buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.empty buf in
  ()

let test_parse_array buf =
  let buf = truncate buf in
  let len = min 10 (String.length buf) in
  let arr = Wire.array ~len:(Wire.int len) Wire.uint8 in
  let _ = Wire.decode_string arr buf in
  ()

let test_parse_byte_array buf =
  let buf = truncate buf in
  let size = min 10 (String.length buf) in
  let ba = Wire.byte_array ~size:(Wire.int size) in
  let _ = Wire.decode_string ba buf in
  ()

let test_parse_variants buf =
  let buf = truncate buf in
  let e =
    Wire.variants "TestEnum" [ ("A", `A); ("B", `B); ("C", `C) ] Wire.uint8
  in
  let _ = Wire.decode_string e buf in
  ()

let test_parse_where buf =
  let buf = truncate buf in
  let t = Wire.where Wire.Expr.true_ Wire.uint8 in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_all_bytes buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.all_bytes buf in
  ()

let test_parse_all_zeros buf =
  let buf = truncate buf in
  let _ = Wire.decode_string Wire.all_zeros buf in
  ()

let test_parse_struct buf =
  let buf = truncate buf in
  let s =
    Wire.C.struct_ "Test"
      [
        Wire.C.field "a" Wire.uint8;
        Wire.C.field "b" Wire.uint16;
        Wire.C.field "c" Wire.uint32;
      ]
  in
  let t = Wire.C.struct_typ s in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_struct_constrained buf =
  let buf = truncate buf in
  let s =
    Wire.C.struct_ "Constrained"
      [
        Wire.C.field "x"
          ~constraint_:Wire.Expr.(Wire.field_ref "x" <= Wire.int 100)
          Wire.uint8;
      ]
  in
  let t = Wire.C.struct_typ s in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_struct_be buf =
  let buf = truncate buf in
  let s =
    Wire.C.struct_ "BE"
      [
        Wire.C.field "a" Wire.uint16be;
        Wire.C.field "b" Wire.uint32be;
        Wire.C.field "c" Wire.uint64be;
      ]
  in
  let t = Wire.C.struct_typ s in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_struct_bitfields buf =
  let buf = truncate buf in
  let s =
    Wire.C.struct_ "BF"
      [
        Wire.C.field "a" (Wire.bits ~width:3 Wire.U8);
        Wire.C.field "b" (Wire.bits ~width:5 Wire.U8);
        Wire.C.field "c" (Wire.bits ~width:10 Wire.U16);
        Wire.C.field "d" (Wire.bits ~width:6 Wire.U16);
      ]
  in
  let t = Wire.C.struct_typ s in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_anon_field buf =
  let buf = truncate buf in
  let s =
    Wire.C.struct_ "Anon"
      [
        Wire.C.field "x" Wire.uint8;
        Wire.C.anon_field Wire.uint8;
        Wire.C.field "y" Wire.uint16;
      ]
  in
  let t = Wire.C.struct_typ s in
  let _ = Wire.decode_string t buf in
  ()

let test_parse_casetype buf =
  let buf = truncate buf in
  let t =
    Wire.casetype "Tag" Wire.uint8
      [
        Wire.case 0 Wire.uint8;
        Wire.case 1 Wire.uint16;
        Wire.default Wire.uint32;
      ]
  in
  let _ = Wire.decode_string t buf in
  ()

(** {1 Roundtrip Tests} *)

let test_roundtrip_uint8 n =
  let n = abs n mod 256 in
  let encoded = Wire.encode_to_string Wire.uint8 n in
  match Wire.decode_string Wire.uint8 encoded with
  | Ok decoded -> if n <> decoded then fail "uint8 roundtrip mismatch"
  | Error _ -> fail "uint8 roundtrip parse failed"

let test_roundtrip_uint16 n =
  let n = abs n mod 65536 in
  let encoded = Wire.encode_to_string Wire.uint16 n in
  match Wire.decode_string Wire.uint16 encoded with
  | Ok decoded -> if n <> decoded then fail "uint16 roundtrip mismatch"
  | Error _ -> fail "uint16 roundtrip parse failed"

let test_roundtrip_uint16be n =
  let n = abs n mod 65536 in
  let encoded = Wire.encode_to_string Wire.uint16be n in
  match Wire.decode_string Wire.uint16be encoded with
  | Ok decoded -> if n <> decoded then fail "uint16be roundtrip mismatch"
  | Error _ -> fail "uint16be roundtrip parse failed"

let test_roundtrip_uint32 n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = Wire.encode_to_string Wire.uint32 n in
  match Wire.decode_string Wire.uint32 encoded with
  | Ok decoded -> if n <> decoded then fail "uint32 roundtrip mismatch"
  | Error _ -> fail "uint32 roundtrip parse failed"

let test_roundtrip_uint32be n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = Wire.encode_to_string Wire.uint32be n in
  match Wire.decode_string Wire.uint32be encoded with
  | Ok decoded -> if n <> decoded then fail "uint32be roundtrip mismatch"
  | Error _ -> fail "uint32be roundtrip parse failed"

let test_roundtrip_uint63 n =
  let n = abs n in
  let encoded = Wire.encode_to_string Wire.uint63 n in
  match Wire.decode_string Wire.uint63 encoded with
  | Ok decoded -> if n <> decoded then fail "uint63 roundtrip mismatch"
  | Error _ -> fail "uint63 roundtrip parse failed"

let test_roundtrip_uint63be n =
  let n = abs n in
  let encoded = Wire.encode_to_string Wire.uint63be n in
  match Wire.decode_string Wire.uint63be encoded with
  | Ok decoded -> if n <> decoded then fail "uint63be roundtrip mismatch"
  | Error _ -> fail "uint63be roundtrip parse failed"

let test_roundtrip_uint64 n =
  let encoded = Wire.encode_to_string Wire.uint64 n in
  match Wire.decode_string Wire.uint64 encoded with
  | Ok decoded -> if n <> decoded then fail "uint64 roundtrip mismatch"
  | Error _ -> fail "uint64 roundtrip parse failed"

let test_roundtrip_uint64be n =
  let encoded = Wire.encode_to_string Wire.uint64be n in
  match Wire.decode_string Wire.uint64be encoded with
  | Ok decoded -> if n <> decoded then fail "uint64be roundtrip mismatch"
  | Error _ -> fail "uint64be roundtrip parse failed"

let test_roundtrip_map n =
  let n = abs n mod 256 in
  let t = Wire.map (fun x -> x * 2) (fun x -> x / 2) Wire.uint8 in
  let encoded = Wire.encode_to_string t (n * 2) in
  match Wire.decode_string t encoded with
  | Ok decoded -> if n * 2 <> decoded then fail "map roundtrip mismatch"
  | Error _ -> fail "map roundtrip parse failed"

let test_roundtrip_bool n =
  let v = n mod 2 = 0 in
  let t = Wire.to_bool Wire.uint8 in
  let encoded = Wire.encode_to_string t v in
  match Wire.decode_string t encoded with
  | Ok decoded -> if v <> decoded then fail "bool roundtrip mismatch"
  | Error _ -> fail "bool roundtrip parse failed"

let test_roundtrip_array a b c =
  let arr = [ abs a mod 256; abs b mod 256; abs c mod 256 ] in
  let t = Wire.array ~len:(Wire.int 3) Wire.uint8 in
  let encoded = Wire.encode_to_string t arr in
  match Wire.decode_string t encoded with
  | Ok decoded -> if arr <> decoded then fail "array roundtrip mismatch"
  | Error _ -> fail "array roundtrip parse failed"

let test_roundtrip_byte_array buf =
  let buf = truncate buf in
  let len = String.length buf in
  if len > 0 then begin
    let t = Wire.byte_array ~size:(Wire.int len) in
    let encoded = Wire.encode_to_string t buf in
    match Wire.decode_string t encoded with
    | Ok decoded -> if buf <> decoded then fail "byte_array roundtrip mismatch"
    | Error _ -> fail "byte_array roundtrip parse failed"
  end

let test_roundtrip_variants n =
  let n = abs n mod 3 in
  let variants = [| `A; `B; `C |] in
  let t = Wire.variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] Wire.uint8 in
  let v = variants.(n) in
  let encoded = Wire.encode_to_string t v in
  match Wire.decode_string t encoded with
  | Ok decoded -> if v <> decoded then fail "variants roundtrip mismatch"
  | Error _ -> fail "variants roundtrip parse failed"

(** {1 Record Codec Tests} *)

type test_record = { x : int; y : int; z : int }

let test_record_codec =
  Wire.Codec.view "TestRecord"
    (fun x y z -> { x; y; z })
    Wire.Codec.
      [
        Wire.Codec.field "x" Wire.uint8 (fun r -> r.x);
        Wire.Codec.field "y" Wire.uint16 (fun r -> r.y);
        Wire.Codec.field "z" Wire.uint32 (fun r -> r.z);
      ]

let test_record_roundtrip x y z =
  let x = abs x mod 256 in
  let y = abs y mod 65536 in
  let z = z land 0xFFFFFFFF in
  let original = { x; y; z } in
  match encode_record_to_string test_record_codec original with
  | Error _ -> fail "record encode failed"
  | Ok encoded -> (
      match decode_record_from_string test_record_codec encoded with
      | Ok decoded ->
          if original.x <> decoded.x then fail "record x mismatch";
          if original.y <> decoded.y then fail "record y mismatch";
          if original.z <> decoded.z then fail "record z mismatch"
      | Error _ -> fail "record roundtrip decode failed")

let test_record_decode_crash buf =
  let buf = truncate buf in
  let _ = decode_record_from_string test_record_codec buf in
  ()

type be_record = { a : int; b : int }
(** Record codec with big-endian fields. *)

let be_record_codec =
  Wire.Codec.view "BERecord"
    (fun a b -> { a; b })
    Wire.Codec.
      [
        Wire.Codec.field "a" Wire.uint16be (fun r -> r.a);
        Wire.Codec.field "b" Wire.uint32be (fun r -> r.b);
      ]

let test_record_be_roundtrip a b =
  let a = abs a mod 65536 in
  let b = b land 0xFFFFFFFF in
  let original = { a; b } in
  match encode_record_to_string be_record_codec original with
  | Error _ -> fail "be record encode failed"
  | Ok encoded -> (
      match decode_record_from_string be_record_codec encoded with
      | Ok decoded ->
          if original.a <> decoded.a then fail "be record a mismatch";
          if original.b <> decoded.b then fail "be record b mismatch"
      | Error _ -> fail "be record roundtrip decode failed")

type bool_record = { flag : bool; value : int }
(** Record codec with bool/map fields. *)

let bool_record_codec =
  Wire.Codec.view "BoolRecord"
    (fun flag value -> { flag; value })
    Wire.Codec.
      [
        Wire.Codec.field "flag" (Wire.to_bool Wire.uint8) (fun r -> r.flag);
        Wire.Codec.field "value" Wire.uint16 (fun r -> r.value);
      ]

let test_record_bool_roundtrip n =
  let flag = n mod 2 = 0 in
  let value = abs n mod 65536 in
  let original = { flag; value } in
  match encode_record_to_string bool_record_codec original with
  | Error _ -> fail "bool record encode failed"
  | Ok encoded -> (
      match decode_record_from_string bool_record_codec encoded with
      | Ok decoded ->
          if original.flag <> decoded.flag then fail "bool record flag mismatch";
          if original.value <> decoded.value then
            fail "bool record value mismatch"
      | Error _ -> fail "bool record roundtrip decode failed")

(** {1 Streaming: cross-slice boundary roundtrips} *)

(* Parse from a chunked reader — forces multi-byte values to straddle slices *)
let parse_chunked ~chunk_size typ s =
  let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:chunk_size s in
  Wire.decode typ reader

let test_stream_roundtrip_uint16 n =
  let n = abs n mod 65536 in
  let encoded = Wire.encode_to_string Wire.uint16 n in
  match parse_chunked ~chunk_size:1 Wire.uint16 encoded with
  | Ok decoded -> if n <> decoded then fail "stream uint16 mismatch"
  | Error _ -> fail "stream uint16 parse failed"

let test_stream_roundtrip_uint16be n =
  let n = abs n mod 65536 in
  let encoded = Wire.encode_to_string Wire.uint16be n in
  match parse_chunked ~chunk_size:1 Wire.uint16be encoded with
  | Ok decoded -> if n <> decoded then fail "stream uint16be mismatch"
  | Error _ -> fail "stream uint16be parse failed"

let test_stream_roundtrip_uint32 n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = Wire.encode_to_string Wire.uint32 n in
  match parse_chunked ~chunk_size:1 Wire.uint32 encoded with
  | Ok decoded -> if n <> decoded then fail "stream uint32 chunk=1 mismatch"
  | Error _ -> fail "stream uint32 chunk=1 parse failed"

let test_stream_roundtrip_uint32be_chunk3 n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = Wire.encode_to_string Wire.uint32be n in
  match parse_chunked ~chunk_size:3 Wire.uint32be encoded with
  | Ok decoded -> if n <> decoded then fail "stream uint32be chunk=3 mismatch"
  | Error _ -> fail "stream uint32be chunk=3 parse failed"

let test_stream_roundtrip_uint64 n =
  let encoded = Wire.encode_to_string Wire.uint64 n in
  match parse_chunked ~chunk_size:1 Wire.uint64 encoded with
  | Ok decoded -> if n <> decoded then fail "stream uint64 chunk=1 mismatch"
  | Error _ -> fail "stream uint64 chunk=1 parse failed"

let test_stream_roundtrip_uint64be_chunk5 n =
  let encoded = Wire.encode_to_string Wire.uint64be n in
  match parse_chunked ~chunk_size:5 Wire.uint64be encoded with
  | Ok decoded -> if n <> decoded then fail "stream uint64be chunk=5 mismatch"
  | Error _ -> fail "stream uint64be chunk=5 parse failed"

let test_stream_roundtrip_record x y z =
  let x = abs x mod 256 in
  let y = abs y mod 65536 in
  let z = z land ((1 lsl 32) - 1) in
  let original = { x; y; z } in
  match encode_record_to_string test_record_codec original with
  | Error _ -> fail "stream record encode failed"
  | Ok encoded -> (
      (* Parse individual fields through chunked reader *)
      match parse_chunked ~chunk_size:1 Wire.uint8 (String.sub encoded 0 1) with
      | Error _ -> fail "stream record x parse failed"
      | Ok vx -> (
          if x <> vx then fail "stream record x mismatch";
          match
            parse_chunked ~chunk_size:1 Wire.uint16 (String.sub encoded 1 2)
          with
          | Error _ -> fail "stream record y parse failed"
          | Ok vy -> (
              if y <> vy then fail "stream record y mismatch";
              match
                parse_chunked ~chunk_size:1 Wire.uint32 (String.sub encoded 3 4)
              with
              | Error _ -> fail "stream record z parse failed"
              | Ok vz -> if z <> vz then fail "stream record z mismatch")))

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
    test_case "single elem array" [ const () ] test_single_elem_array;
    test_case "single elem at most" [ const () ] test_single_elem_at_most;
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

let parse_tests =
  [
    test_case "parse uint8" [ bytes ] test_parse_uint8;
    test_case "parse uint16" [ bytes ] test_parse_uint16;
    test_case "parse uint16be" [ bytes ] test_parse_uint16be;
    test_case "parse uint32" [ bytes ] test_parse_uint32;
    test_case "parse uint32be" [ bytes ] test_parse_uint32be;
    test_case "parse uint63" [ bytes ] test_parse_uint63;
    test_case "parse uint63be" [ bytes ] test_parse_uint63be;
    test_case "parse uint64" [ bytes ] test_parse_uint64;
    test_case "parse uint64be" [ bytes ] test_parse_uint64be;
    test_case "parse bitfield" [ bytes ] test_parse_bitfield;
    test_case "parse U8" [ bytes ] test_parse_bf_uint8;
    test_case "parse U16" [ bytes ] test_parse_bf_uint16;
    test_case "parse U16be" [ bytes ] test_parse_bf_uint16be;
    test_case "parse U32be" [ bytes ] test_parse_bf_uint32be;
    test_case "parse map" [ bytes ] test_parse_map;
    test_case "parse bool" [ bytes ] test_parse_bool;
    test_case "parse unit" [ bytes ] test_parse_unit;
    test_case "parse array" [ bytes ] test_parse_array;
    test_case "parse byte_array" [ bytes ] test_parse_byte_array;
    test_case "parse enum" [ bytes ] test_parse_variants;
    test_case "parse where" [ bytes ] test_parse_where;
    test_case "parse all_bytes" [ bytes ] test_parse_all_bytes;
    test_case "parse all_zeros" [ bytes ] test_parse_all_zeros;
    test_case "parse struct" [ bytes ] test_parse_struct;
    test_case "parse struct constrained" [ bytes ] test_parse_struct_constrained;
    test_case "parse struct be" [ bytes ] test_parse_struct_be;
    test_case "parse struct bitfields" [ bytes ] test_parse_struct_bitfields;
    test_case "parse anon field" [ bytes ] test_parse_anon_field;
    test_case "parse casetype" [ bytes ] test_parse_casetype;
  ]

let roundtrip_tests =
  [
    test_case "roundtrip uint8" [ int ] test_roundtrip_uint8;
    test_case "roundtrip uint16" [ int ] test_roundtrip_uint16;
    test_case "roundtrip uint16be" [ int ] test_roundtrip_uint16be;
    test_case "roundtrip uint32" [ int ] test_roundtrip_uint32;
    test_case "roundtrip uint32be" [ int ] test_roundtrip_uint32be;
    test_case "roundtrip uint63" [ int ] test_roundtrip_uint63;
    test_case "roundtrip uint63be" [ int ] test_roundtrip_uint63be;
    test_case "roundtrip uint64" [ int64 ] test_roundtrip_uint64;
    test_case "roundtrip uint64be" [ int64 ] test_roundtrip_uint64be;
    test_case "roundtrip map" [ int ] test_roundtrip_map;
    test_case "roundtrip bool" [ int ] test_roundtrip_bool;
    test_case "roundtrip array" [ int; int; int ] test_roundtrip_array;
    test_case "roundtrip byte_array" [ bytes ] test_roundtrip_byte_array;
    test_case "roundtrip enum" [ int ] test_roundtrip_variants;
  ]

let record_tests =
  [
    test_case "record roundtrip" [ int; int; int ] test_record_roundtrip;
    test_case "record decode crash" [ bytes ] test_record_decode_crash;
    test_case "record be roundtrip" [ int; int ] test_record_be_roundtrip;
    test_case "record bool roundtrip" [ int ] test_record_bool_roundtrip;
  ]

let stream_tests =
  [
    test_case "stream uint16 chunk=1" [ int ] test_stream_roundtrip_uint16;
    test_case "stream uint16be chunk=1" [ int ] test_stream_roundtrip_uint16be;
    test_case "stream uint32 chunk=1" [ int ] test_stream_roundtrip_uint32;
    test_case "stream uint32be chunk=3" [ int ]
      test_stream_roundtrip_uint32be_chunk3;
    test_case "stream uint64 chunk=1" [ int64 ] test_stream_roundtrip_uint64;
    test_case "stream uint64be chunk=5" [ int64 ]
      test_stream_roundtrip_uint64be_chunk5;
    test_case "stream record chunk=1" [ int; int; int ]
      test_stream_roundtrip_record;
  ]

(** {1 Dependent-size Field Tests} *)

module Slice = Bytesrw.Bytes.Slice

(* -- byte_slice variant: [length:u16be] [payload:byte_slice(length)] -- *)

type slice_msg = { sl_length : int; sl_payload : Slice.t }

let f_sl_length = Wire.Codec.field "Length" Wire.uint16be (fun r -> r.sl_length)

let f_sl_payload =
  Wire.Codec.field "Payload"
    (Wire.byte_slice ~size:(Wire.Codec.field_ref f_sl_length))
    (fun r -> r.sl_payload)

let slice_msg_codec =
  Wire.Codec.view "SliceMsg"
    (fun length payload -> { sl_length = length; sl_payload = payload })
    Wire.Codec.[ f_sl_length; f_sl_payload ]

let slice_or_eod buf len =
  if len = 0 then Slice.eod else Slice.make buf ~first:0 ~length:len

let test_depsize_slice_roundtrip payload_str =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let payload_bytes = Bytes.of_string payload_str in
  let payload = slice_or_eod payload_bytes len in
  let original = { sl_length = len; sl_payload = payload } in
  let total = 2 + len in
  let buf = Bytes.create total in
  Wire.Codec.encode slice_msg_codec original buf 0;
  let decoded = Wire.Codec.decode slice_msg_codec buf 0 in
  if decoded.sl_length <> len then fail "depsize slice length mismatch";
  let dec_payload =
    Bytes.sub_string
      (Slice.bytes decoded.sl_payload)
      (Slice.first decoded.sl_payload)
      (Slice.length decoded.sl_payload)
  in
  if dec_payload <> payload_str then fail "depsize slice payload mismatch"

let test_depsize_slice_empty () =
  let payload = Slice.eod in
  let original = { sl_length = 0; sl_payload = payload } in
  let buf = Bytes.create 2 in
  Wire.Codec.encode slice_msg_codec original buf 0;
  let decoded = Wire.Codec.decode slice_msg_codec buf 0 in
  if decoded.sl_length <> 0 then fail "depsize slice empty length mismatch";
  if Slice.length decoded.sl_payload <> 0 then
    fail "depsize slice empty payload mismatch"

(* -- byte_array variant: [length:u16be] [data:byte_array(length)] -- *)

type array_msg = { ba_length : int; ba_data : string }

let f_ba_length = Wire.Codec.field "Length" Wire.uint16be (fun r -> r.ba_length)

let f_ba_data =
  Wire.Codec.field "Data"
    (Wire.byte_array ~size:(Wire.Codec.field_ref f_ba_length))
    (fun r -> r.ba_data)

let array_msg_codec =
  Wire.Codec.view "ArrayMsg"
    (fun length data -> { ba_length = length; ba_data = data })
    Wire.Codec.[ f_ba_length; f_ba_data ]

let test_depsize_array_roundtrip payload_str =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let original = { ba_length = len; ba_data = payload_str } in
  let total = 2 + len in
  let buf = Bytes.create total in
  Wire.Codec.encode array_msg_codec original buf 0;
  let decoded = Wire.Codec.decode array_msg_codec buf 0 in
  if decoded.ba_length <> len then fail "depsize array length mismatch";
  if decoded.ba_data <> payload_str then fail "depsize array data mismatch"

let test_depsize_array_empty () =
  let original = { ba_length = 0; ba_data = "" } in
  let buf = Bytes.create 2 in
  Wire.Codec.encode array_msg_codec original buf 0;
  let decoded = Wire.Codec.decode array_msg_codec buf 0 in
  if decoded.ba_length <> 0 then fail "depsize array empty length mismatch";
  if decoded.ba_data <> "" then fail "depsize array empty data mismatch"

(* -- trailing fixed field: [length:u16be] [payload:byte_slice(length)] [tag:u8] -- *)

type tagged_msg = { tm_length : int; tm_payload : Slice.t; tm_tag : int }

let f_tm_length = Wire.Codec.field "Length" Wire.uint16be (fun r -> r.tm_length)

let f_tm_payload =
  Wire.Codec.field "Payload"
    (Wire.byte_slice ~size:(Wire.Codec.field_ref f_tm_length))
    (fun r -> r.tm_payload)

let f_tm_tag = Wire.Codec.field "Tag" Wire.uint8 (fun r -> r.tm_tag)

let tagged_msg_codec =
  Wire.Codec.view "TaggedMsg"
    (fun length payload tag ->
      { tm_length = length; tm_payload = payload; tm_tag = tag })
    Wire.Codec.[ f_tm_length; f_tm_payload; f_tm_tag ]

let test_depsize_tagged_roundtrip payload_str tag =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let tag = abs tag mod 256 in
  let payload_bytes = Bytes.of_string payload_str in
  let payload = slice_or_eod payload_bytes len in
  let original = { tm_length = len; tm_payload = payload; tm_tag = tag } in
  let total = 2 + len + 1 in
  let buf = Bytes.create total in
  Wire.Codec.encode tagged_msg_codec original buf 0;
  let decoded = Wire.Codec.decode tagged_msg_codec buf 0 in
  if decoded.tm_length <> len then fail "depsize tagged length mismatch";
  let dec_payload =
    Bytes.sub_string
      (Slice.bytes decoded.tm_payload)
      (Slice.first decoded.tm_payload)
      (Slice.length decoded.tm_payload)
  in
  if dec_payload <> payload_str then fail "depsize tagged payload mismatch";
  if decoded.tm_tag <> tag then fail "depsize tagged tag mismatch"

let test_depsize_tagged_empty tag =
  let tag = abs tag mod 256 in
  let payload = Slice.eod in
  let original = { tm_length = 0; tm_payload = payload; tm_tag = tag } in
  let buf = Bytes.create 3 in
  Wire.Codec.encode tagged_msg_codec original buf 0;
  let decoded = Wire.Codec.decode tagged_msg_codec buf 0 in
  if decoded.tm_length <> 0 then fail "depsize tagged empty length mismatch";
  if Slice.length decoded.tm_payload <> 0 then
    fail "depsize tagged empty payload mismatch";
  if decoded.tm_tag <> tag then fail "depsize tagged empty tag mismatch"

let test_depsize_compute_wire_size payload_str =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let payload_bytes = Bytes.of_string payload_str in
  let payload = slice_or_eod payload_bytes len in
  let original = { sl_length = len; sl_payload = payload } in
  let total = 2 + len in
  let buf = Bytes.create total in
  Wire.Codec.encode slice_msg_codec original buf 0;
  let ws = Wire.Codec.wire_size_at slice_msg_codec buf 0 in
  if ws <> total then
    fail (Fmt.str "depsize wire_size_at: expected %d got %d" total ws)

let depsize_tests =
  [
    test_case "depsize slice roundtrip" [ bytes ] test_depsize_slice_roundtrip;
    test_case "depsize slice empty" [ const () ] test_depsize_slice_empty;
    test_case "depsize array roundtrip" [ bytes ] test_depsize_array_roundtrip;
    test_case "depsize array empty" [ const () ] test_depsize_array_empty;
    test_case "depsize tagged roundtrip" [ bytes; int ]
      test_depsize_tagged_roundtrip;
    test_case "depsize tagged empty" [ int ] test_depsize_tagged_empty;
    test_case "depsize wire_size_at" [ bytes ] test_depsize_compute_wire_size;
  ]

let suite =
  ( "wire",
    pp_tests @ codegen_tests @ parse_tests @ roundtrip_tests @ record_tests
    @ stream_tests @ depsize_tests )
