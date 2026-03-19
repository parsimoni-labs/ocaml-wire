(** Fuzz tests for param/action/eval runtime parse safety. *)

open Alcobar

(** Truncate input to reasonable size for protocol messages. *)
let truncate buf =
  let max_len = 1024 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

(* Parse crash safety: lookup with random input *)
let test_parse_lookup buf =
  let buf = truncate buf in
  let t = Wire.lookup [ `A; `B; `C ] Wire.uint8 in
  let _ = Wire.decode_string t buf in
  ()

(* Parse crash safety: struct with action on random input *)
let test_parse_struct_action buf =
  let buf = truncate buf in
  let f_x = Wire.C.field "x" Wire.uint8 in
  let f_tmp = Wire.C.field "tmp" Wire.uint8 in
  let s =
    Wire.C.struct_ "ActionFuzz"
      [
        Wire.C.field "x"
          ~action:
            (Wire.Action.on_success
               [
                 Wire.Action.var "tmp"
                   Wire.Expr.(Wire.C.field_ref f_x * Wire.int 2);
                 Wire.Action.return_bool
                   Wire.Expr.(Wire.C.field_ref f_tmp <= Wire.int 510);
               ])
          Wire.uint8;
        Wire.C.field "y" Wire.uint8;
      ]
  in
  let _ = Wire.decode_string (Wire.C.struct_typ s) buf in
  ()

(* Parse crash safety: struct with action abort on random input *)
let test_parse_struct_action_abort buf =
  let buf = truncate buf in
  let f_x = Wire.C.field "x" Wire.uint8 in
  let s =
    Wire.C.struct_ "AbortFuzz"
      [
        Wire.C.field "x"
          ~action:
            (Wire.Action.on_success
               [
                 Wire.Action.if_
                   Wire.Expr.(Wire.C.field_ref f_x = Wire.int 0)
                   [ Wire.Action.abort ] None;
               ])
          Wire.uint8;
      ]
  in
  let _ = Wire.decode_string (Wire.C.struct_typ s) buf in
  ()

(* Parse crash safety: struct with sizeof/sizeof_this/field_pos constraints *)
let test_parse_struct_sizeof buf =
  let buf = truncate buf in
  let s =
    Wire.C.struct_ "SizeofFuzz"
      [
        Wire.C.field "a" Wire.uint8;
        Wire.C.field "b"
          ~constraint_:Wire.Expr.(Wire.sizeof_this = Wire.int 1)
          Wire.uint8;
        Wire.C.field "c"
          ~constraint_:Wire.Expr.(Wire.field_pos = Wire.int 2)
          Wire.uint8;
      ]
  in
  let _ = Wire.decode_string (Wire.C.struct_typ s) buf in
  ()

(* Parse crash safety: param struct with random input *)
let test_parse_param_struct buf =
  let buf = truncate buf in
  let limit = Wire.Param.input "limit" Wire.uint8 in
  let _limit_expr = Wire.Param.init limit 128 in
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_x = Wire.C.field "x" Wire.uint8 in
  let c =
    Wire.Codec.view "ParamFuzz"
      ~where:Wire.Expr.(Wire.C.field_ref f_x <= Wire.Param.expr limit)
      (fun x -> x)
      Wire.Codec.
        [
          Wire.Codec.field "x"
            ~action:
              (Wire.Action.on_success
                 [ Wire.Action.assign out (Wire.C.field_ref f_x) ])
            Wire.uint8
            (fun x -> x);
        ]
  in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

let f_fuzz_x = Wire.Codec.field "x" Wire.uint8 (fun x -> x)

(* Fuzz: Param_ref in where clause with random values *)
let test_param_ref_where buf =
  let buf = truncate buf in
  let max_val = Wire.Param.input "max_val" Wire.uint16be in
  let _ = Wire.Param.init max_val 200 in
  let c =
    Wire.Codec.view "ParamRefWhere"
      ~where:
        Wire.Expr.(Wire.Codec.field_ref f_fuzz_x <= Wire.Param.expr max_val)
      (fun x -> x)
      Wire.Codec.[ f_fuzz_x ]
  in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

(* Fuzz: Param_ref in constraint with random input *)
let test_param_ref_constraint buf =
  let buf = truncate buf in
  let limit = Wire.Param.input "limit" Wire.uint8 in
  let _ = Wire.Param.init limit 50 in
  let f_v = Wire.C.field "v" Wire.uint8 in
  let f =
    Wire.Codec.field "v"
      ~constraint_:Wire.Expr.(Wire.C.field_ref f_v <= Wire.Param.expr limit)
      Wire.uint8
      (fun v -> v)
  in
  let c = Wire.Codec.view "ParamRefConst" (fun v -> v) Wire.Codec.[ f ] in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

(* Fuzz: typed Assign to output param *)
let test_typed_assign buf =
  let buf = truncate buf in
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_v = Wire.C.field "v" Wire.uint8 in
  let f =
    Wire.Codec.field "v"
      ~action:
        (Wire.Action.on_success
           [ Wire.Action.assign out (Wire.C.field_ref f_v) ])
      Wire.uint8
      (fun v -> v)
  in
  let c = Wire.Codec.view "TypedAssign" (fun v -> v) Wire.Codec.[ f ] in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  (* Verify output was set (no crash) *)
  let _ = Wire.Param.get out in
  ()

(* Fuzz: map ~decode ~encode roundtrip *)
let test_map_roundtrip n =
  let n = abs n mod 256 in
  let t =
    Wire.map ~decode:(fun x -> x * 2) ~encode:(fun x -> x / 2) Wire.uint8
  in
  let encoded = Wire.encode_to_string t (n * 2) in
  match Wire.decode_string t encoded with
  | Ok decoded -> if n * 2 <> decoded then fail "map roundtrip mismatch"
  | Error _ -> fail "map roundtrip parse failed"

(** {1 Test Registration} *)

let parse_tests =
  [
    test_case "parse lookup" [ bytes ] test_parse_lookup;
    test_case "parse struct action" [ bytes ] test_parse_struct_action;
    test_case "parse struct action abort" [ bytes ]
      test_parse_struct_action_abort;
    test_case "parse struct sizeof" [ bytes ] test_parse_struct_sizeof;
    test_case "parse param struct" [ bytes ] test_parse_param_struct;
  ]

let param_ref_tests =
  [
    test_case "param_ref where" [ bytes ] test_param_ref_where;
    test_case "param_ref constraint" [ bytes ] test_param_ref_constraint;
    test_case "typed assign" [ bytes ] test_typed_assign;
    test_case "map roundtrip" [ int ] test_map_roundtrip;
  ]

let suite = ("param", parse_tests @ param_ref_tests)
