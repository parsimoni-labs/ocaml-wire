(** Fuzz tests for param/action/eval runtime parse safety. *)

open Crowbar
open Crowbar_util

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
  let s =
    Wire.C.struct_ "ActionFuzz"
      [
        Wire.C.field "x"
          ~action:
            (Wire.Action.on_success
               [
                 Wire.Action.var "tmp"
                   Wire.Expr.(Wire.field_ref "x" * Wire.int 2);
                 Wire.Action.return_bool
                   Wire.Expr.(Wire.field_ref "tmp" <= Wire.int 510);
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
  let s =
    Wire.C.struct_ "AbortFuzz"
      [
        Wire.C.field "x"
          ~action:
            (Wire.Action.on_success
               [
                 Wire.Action.if_
                   Wire.Expr.(Wire.field_ref "x" = Wire.int 0)
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
  let out = Wire.Param.output "out" Wire.uint8 in
  let s =
    Wire.C.param_struct "ParamFuzz"
      [ Wire.Param.v limit; Wire.Param.v out ]
      ~where:Wire.Expr.(Wire.field_ref "x" <= Wire.field_ref "limit")
      [
        Wire.C.field "x"
          ~action:
            (Wire.Action.on_success
               [ Wire.Action.assign "out" (Wire.field_ref "x") ])
          Wire.uint8;
      ]
  in
  let env =
    Wire.Param.empty |> fun env ->
    Wire.Param.bind env limit 128 |> fun env -> Wire.Param.init env out 0
  in
  let _ = Wire.decode_string ~env (Wire.C.struct_typ s) buf in
  ()

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

let suite = ("param", parse_tests)
