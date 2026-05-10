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
  let _ = Wire.of_string t buf in
  ()

(* Parse crash safety: struct with action on random input *)
let test_parse_struct_action buf =
  let buf = truncate buf in
  let f_x = Wire.Field.v "x" Wire.uint8 in
  let f_tmp = Wire.Field.v "tmp" Wire.uint8 in
  let c =
    Wire.Codec.v "ActionFuzz"
      (fun x y -> (x, y))
      Wire.Codec.
        [
          Wire.Field.v "x"
            ~action:
              (Wire.Action.on_success
                 [
                   Wire.Action.var "tmp"
                     Wire.Expr.(Wire.Field.ref f_x * Wire.int 2);
                   Wire.Action.return_bool
                     Wire.Expr.(Wire.Field.ref f_tmp <= Wire.int 510);
                 ])
            Wire.uint8
          $ fst;
          Wire.Field.v "y" Wire.uint8 $ snd;
        ]
  in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

(* Parse crash safety: struct with action abort on random input *)
let test_parse_struct_action_abort buf =
  let buf = truncate buf in
  let f_x = Wire.Field.v "x" Wire.uint8 in
  let c =
    Wire.Codec.v "AbortFuzz"
      (fun x -> x)
      Wire.Codec.
        [
          ( Wire.Field.v "x"
              ~action:
                (Wire.Action.on_success
                   [
                     Wire.Action.if_
                       Wire.Expr.(Wire.Field.ref f_x = Wire.int 0)
                       [ Wire.Action.abort ] None;
                   ])
              Wire.uint8
          $ fun x -> x );
        ]
  in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

(* Parse crash safety: struct with sizeof/sizeof_this/field_pos constraints *)
let test_parse_struct_sizeof buf =
  let buf = truncate buf in
  let c =
    Wire.Codec.v "SizeofFuzz"
      (fun a b c -> (a, b, c))
      Wire.Codec.
        [
          (Wire.Field.v "a" Wire.uint8 $ fun (a, _, _) -> a);
          ( Wire.Field.v "b"
              ~constraint_:Wire.Expr.(Wire.sizeof_this = Wire.int 1)
              Wire.uint8
          $ fun (_, b, _) -> b );
          ( Wire.Field.v "c"
              ~constraint_:Wire.Expr.(Wire.field_pos = Wire.int 2)
              Wire.uint8
          $ fun (_, _, c) -> c );
        ]
  in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

(* Parse crash safety: param struct with random input *)
let test_parse_param_struct buf =
  let buf = truncate buf in
  let limit = Wire.Param.input "limit" Wire.uint8 in
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_x = Wire.Field.v "x" Wire.uint8 in
  let c =
    Wire.Codec.v "ParamFuzz"
      ~where:Wire.Expr.(Wire.Field.ref f_x <= Wire.Param.expr limit)
      (fun x -> x)
      Wire.Codec.
        [
          ( Wire.Field.v "x"
              ~action:
                (Wire.Action.on_success
                   [ Wire.Action.assign out (Wire.Field.ref f_x) ])
              Wire.uint8
          $ fun x -> x );
        ]
  in
  let env = Wire.Codec.env c |> Wire.Param.bind limit 128 in
  let _ = Wire.Codec.decode ~env c (Bytes.of_string buf) 0 in
  ()

let f_fuzz_x = Wire.Field.v "x" Wire.uint8

(* Fuzz: Param_ref in where clause with random values *)
let test_param_ref_where buf =
  let buf = truncate buf in
  let max_val = Wire.Param.input "max_val" Wire.uint16be in
  let c =
    Wire.Codec.v "ParamRefWhere"
      ~where:Wire.Expr.(Wire.Field.ref f_fuzz_x <= Wire.Param.expr max_val)
      (fun x -> x)
      Wire.Codec.[ (f_fuzz_x $ fun x -> x) ]
  in
  let env = Wire.Codec.env c |> Wire.Param.bind max_val 200 in
  let _ = Wire.Codec.decode ~env c (Bytes.of_string buf) 0 in
  ()

(* Fuzz: Param_ref in constraint with random input *)
let test_param_ref_constraint buf =
  let buf = truncate buf in
  let limit = Wire.Param.input "limit" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  let c =
    Wire.Codec.v "ParamRefConst"
      (fun v -> v)
      Wire.Codec.
        [
          ( Wire.Field.v "v"
              ~constraint_:
                Wire.Expr.(Wire.Field.ref f_v <= Wire.Param.expr limit)
              Wire.uint8
          $ fun v -> v );
        ]
  in
  let env = Wire.Codec.env c |> Wire.Param.bind limit 50 in
  let _ = Wire.Codec.decode ~env c (Bytes.of_string buf) 0 in
  ()

(* Fuzz: typed Assign to output param *)
let test_typed_assign buf =
  let buf = truncate buf in
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  let c =
    Wire.Codec.v "TypedAssign"
      (fun v -> v)
      Wire.Codec.
        [
          ( Wire.Field.v "v"
              ~action:
                (Wire.Action.on_success
                   [ Wire.Action.assign out (Wire.Field.ref f_v) ])
              Wire.uint8
          $ fun v -> v );
        ]
  in
  let env = Wire.Codec.env c in
  let _ = Wire.Codec.decode ~env c (Bytes.of_string buf) 0 in
  (* Verify output was set (no crash) *)
  let _ = Wire.Param.get env out in
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

let param_ref_tests =
  [
    test_case "param_ref where" [ bytes ] test_param_ref_where;
    test_case "param_ref constraint" [ bytes ] test_param_ref_constraint;
    test_case "typed assign" [ bytes ] test_typed_assign;
  ]

let suite = ("param", parse_tests @ param_ref_tests)
