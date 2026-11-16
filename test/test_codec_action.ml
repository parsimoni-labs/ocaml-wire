(* Tests for codec.ml: actions, parameters and fields shared by codecs. *)

open Wire
open Test_helpers

(* -- action semantics -- *)

let test_action_fires_decode_env () =
  (* decode_env fires actions and syncs output params *)
  let env =
    Codec.env projection_codec |> Param.bind projection_limit (UInt8.v 10)
  in
  let buf = Bytes.of_string "\x05" in
  Alcotest.(check int)
    "outx before" 0
    (UInt8.to_int (Param.get env projection_outx));
  let _v = decode_ok (Codec.decode ~env projection_codec buf 0) in
  Alcotest.(check int)
    "outx after decode_env" 5
    (UInt8.to_int (Param.get env projection_outx))

let test_action_fires_on_get () =
  (* get fires field actions. A return_bool action that rejects odd values
     should cause get to raise on odd input. *)
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_ref mod int 2 = int 0) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "ActionGet" (fun v -> v) [ cf_v ] in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  (* Even value: action passes *)
  Alcotest.(check int)
    "get even" 0x42
    (UInt8.to_int (get_v (Bytes.of_string "\x42") 0));
  (* Odd value: action rejects *)
  match get_v (Bytes.of_string "\x43") 0 with
  | _ -> Alcotest.fail "expected action to reject odd value"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_action_validate_does_not_writeback () =
  (* validate runs actions in scratch storage but does not expose their output
     assignments through the caller's env. [test_validate_runs_field_action]
     separately proves that the action itself runs. *)
  let action_out2 = Param.output "act_out2" uint8 in
  let f_ref2 = Field.v "v" uint8 in
  let cf_v2 =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success [ Action.assign action_out2 (Field.ref f_ref2) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "ActionValidate" (fun v -> v) [ cf_v2 ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  Codec.validate ~env codec buf 0;
  Alcotest.(check int)
    "action output not written back by validate" 0
    (UInt8.to_int (Param.get env action_out2))

let test_get_noaction_zero_overhead () =
  (* get on a field without an action should not allocate.
     We just verify it works -- allocation is checked by benchmarks. *)
  let cf_v = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec = Codec.v "NoAction" (fun v -> v) [ cf_v ] in
  let buf = Bytes.of_string "\x42" in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  Alcotest.(check int) "get returns value" 0x42 (UInt8.to_int (get_v buf 0))

let test_get_with_env () =
  (* get ~env fires action and syncs output params to env *)
  let out = Param.output "out" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "GetEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  let v = get_v buf 0 in
  Alcotest.(check int) "get returns value" 0x42 (UInt8.to_int v);
  Alcotest.(check int)
    "output param synced" 0x42
    (UInt8.to_int (Param.get env out))

let test_get_action_field_twocodecs () =
  (* Same action field in two codecs -- each codec gets its own action runner *)
  let out1 = Param.output "out1" uint8 in
  let out2 = Param.output "out2" uint16be in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out1 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  (* Codec1: [v] at offset 0 *)
  let codec1 = Codec.v "ActTwo1" (fun v -> v) [ cf_v ] in
  (* Codec2: [pad] [v] -- v at offset 1, different action *)
  let cf_v2 =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out2 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec2 =
    let open Codec in
    v "ActTwo2"
      (fun _pad v -> v)
      [ (Field.v "pad" uint8 $ fun _ -> UInt8.v 0); cf_v2 ]
  in
  let env1 = Codec.env codec1 in
  let env2 = Codec.env codec2 in
  let buf = Bytes.of_string "\xAA\xBB" in
  let get1 = Staged.unstage (Codec.get ~env:env1 codec1 cf_v) in
  let get2 = Staged.unstage (Codec.get ~env:env2 codec2 cf_v2) in
  (* codec1 reads offset 0 = 0xAA *)
  Alcotest.(check int) "codec1 get" 0xAA (UInt8.to_int (get1 buf 0));
  Alcotest.(check int) "codec1 out" 0xAA (UInt8.to_int (Param.get env1 out1));
  (* codec2 reads offset 1 = 0xBB *)
  Alcotest.(check int) "codec2 get" 0xBB (UInt8.to_int (get2 buf 0));
  Alcotest.(check int) "codec2 out" 0xBB (UInt16.to_int (Param.get env2 out2))

let test_get_action_no_env () =
  (* get without ~env on action field: action fires but output not accessible *)
  let out = Param.output "out_noenv" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "NoEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  (* No ~env: action fires (no crash) but output stays 0 *)
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  Alcotest.(check int) "get returns value" 0x42 (UInt8.to_int (get_v buf 0));
  Alcotest.(check int)
    "output not synced without env" 0
    (UInt8.to_int (Param.get env out))

let test_get_action_abort_field () =
  (* get on a field with abort action always raises *)
  let cf_v =
    Codec.(
      Field.v "v" ~action:(Action.on_success [ Action.abort ]) uint8 $ fun v ->
      v)
  in
  let codec = Codec.v "AbortGet" (fun v -> v) [ cf_v ] in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  match get_v (Bytes.of_string "\x42") 0 with
  | _ -> Alcotest.fail "expected abort"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_get_noaction_ignores_env () =
  (* Passing ~env to get on a field without action is harmless *)
  let cf_v = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec = Codec.v "NoActEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int)
    "get returns value" 0x42
    (UInt8.to_int (get_v (Bytes.of_string "\x42") 0))

let test_get_action_multiple_calls () =
  (* get with ~env updates output on every call *)
  let out = Param.output "out_multi" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "Multi" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  ignore (get_v (Bytes.of_string "\x10") 0);
  Alcotest.(check int) "after first" 0x10 (UInt8.to_int (Param.get env out));
  ignore (get_v (Bytes.of_string "\x20") 0);
  Alcotest.(check int) "after second" 0x20 (UInt8.to_int (Param.get env out))

let test_get_action_with_inputparam () =
  (* Action references an input param -- get ~env must blit it into the
     scratch array so the action sees the bound value. *)
  let limit = Param.input "limit" uint8 in
  let out = Param.output "result" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [
               Action.assign out (Field.ref f_ref);
               Action.return_bool Expr.(Field.ref f_ref <= Param.expr limit);
             ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "InputParam" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec |> Param.bind limit (UInt8.v 50) in
  let buf_ok = Bytes.of_string "\x30" in
  let buf_bad = Bytes.of_string "\x40" in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  (* 0x30 = 48 <= 50: passes *)
  Alcotest.(check int)
    "get with input param" 0x30
    (UInt8.to_int (get_v buf_ok 0));
  Alcotest.(check int) "output synced" 0x30 (UInt8.to_int (Param.get env out));
  (* 0x40 = 64 > 50: action rejects *)
  match get_v buf_bad 0 with
  | _ -> Alcotest.fail "expected rejection from input param check"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_get_action_inputparam_noenv () =
  (* An action comparing against an input param is a verdict on the bytes, so
     resolving the unbound param to 0 does not weaken the check, it answers a
     different question: every value above 0 was rejected against a bound the
     caller never set. The reader refuses instead, and answers once bound. *)
  let limit = Param.input "lim2" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_ref <= Param.expr limit) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "NoEnvInput" (fun v -> v) [ cf_v ] in
  (match Codec.get codec cf_v with
  | _ -> Alcotest.fail "expected get to refuse an unbound action param"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "the refusal names the remedy" true (contains ~sub:"?env" msg));
  let env = Param.bind_by_name "lim2" 4 (Codec.env codec) in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int)
    "within the bound the value reads back" 4
    (UInt8.to_int (get_v (Bytes.of_string "\x04") 0));
  match get_v (Bytes.of_string "\x05") 0 with
  | _ -> Alcotest.fail "expected rejection above the bound"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

(* -- Param forwarding into embedded sub-codecs -- *)

(* A sub-codec whose field is sized by an input param, embedded as a field of an
   outer codec. The outer surfaces the sub's param; binding it on the outer env
   drives the embedded field width on both encode and decode. *)
let test_embed_param_sized () =
  let elen = Param.input "elen" uint8 in
  let sub =
    Codec.v "EmbSub"
      (fun d -> d)
      Codec.[ Field.v "data" (byte_array ~size:(Param.expr elen)) $ Fun.id ]
  in
  let outer =
    Codec.v "EmbOuter" (fun s -> s) Codec.[ Field.v "s" (codec sub) $ Fun.id ]
  in
  let env = Codec.env outer |> Param.bind elen (UInt8.v 3) in
  let n = Codec.size_of_value outer "abc" in
  Alcotest.(check int) "wire size" 3 n;
  let buf = Bytes.create n in
  Codec.encode ~env outer "abc" buf 0;
  Alcotest.(check string)
    "roundtrip" "abc"
    (decode_ok (Codec.decode ~env outer buf 0))

(* A fixed-width embedded codec can still depend on an input parameter through
   its validation. A staged getter must preserve the outer environment when it
   enters that codec, even though neither the field offset nor its size varies. *)
let test_get_embed_param_fixed () =
  let limit = Param.input "get_embed_limit" uint8 in
  let f_v = Field.v "v" uint8 in
  let sub =
    Codec.v "GetEmbedSub"
      ~where:Expr.(Field.ref f_v <= Param.expr limit)
      Fun.id
      Codec.[ f_v $ Fun.id ]
  in
  let f_sub = Field.v "sub" (codec sub) in
  let cf_sub = Codec.(f_sub $ Fun.id) in
  let outer = Codec.v "GetEmbedOuter" Fun.id Codec.[ cf_sub ] in
  let env = Codec.env outer |> Param.bind limit (UInt8.v 10) in
  let get_sub = Staged.unstage (Codec.get ~env outer cf_sub) in
  Alcotest.(check int)
    "embedded validation sees env" 5
    (UInt8.to_int (get_sub (Bytes.of_string "\x05") 0))

(* The outer codec inherits the embedded sub-codec's input param, so encoding it
   without an env is rejected. *)
let test_embed_param_requires_env () =
  let elen = Param.input "elen2" uint8 in
  let sub =
    Codec.v "EmbSub2"
      (fun d -> d)
      Codec.[ Field.v "data" (byte_array ~size:(Param.expr elen)) $ Fun.id ]
  in
  let outer =
    Codec.v "EmbOuter2" (fun s -> s) Codec.[ Field.v "s" (codec sub) $ Fun.id ]
  in
  Alcotest.(check bool)
    "encode without env rejected" true
    (raises_invalid (fun () -> Codec.encode outer "ab" (Bytes.create 8) 0))

(* A sub-codec's [where] is enforced when the sub is embedded, matching its
   standalone behaviour. *)
let test_embed_where_enforced () =
  let f_v = Field.v "v" uint8 in
  let sub =
    Codec.v "WSub"
      ~where:Expr.(Field.ref f_v <= int 10)
      (fun v -> v)
      Codec.[ f_v $ Fun.id ]
  in
  let outer =
    Codec.v "WOuter" (fun s -> s) Codec.[ Field.v "s" (codec sub) $ Fun.id ]
  in
  Alcotest.(check bool)
    "violating value rejected when embedded" true
    (match Codec.decode outer (Bytes.make 1 '\xFF') 0 with
    | Ok _ -> false
    | _ -> true);
  Alcotest.(check int)
    "satisfying value accepted" 5
    (UInt8.to_int (decode_ok (Codec.decode outer (Bytes.make 1 '\x05') 0)))

let test_embed_output_param () =
  let out = Param.output "embedded_out" uint8 in
  let f_v = Field.v "v" uint8 in
  let sub_field =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_v) ])
        uint8
      $ Fun.id)
  in
  let sub = Codec.v "EmbeddedOutputSub" Fun.id Codec.[ sub_field ] in
  let outer =
    Codec.v "EmbeddedOutputOuter" Fun.id
      Codec.[ Field.v "sub" (codec sub) $ Fun.id ]
  in
  let env = Codec.env outer in
  Alcotest.(check int)
    "decoded value" 42
    (UInt8.to_int
       (decode_ok (Codec.decode ~env outer (Bytes.of_string "\x2A") 0)));
  Alcotest.(check int) "forwarded output" 42 (UInt8.to_int (Param.get env out))

(* A list of param-constrained sub-records within a byte budget, the param
   forwarded through the repeat (the shape that first exposed the gap). *)
let test_embed_param_repeat () =
  let rlim = Param.input "rlim" uint8 in
  let f_v = Field.v "v" uint8 in
  let sub =
    Codec.v "RSub"
      ~where:Expr.(Field.ref f_v <= Param.expr rlim)
      (fun v -> v)
      Codec.[ f_v $ Fun.id ]
  in
  let f_n = Field.v "n" uint8 in
  let f_items = Field.repeat "items" ~size:(Field.ref f_n) (codec sub) in
  let outer =
    Codec.v "RepOuter" (fun n xs -> (n, xs)) Codec.[ f_n $ fst; f_items $ snd ]
  in
  let v = (UInt8.v 3, List.map UInt8.v [ 1; 5; 9 ]) in
  let env = Codec.env outer |> Param.bind rlim (UInt8.v 100) in
  let n = Codec.size_of_value outer v in
  let buf = Bytes.create n in
  Codec.encode ~env outer v buf 0;
  let dn, dxs = decode_ok (Codec.decode ~env outer buf 0) in
  let vn, vxs = v in
  Alcotest.(check bool)
    "roundtrip" true
    (UInt8.equal dn vn && List.equal UInt8.equal dxs vxs)

let test_get_action_output_only () =
  (* Action with only assign (no return_bool/abort) -- should never fail *)
  let out = Param.output "out_only" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "OutOnly" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  (* Any value should work -- no validation in this action *)
  Alcotest.(check int)
    "get 0xFF" 0xFF
    (UInt8.to_int (get_v (Bytes.of_string "\xFF") 0));
  Alcotest.(check int) "output 0xFF" 0xFF (UInt8.to_int (Param.get env out));
  Alcotest.(check int)
    "get 0x00" 0x00
    (UInt8.to_int (get_v (Bytes.of_string "\x00") 0));
  Alcotest.(check int) "output 0x00" 0x00 (UInt8.to_int (Param.get env out))

let test_get_action_varthen_assign () =
  (* Action with local var computation then assign to output *)
  let out = Param.output "doubled" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [
               Action.var "tmp" Expr.(Field.ref f_ref * int 2);
               Action.assign out (Field.ref (Field.v "tmp" uint8));
             ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "VarAssign" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int)
    "get value" 21
    (UInt8.to_int (get_v (Bytes.of_string "\x15") 0));
  Alcotest.(check int) "doubled output" 42 (UInt8.to_int (Param.get env out))

let test_get_action_crossfield_ref () =
  (* Action on field y references field x's value *)
  let f_x = Field.v "x" uint8 in
  let out = Param.output "sum" uint8 in
  let cf_x = Codec.(f_x $ fun (x, _) -> x) in
  let cf_y =
    Codec.(
      Field.v "y"
        ~action:
          (Action.on_success
             [ Action.assign out Expr.(Field.ref f_x + int 100) ])
        uint8
      $ fun (_, y) -> y)
  in
  let codec = Codec.v "CrossRef" (fun x y -> (x, y)) [ cf_x; cf_y ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x0A\x14" in
  let get_y = Staged.unstage (Codec.get ~env codec cf_y) in
  let y = get_y buf 0 in
  Alcotest.(check int) "y value" 0x14 (UInt8.to_int y);
  (* Action computed x + 100 = 10 + 100 = 110 *)
  Alcotest.(check int)
    "cross-field output" 110
    (UInt8.to_int (Param.get env out))

let test_validate_constraint_only () =
  (* Codec with constraint but no where clause *)
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Codec.(
      Field.v "x" ~constraint_:Expr.(Field.ref f_x <= int 10) uint8 $ fun v -> v)
  in
  let codec = Codec.v "ConstOnly" (fun v -> v) [ cf_x ] in
  let good = Bytes.of_string "\x05" in
  let bad = Bytes.of_string "\x0B" in
  Codec.validate codec good 0;
  match Codec.validate codec bad 0 with
  | () -> Alcotest.fail "expected constraint failure"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_validate_where_only () =
  (* Codec with where clause but no field constraints *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun v -> v) in
  let codec =
    Codec.v "WhereOnly"
      ~where:Expr.(Field.ref f_x = int 42)
      (fun v -> v)
      [ cf_x ]
  in
  let good = Bytes.of_string "\x2A" in
  let bad = Bytes.of_string "\x00" in
  Codec.validate codec good 0;
  match Codec.validate codec bad 0 with
  | () -> Alcotest.fail "expected where failure"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

(* -- an output param that sizes a later field -- *)

let sized_out = Param.output "sized_out" uint8
let sized_slot = Param.input "sized_slot" uint8
let sized_limit = 9

let sized_sub =
  let f_v = Field.v "v" uint8 in
  Codec.v "SizedSub"
    ~where:Expr.(Field.ref f_v <= int sized_limit)
    Fun.id
    Codec.[ f_v $ Fun.id ]

let sized_n =
  let f_n = Field.v "n" uint8 in
  Codec.(
    Field.v "n"
      ~action:(Action.on_success [ Action.assign sized_out (Field.ref f_n) ])
      uint8
    $ fun (n, _, _) -> n)

let sized_pad =
  Codec.(
    Field.v "pad" (byte_array ~size:(Param.expr sized_out)) $ fun (_, p, _) -> p)

let sized_last typ = Codec.(Field.v "s" typ $ fun (_, _, s) -> s)

(* [n] sets the param, [pad] is sized by it, then a sub-codec with a [where]. *)
let sized_then_sub =
  Codec.v "SizedThenSub"
    (fun n p s -> (n, p, s))
    [ sized_n; sized_pad; sized_last (codec sized_sub) ]

(* The same with a constrained field of the record itself. *)
let sized_then_own =
  let f_s = Field.v "s" uint8 in
  let own =
    Codec.(
      Field.v "s" ~constraint_:Expr.(Field.ref f_s <= int sized_limit) uint8
      $ fun (_, _, s) -> s)
  in
  Codec.v "SizedThenOwn" (fun n p s -> (n, p, s)) [ sized_n; sized_pad; own ]

(* [pad] reads the param before [n] sets it. *)
let sized_before_set =
  Codec.v "SizedBeforeSet"
    (fun p n s -> (n, p, s))
    [ sized_pad; sized_n; sized_last (codec sized_sub) ]

let sized_show (n, p, s) =
  Fmt.str "%d %S %d" (UInt8.to_int n) p (UInt8.to_int s)

(* Each decode entry point of [c] with the validate that checks the same
   bytes: at offset 0 with and without an env, at offset 2, as a field of
   another codec, and in a fixed and a param-sized region. *)
let sized_entries name c =
  let field typ = Codec.v (name ^ typ) Fun.id in
  let frame = field "Frame" Codec.[ Field.v "r" (codec c) $ Fun.id ] in
  let region size =
    Codec.[ Field.v "r" (nested_at_most ~size (codec c)) $ Fun.id ]
  in
  let fixed = field "Region" (region (int 4)) in
  let sized = field "ParamRegion" (region (Param.expr sized_slot)) in
  let slot () = Codec.env sized |> Param.bind sized_slot (UInt8.v 4) in
  fun input ->
    let shifted = Bytes.cat (Bytes.of_string "\xee\xee") input in
    [
      ( "plain",
        (fun () -> Codec.validate c input 0),
        fun () -> Codec.decode_exn c input 0 );
      ( "env",
        (fun () -> Codec.validate ~env:(Codec.env c) c input 0),
        fun () -> Codec.decode_exn ~env:(Codec.env c) c input 0 );
      ( "shifted",
        (fun () -> Codec.validate c shifted 2),
        fun () -> Codec.decode_exn c shifted 2 );
      ( "field",
        (fun () -> Codec.validate frame input 0),
        fun () -> Codec.decode_exn frame input 0 );
      ( "field, env",
        (fun () -> Codec.validate ~env:(Codec.env frame) frame input 0),
        fun () -> Codec.decode_exn ~env:(Codec.env frame) frame input 0 );
      ( "region",
        (fun () -> Codec.validate fixed input 0),
        fun () -> Codec.decode_exn fixed input 0 );
      ( "param region",
        (fun () -> Codec.validate ~env:(slot ()) sized input 0),
        fun () -> Codec.decode_exn ~env:(slot ()) sized input 0 );
    ]

let sized_inputs =
  let bytes = [ 0; 1; 2; 5; 0xff ] in
  let ( let* ) l f = List.concat_map f l in
  let* a = bytes in
  let* b = bytes in
  let* c = bytes in
  let* d = bytes in
  [ Bytes.of_string (String.init 4 (fun i -> Char.chr [| a; b; c; d |].(i))) ]

let sized_result f =
  match f () with v -> Ok v | exception Parse_error e -> Error e

(* No entry point returns a record that breaks a check, and with [exact] each
   accepts exactly what its validate accepts. *)
let check_sized ~exact name c =
  let entries = sized_entries name c in
  List.iter
    (fun input ->
      List.iter
        (fun (entry, validate, decode) ->
          let where = Fmt.str "%s, %s, %S" name entry (Bytes.to_string input) in
          match (sized_result validate, sized_result decode) with
          | _, Ok ((_, _, s) as v) when UInt8.to_int s > sized_limit ->
              Alcotest.failf "%s: decode returned %s" where (sized_show v)
          | Ok (), Error e when exact ->
              Alcotest.failf "%s: validate accepts, decode fails with %a" where
                pp_parse_error e
          | Error e, Ok v when exact ->
              Alcotest.failf "%s: decode returned %s, validate fails with %a"
                where (sized_show v) pp_parse_error e
          | _ -> ())
        (entries input))
    sized_inputs

let test_output_sizes_later_field () =
  let env = Codec.env sized_then_sub in
  let v =
    decode_ok
      (Codec.decode ~env sized_then_sub (Bytes.of_string "\x01\xff\x05") 0)
  in
  Alcotest.(check string)
    "read where it was checked" {|1 "\255" 5|} (sized_show v);
  Alcotest.(check int) "output" 1 (UInt8.to_int (Param.get env sized_out));
  check_sized ~exact:true "SizedThenSub" sized_then_sub;
  check_sized ~exact:true "SizedThenOwn" sized_then_own;
  check_sized ~exact:false "SizedBeforeSet" sized_before_set

let test_get_twostaged_same_field () =
  (* Two staged getters from the same codec+field with different envs *)
  let out = Param.output "out_two" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "TwoStaged" (fun v -> v) [ cf_v ] in
  let env1 = Codec.env codec in
  let env2 = Codec.env codec in
  let get1 = Staged.unstage (Codec.get ~env:env1 codec cf_v) in
  let get2 = Staged.unstage (Codec.get ~env:env2 codec cf_v) in
  (* Each staged getter has its own scratch array and env *)
  ignore (get1 (Bytes.of_string "\xAA") 0);
  ignore (get2 (Bytes.of_string "\xBB") 0);
  Alcotest.(check int) "env1" 0xAA (UInt8.to_int (Param.get env1 out));
  Alcotest.(check int) "env2" 0xBB (UInt8.to_int (Param.get env2 out))

let test_encode_shared_bitfield () =
  (* Encode via a codec that shares a bitfield with another codec.
     Default [bit_order] is [Msb_first], so the first-declared field [a]
     lives in the top nibble of the byte. *)
  let f_a = Field.v "a" (bits ~width:4 U8) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec1 =
    let open Codec in
    v "EncBf1"
      (fun a _b -> a)
      [ cf_a; (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "EncBf2"
      (fun _b a -> a)
      [ (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0); cf_a ]
  in
  let buf = Bytes.make 1 '\x00' in
  Codec.encode codec1 0xA buf 0;
  Alcotest.(check int) "top nibble (MSB-first)" 0xA0 (Bytes.get_uint8 buf 0)

(* -- same bound field in two codecs -- *)

let test_same_field_two_codecs () =
  (* A single bound field used in two codecs with different layouts.
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
     If f_reader is mutable and set at seal time, the second seal clobbers
     the first. Both get/set must use the correct offset for their codec. *)
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "TwoCodec1"
      (fun x _y -> x)
      [ cf_x; (Field.v "y" uint16be $ fun _ -> UInt16.zero) ]
  in
  let codec2 =
    let open Codec in
    v "TwoCodec2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint16be $ fun _ -> UInt16.zero); cf_x ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint16_be buf 2 0xBBBB;
  (* codec1 should read x at offset 0 -> 0xAAAA *)
  Alcotest.(check int)
    "codec1 get x" 0xAAAA
    (UInt16.to_int ((Staged.unstage (Codec.get codec1 cf_x)) buf 0));
  (* codec2 should read x at offset 2 -> 0xBBBB *)
  Alcotest.(check int)
    "codec2 get x" 0xBBBB
    (UInt16.to_int ((Staged.unstage (Codec.get codec2 cf_x)) buf 0))

let test_samefield_twocodecs_set () =
  (* Same field in two codecs: set via each must write to the correct offset. *)
  let f_v = Field.v "v" uint8 in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec1 =
    let open Codec in
    v "SetTwo1"
      (fun v _pad -> v)
      [ cf_v; (Field.v "pad" uint8 $ fun _ -> UInt8.v 0) ]
  in
  let codec2 =
    let open Codec in
    v "SetTwo2"
      (fun _pad v -> v)
      [ (Field.v "pad" uint8 $ fun _ -> UInt8.v 0); cf_v ]
  in
  let buf = Bytes.make 2 '\x00' in
  (* set via codec1 should write to offset 0 *)
  (Staged.unstage (Codec.set codec1 cf_v)) buf 0 (UInt8.v 0xAA);
  Alcotest.(check int) "codec1 set -> byte 0" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int)
    "codec1 set -> byte 1 untouched" 0 (Bytes.get_uint8 buf 1);
  Bytes.fill buf 0 2 '\x00';
  (* set via codec2 should write to offset 1 *)
  (Staged.unstage (Codec.set codec2 cf_v)) buf 0 (UInt8.v 0xBB);
  Alcotest.(check int)
    "codec2 set -> byte 0 untouched" 0 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "codec2 set -> byte 1" 0xBB (Bytes.get_uint8 buf 1)

let test_samefield_twocodecs_decode () =
  (* Decode via the first codec after sealing both.
     The second seal clobbers f_reader, so decode uses the wrong offset. *)
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "DecTwo1"
      (fun x _y -> x)
      [ cf_x; (Field.v "y" uint16be $ fun _ -> UInt16.zero) ]
  in
  let _codec2 =
    let open Codec in
    v "DecTwo2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint16be $ fun _ -> UInt16.zero); cf_x ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (* codec1 decode should construct record with x from offset 0 *)
  match Codec.decode codec1 buf 0 with
  | Ok v -> Alcotest.(check int) "decoded x" 0x1234 (UInt16.to_int v)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_samefield_twocodecs_encode () =
  (* Encode via the first codec after sealing both.
     The second seal clobbers f_writer, so encode writes to the wrong offset. *)
  let f_v = Field.v "v" uint8 in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec1 =
    let open Codec in
    v "EncTwo1"
      (fun v _pad -> v)
      [ cf_v; (Field.v "pad" uint8 $ fun _ -> UInt8.v 0) ]
  in
  let _codec2 =
    let open Codec in
    v "EncTwo2"
      (fun _pad v -> v)
      [ (Field.v "pad" uint8 $ fun _ -> UInt8.v 0); cf_v ]
  in
  let buf = Bytes.make 2 '\x00' in
  Codec.encode codec1 (UInt8.v 0xAA) buf 0;
  (* codec1 should write v at offset 0 *)
  Alcotest.(check int) "byte 0" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "byte 1" 0x00 (Bytes.get_uint8 buf 1)

let test_same_bitfield_two_codecs () =
  (* Same bitfield bound field in two codecs with different bit positions.
     Default [bit_order = Msb_first]: first-declared field at top of byte. *)
  let f_a = Field.v "a" (bits ~width:4 U8) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec1 =
    let open Codec in
    v "BfTwo1"
      (fun a _b -> a)
      [ cf_a; (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "BfTwo2"
      (fun _b a -> a)
      [ (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0); cf_a ]
  in
  (* 0xA3: top nibble = 0xA, bottom nibble = 3. *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA3;
  (* codec1: a is first-declared, so top 4 bits -> 0xA. *)
  Alcotest.(check int)
    "codec1 get a (top)" 0xA
    ((Staged.unstage (Codec.get codec1 cf_a)) buf 0);
  (* codec2: a is second-declared, so bottom 4 bits -> 3. *)
  Alcotest.(check int)
    "codec2 get a (bottom)" 3
    ((Staged.unstage (Codec.get codec2 cf_a)) buf 0)

let test_samefield_staged_before_secondseal () =
  (* Stage get from codec1 BEFORE sealing codec2.
     The staged function captures f_reader at staging time. If f_reader
     is a mutable slot, the staged function sees the clobbered value
     after codec2 seals. *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "StagedTwo1"
      (fun x _y -> x)
      [ cf_x; (Field.v "y" uint8 $ fun _ -> UInt8.v 0) ]
  in
  (* Stage get from codec1 *)
  let get_x_1 = Staged.unstage (Codec.get codec1 cf_x) in
  (* Now seal codec2 -- this clobbers f_reader *)
  let _codec2 =
    let open Codec in
    v "StagedTwo2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint8 $ fun _ -> UInt8.v 0); cf_x ]
  in
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  (* get_x_1 was staged before codec2 -- should still read offset 0 *)
  Alcotest.(check int)
    "staged before second seal" 0xAA
    (UInt8.to_int (get_x_1 buf 0))

let suite =
  ( "codec_action",
    [
      Alcotest.test_case "action: fires on decode_env" `Quick
        test_action_fires_decode_env;
      Alcotest.test_case "action: fires on get" `Quick test_action_fires_on_get;
      Alcotest.test_case "action: validate does not write back" `Quick
        test_action_validate_does_not_writeback;
      Alcotest.test_case "action: no action zero overhead" `Quick
        test_get_noaction_zero_overhead;
      Alcotest.test_case "action: get with env" `Quick test_get_with_env;
      Alcotest.test_case "action: field in two codecs" `Quick
        test_get_action_field_twocodecs;
      Alcotest.test_case "action: get without env" `Quick test_get_action_no_env;
      Alcotest.test_case "action: abort on get" `Quick
        test_get_action_abort_field;
      Alcotest.test_case "action: no action ignores env" `Quick
        test_get_noaction_ignores_env;
      Alcotest.test_case "action: multiple calls update env" `Quick
        test_get_action_multiple_calls;
      Alcotest.test_case "action: with input param" `Quick
        test_get_action_with_inputparam;
      Alcotest.test_case "action: input param needs an env" `Quick
        test_get_action_inputparam_noenv;
      Alcotest.test_case "embed: param-sized sub-codec forwards param" `Quick
        test_embed_param_sized;
      Alcotest.test_case "get: fixed embedded codec preserves param env" `Quick
        test_get_embed_param_fixed;
      Alcotest.test_case "embed: param sub-codec requires env" `Quick
        test_embed_param_requires_env;
      Alcotest.test_case "embed: sub-codec where enforced" `Quick
        test_embed_where_enforced;
      Alcotest.test_case "embed: output parameter forwarded" `Quick
        test_embed_output_param;
      Alcotest.test_case "embed: param forwarded through repeat" `Quick
        test_embed_param_repeat;
      Alcotest.test_case "action: output only" `Quick
        test_get_action_output_only;
      Alcotest.test_case "action: var then assign" `Quick
        test_get_action_varthen_assign;
      Alcotest.test_case "action: cross-field ref" `Quick
        test_get_action_crossfield_ref;
      Alcotest.test_case "validate: constraint only" `Quick
        test_validate_constraint_only;
      Alcotest.test_case "validate: where only" `Quick test_validate_where_only;
      Alcotest.test_case "action: output param sizes a later field" `Quick
        test_output_sizes_later_field;
      Alcotest.test_case "action: two staged same field" `Quick
        test_get_twostaged_same_field;
      Alcotest.test_case "shared: encode bitfield" `Quick
        test_encode_shared_bitfield;
      Alcotest.test_case "shared: same field two codecs get" `Quick
        test_same_field_two_codecs;
      Alcotest.test_case "shared: same field two codecs set" `Quick
        test_samefield_twocodecs_set;
      Alcotest.test_case "shared: same field two codecs decode" `Quick
        test_samefield_twocodecs_decode;
      Alcotest.test_case "shared: same field two codecs encode" `Quick
        test_samefield_twocodecs_encode;
      Alcotest.test_case "shared: same bitfield two codecs" `Quick
        test_same_bitfield_two_codecs;
      Alcotest.test_case "shared: staged before second seal" `Quick
        test_samefield_staged_before_secondseal;
    ] )
