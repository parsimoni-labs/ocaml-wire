(* Tests for Param module: typed parameter handles, runtime environments, and
   decode-time semantics. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

(* -- Param.input / Param.output / Param.decl -- *)

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s
let err_parse fmt = Fmt.kstr (fun message -> Error message) fmt

let string_error = function
  | Ok value -> Ok value
  | Error error -> err_parse "%a" pp_parse_error error

(* Render a one-field struct carrying [p] to 3D for substring assertions. *)
let render_param_3d p =
  to_3d
    (module_
       [ typedef (param_struct "T" [ Param.decl p ] [ field "x" uint8 ]) ])

let test_input_spec () =
  let output = render_param_3d (Param.input "limit" uint8) in
  Alcotest.(check bool)
    "contains UINT8 limit" true
    (contains ~sub:"UINT8 limit" output);
  Alcotest.(check bool) "not mutable" false (contains ~sub:"mutable" output)

let test_output_spec () =
  let output = render_param_3d (Param.output "out" uint16be) in
  Alcotest.(check bool) "contains mutable" true (contains ~sub:"mutable" output);
  Alcotest.(check bool) "contains out" true (contains ~sub:"out" output)

(* -- Param.bind / Param.get / Param.env -- *)

let test_input_binding () =
  let p = Param.input "limit" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x" ~constraint_:Expr.(Field.ref f_x <= Param.expr p) uint8
  in
  let c = Codec.v "InputBinding" (fun x -> x) Codec.[ (cf_x $ fun r -> r) ] in
  let env = Codec.env c |> Param.bind p 42 in
  Alcotest.(check int) "value" 42 (Param.get env p)

let test_wide_input_binding_error () =
  let p = Param.input "wide_limit" uint32 in
  let f_x = Field.v "x" uint8 in
  let c =
    Codec.v "WideInputBinding"
      ~where:Expr.(Field.ref f_x <= Param.expr p)
      (fun x -> x)
      Codec.[ f_x $ Fun.id ]
  in
  let high = Wire.Private.UInt32.be (Bytes.of_string "\x80\x00\x00\x00") 0 in
  if Sys.int_size > 32 then begin
    let env = Codec.env c |> Param.bind p high in
    Alcotest.(check int32)
      "wide value" Int32.min_int
      (Wire.Private.UInt32.to_int32 (Param.get env p))
  end
  else
    match Param.bind p high (Codec.env c) with
    | _ -> Alcotest.fail "expected an unfittable parameter error"
    | exception Invalid_argument msg ->
        Alcotest.(check bool)
          "names the parameter" true
          (contains ~sub:"wide_limit" msg)

let test_output_binding () =
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
      uint8
  in
  let c = Codec.v "OutputBinding" (fun x -> x) Codec.[ (cf_x $ fun r -> r) ] in
  let env = Codec.env c in
  (* output param starts at 0 *)
  Alcotest.(check int) "initial value" 0 (Param.get env out);
  (* after decode, output param is written by the action *)
  let buf = Bytes.of_string "\x07" in
  ignore (decode_ok (Codec.decode ~env c buf 0));
  Alcotest.(check int) "after decode" 7 (Param.get env out)

(* -- Input param visible to constraints (via Codec) -- *)

type bounded_record = { x : int }

let test_input_param_constraint () =
  let limit = Param.input "limit" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x" ~constraint_:Expr.(Field.ref f_x <= Param.expr limit) uint8
  in
  let c = Codec.v "Bounded" (fun x -> { x }) Codec.[ (cf_x $ fun r -> r.x) ] in
  (* limit=10, x=5: passes *)
  let buf = Bytes.of_string "\x05" in
  let env = Codec.env c |> Param.bind limit 10 in
  let r = decode_ok (Codec.decode ~env c buf 0) in
  Alcotest.(check int) "x" 5 r.x;
  (* limit=3, x=5: fails *)
  let env = Codec.env c |> Param.bind limit 3 in
  expect_constraint_fail (Codec.decode ~env c buf 0)

(* -- Output param written by action (via Codec) -- *)

let test_output_param_action () =
  let out = Param.output "out" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
      uint8
  in
  let c = Codec.v "Writer" (fun x -> { x }) Codec.[ (cf_x $ fun r -> r.x) ] in
  let buf = Bytes.of_string "\x2A" in
  let env = Codec.env c in
  ignore (decode_ok (Codec.decode ~env c buf 0));
  Alcotest.(check int) "out" 42 (Param.get env out)

let test_output_param_computed () =
  let out = Param.output "out" uint16be in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:
        (Action.on_success [ Action.assign out Expr.(Field.ref f_x * int 2) ])
      uint8
  in
  let c = Codec.v "Computed" (fun x -> { x }) Codec.[ (cf_x $ fun r -> r.x) ] in
  let buf = Bytes.of_string "\x15" in
  let env = Codec.env c in
  ignore (decode_ok (Codec.decode ~env c buf 0));
  Alcotest.(check int) "out" 42 (Param.get env out)

(* -- Where clause with params (via Codec) -- *)

type bounded_value = { bv_value : int }

let test_where_clause_pass () =
  let max_val = Param.input "max_val" uint16be in
  let f_value = Field.v "value" uint16be in
  let cf_value = Field.v "value" uint16be in
  let c =
    Codec.v "Bounded"
      ~where:Expr.(Field.ref f_value <= Param.expr max_val)
      (fun value -> { bv_value = value })
      Codec.[ (cf_value $ fun r -> r.bv_value) ]
  in
  (* max_val=100, value=50: passes *)
  let buf = Bytes.of_string "\x00\x32" in
  let env = Codec.env c |> Param.bind max_val 100 in
  let r = decode_ok (Codec.decode ~env c buf 0) in
  Alcotest.(check int) "value" 50 r.bv_value

let test_where_clause_fail () =
  let max_val = Param.input "max_val" uint16be in
  let f_value = Field.v "value" uint16be in
  let cf_value = Field.v "value" uint16be in
  let c =
    Codec.v "Bounded"
      ~where:Expr.(Field.ref f_value <= Param.expr max_val)
      (fun value -> { bv_value = value })
      Codec.[ (cf_value $ fun r -> r.bv_value) ]
  in
  (* max_val=10, value=50: where clause fails *)
  let buf = Bytes.of_string "\x00\x32" in
  let env = Codec.env c |> Param.bind max_val 10 in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error { kind = Constraint_failed { which = Where; _ }; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_bind_by_name () =
  (* [bind_by_name] binds an input param by name, without the typed handle, and
     drives the where clause exactly as the typed [bind] does. Used by the
     differential harness, which has the codec but not the param handles. *)
  let max_val = Param.input "max_val" uint16be in
  let f_value = Field.v "value" uint16be in
  let cf_value = Field.v "value" uint16be in
  let c =
    Codec.v "Bounded"
      ~where:Expr.(Field.ref f_value <= Param.expr max_val)
      (fun value -> { bv_value = value })
      Codec.[ (cf_value $ fun r -> r.bv_value) ]
  in
  let buf = Bytes.of_string "\x00\x32" in
  (* max_val=100 (>= 50): accepts *)
  let env = Codec.env c |> Param.bind_by_name "max_val" 100 in
  Alcotest.(check int)
    "value" 50 (decode_ok (Codec.decode ~env c buf 0)).bv_value;
  (* max_val=10 (< 50): the where clause fails *)
  let env = Codec.env c |> Param.bind_by_name "max_val" 10 in
  (match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error { kind = Constraint_failed { which = Where; _ }; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e);
  (* an unreferenced name is a no-op, not an error *)
  let env =
    Codec.env c |> Param.bind_by_name "nope" 7 |> Param.bind max_val 100
  in
  Alcotest.(check int)
    "ignores unknown name" 50 (decode_ok (Codec.decode ~env c buf 0)).bv_value

(* -- Mixed input + output params (via Codec) -- *)

type mixed_record = { a : int; b : int }

let test_mixed_params () =
  let max_val = Param.input "max_val" uint8 in
  let out_sum = Param.output "out_sum" uint8 in
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint8 in
  let cf_a =
    Field.v "a"
      ~action:(Action.on_success [ Action.assign out_sum (Field.ref f_a) ])
      uint8
  in
  let cf_b =
    Field.v "b"
      ~action:
        (Action.on_success
           [ Action.assign out_sum Expr.(Param.expr out_sum + Field.ref f_b) ])
      uint8
  in
  let c =
    Codec.v "Mixed"
      ~where:Expr.(Param.expr out_sum <= Param.expr max_val)
      (fun a b -> { a; b })
      Codec.[ (cf_a $ fun r -> r.a); (cf_b $ fun r -> r.b) ]
  in
  (* a=10, b=20 => out_sum=30, max_val=50 => 30 <= 50: OK *)
  let buf = Bytes.of_string "\x0A\x14" in
  let env = Codec.env c |> Param.bind max_val 50 in
  ignore (decode_ok (Codec.decode ~env c buf 0));
  Alcotest.(check int) "out_sum" 30 (Param.get env out_sum);
  (* a=10, b=20 => out_sum=30, max_val=20 => 30 > 20: FAIL *)
  let env = Codec.env c |> Param.bind max_val 20 in
  expect_constraint_fail (Codec.decode ~env c buf 0)

(* -- Codec.decode ~env:params with -- *)

type record_with_param = { x : int }

let test_codec_param_decode () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign outx (Field.ref f_x) ])
      uint8
  in
  let c =
    Codec.v "ParamCodec"
      ~where:Expr.(Field.ref f_x <= Param.expr limit)
      (fun x -> { x })
      Codec.[ (cf_x $ fun r -> r.x) ]
  in
  let buf = Bytes.of_string "\x05" in
  let env = Codec.env c |> Param.bind limit 10 in
  let v = decode_ok (Codec.decode ~env c buf 0) in
  Alcotest.(check int) "x" 5 v.x

let test_codec_param_where_fail () =
  let limit = Param.input "limit" uint8 in
  let outx = Param.output "outx" uint8 in
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Field.v "x"
      ~action:(Action.on_success [ Action.assign outx (Field.ref f_x) ])
      uint8
  in
  let c =
    Codec.v "ParamCodecFail"
      ~where:Expr.(Field.ref f_x <= Param.expr limit)
      (fun x -> { x })
      Codec.[ (cf_x $ fun r -> r.x) ]
  in
  let buf = Bytes.of_string "\x05" in
  let env = Codec.env c |> Param.bind limit 3 in
  match Codec.decode ~env c buf 0 with
  | Error { kind = Constraint_failed { which = Where; _ }; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

(* -- 3D rendering -- *)

let test_3d_rendering () =
  let limit = Param.input "limit" uint16be in
  let out = Param.output "out" uint32be in
  let f_x = Field.v "x" uint16be in
  let f_limit = Field.v "limit" uint16be in
  let s =
    param_struct "Rendered"
      [ Param.decl limit; Param.decl out ]
      ~where:Expr.(Field.ref f_x <= Field.ref f_limit)
      [
        field "x"
          ~action:(Action.on_success [ Action.assign out (Field.ref f_x) ])
          uint16be;
      ]
  in
  let m = module_ [ typedef ~entrypoint:true s ] in
  let output = to_3d m in
  Alcotest.(check bool)
    "contains UINT16BE limit" true
    (Re.execp (Re.compile (Re.str "UINT16BE limit")) output);
  Alcotest.(check bool)
    "contains mutable UINT32BE *out" true
    (Re.execp (Re.compile (Re.str "mutable")) output);
  (* The struct-level [where] referencing fields is lowered to a field-level
     [{ ... }] constraint -- 3D's struct [where] only sees parameters. *)
  Alcotest.(check bool)
    "contains lowered where expression" true
    (Re.execp (Re.compile (Re.str "x <= limit")) output);
  Alcotest.(check bool)
    "contains on-success" true
    (Re.execp (Re.compile (Re.str ":on-success")) output);
  Alcotest.(check bool)
    "contains entrypoint" true
    (Re.execp (Re.compile (Re.str "entrypoint")) output)

(* -- No params (default behavior) -- *)

let test_no_params () =
  let s = struct_ "Simple" [ field "x" uint8 ] in
  ignore (decode_ok (of_string (struct_typ s) "\x42"))

(* -- Param-driven size -- *)

type param_size_record = { data : string; tag : int }

let ps_size_param = Param.input "data_size" uint8

let param_size_codec =
  Codec.v "ParamSize"
    (fun data tag -> { data; tag })
    Codec.
      [
        ( Field.v "Data" (byte_array ~size:(Param.expr ps_size_param)) $ fun r ->
          r.data );
        (Field.v "Tag" uint8 $ fun r -> r.tag);
      ]

let test_param_size_decode () =
  let env = Codec.env param_size_codec |> Param.bind ps_size_param 4 in
  let buf = Bytes.create 5 in
  Bytes.blit_string "ABCD" 0 buf 0 4;
  Bytes.set_uint8 buf 4 0xFF;
  let r = decode_ok (Codec.decode ~env param_size_codec buf 0) in
  Alcotest.(check string) "data" "ABCD" r.data;
  Alcotest.(check int) "tag" 0xFF r.tag

let test_param_size_different_sizes () =
  (* Same codec, different param values *)
  let env2 = Codec.env param_size_codec |> Param.bind ps_size_param 2 in
  let buf2 = Bytes.create 3 in
  Bytes.blit_string "XY" 0 buf2 0 2;
  Bytes.set_uint8 buf2 2 0xAA;
  let r2 = decode_ok (Codec.decode ~env:env2 param_size_codec buf2 0) in
  Alcotest.(check string) "data 2" "XY" r2.data;
  Alcotest.(check int) "tag 2" 0xAA r2.tag;
  let env8 = Codec.env param_size_codec |> Param.bind ps_size_param 8 in
  let buf8 = Bytes.create 9 in
  Bytes.blit_string "12345678" 0 buf8 0 8;
  Bytes.set_uint8 buf8 8 0xBB;
  let r8 = decode_ok (Codec.decode ~env:env8 param_size_codec buf8 0) in
  Alcotest.(check string) "data 8" "12345678" r8.data;
  Alcotest.(check int) "tag 8" 0xBB r8.tag

let test_param_size_bind_by_name () =
  (* A param-driven size resolves through the field reader (the cell), not the
     [where]/constraint path. [bind_by_name] must drive it exactly as the typed
     [bind] does, otherwise the size reads as 0 and the field is truncated. *)
  let env = Codec.env param_size_codec |> Param.bind_by_name "data_size" 4 in
  let buf = Bytes.create 5 in
  Bytes.blit_string "ABCD" 0 buf 0 4;
  Bytes.set_uint8 buf 4 0xFF;
  let r = decode_ok (Codec.decode ~env param_size_codec buf 0) in
  Alcotest.(check string) "data" "ABCD" r.data;
  Alcotest.(check int) "tag" 0xFF r.tag;
  (* same codec, a different bound size *)
  let env2 = Codec.env param_size_codec |> Param.bind_by_name "data_size" 2 in
  let buf2 = Bytes.create 3 in
  Bytes.blit_string "XY" 0 buf2 0 2;
  Bytes.set_uint8 buf2 2 0xAA;
  let r2 = decode_ok (Codec.decode ~env:env2 param_size_codec buf2 0) in
  Alcotest.(check string) "data 2" "XY" r2.data;
  Alcotest.(check int) "tag 2" 0xAA r2.tag

let test_decode_rejects_unbound_param () =
  (* An input param that drives a field size must be bound before decode, the
     same precondition encode enforces. Decoding without an env, or with an env
     that left the param unbound, would resolve the size to 0 and silently
     truncate the field, so decode raises [Invalid_argument] up front. *)
  let buf = Bytes.create 5 in
  Bytes.blit_string "ABCD" 0 buf 0 4;
  Bytes.set_uint8 buf 4 0xFF;
  (* no env at all *)
  Alcotest.check_raises "decode without env"
    (Invalid_argument
       "Codec.decode: codec ParamSize has input params; pass ?env (e.g. \
        [Codec.env c |> Param.bind p N]).") (fun () ->
      ignore (Codec.decode param_size_codec buf 0));
  (* env present but the param left unbound *)
  let env = Codec.env param_size_codec in
  Alcotest.check_raises "decode with unbound param"
    (Invalid_argument
       "Codec.decode: codec ParamSize has unbound input params [data_size]; \
        bind every one before use.") (fun () ->
      ignore (Codec.decode ~env param_size_codec buf 0))

let test_param_size_zero () =
  let env = Codec.env param_size_codec |> Param.bind ps_size_param 0 in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xFF;
  let r = decode_ok (Codec.decode ~env param_size_codec buf 0) in
  Alcotest.(check string) "data" "" r.data;
  Alcotest.(check int) "tag" 0xFF r.tag

let test_param_size_reentrant_codec () =
  let size = Param.input "reentrant_size" uint8 in
  let codec_ref = ref None in
  let inner_env_ref = ref None in
  let inside = ref false in
  let reenter f =
    if not !inside then begin
      inside := true;
      Fun.protect
        ~finally:(fun () -> inside := false)
        (fun () -> f (Option.get !codec_ref) (Option.get !inner_env_ref))
    end
  in
  let trigger =
    map uint8
      ~encode:(fun v ->
        reenter (fun codec env ->
            let buf = Bytes.create 2 in
            Codec.encode ~env codec (7, "Z") buf 0);
        v)
      ~decode:(fun v ->
        reenter (fun codec env ->
            ignore
              (decode_ok (Codec.decode ~env codec (Bytes.of_string "\x07Z") 0)));
        v)
  in
  let codec =
    Codec.v "ReentrantParam"
      (fun tag data -> (tag, data))
      Codec.
        [
          Field.v "Tag" trigger $ fst;
          Field.v "Data" (byte_array ~size:(Param.expr size)) $ snd;
        ]
  in
  codec_ref := Some codec;
  let inner_env = Codec.env codec |> Param.bind size 1 in
  inner_env_ref := Some inner_env;
  let outer_env = Codec.env codec |> Param.bind size 2 in
  let buf = Bytes.create 3 in
  Codec.encode ~env:outer_env codec (9, "AB") buf 0;
  Alcotest.(check bytes)
    "outer encode keeps its size" (Bytes.of_string "\x09AB") buf;
  Alcotest.(check (pair int string))
    "outer decode keeps its size" (9, "AB")
    (decode_ok (Codec.decode ~env:outer_env codec buf 0))

let test_param_size_in_casetype () =
  (* Casetype dispatch must preserve the outer encode context when its selected
     body is an embedded codec. Otherwise the inner parameter lookup falls back
     to the unbound sentinel and a param-sized byte field rejects its value. *)
  let size = Param.input "case_size" uint8 in
  let inner =
    Codec.v "ParamCaseBody" Fun.id
      Codec.[ Field.v "data" (byte_array ~size:(Param.expr size)) $ Fun.id ]
  in
  let typ =
    casetype "ParamCase" uint8
      [
        case ~index:1 (codec inner)
          ~inject:(fun value -> `Body value)
          ~project:(function `Body value -> Some value);
      ]
  in
  let outer =
    Codec.v "ParamCaseOuter" Fun.id Codec.[ Field.v "case" typ $ Fun.id ]
  in
  let env = Codec.env outer |> Param.bind size 2 in
  let buf = Bytes.create 3 in
  Codec.encode ~env outer (`Body "AB") buf 0;
  Alcotest.(check bytes) "encoded casetype" (Bytes.of_string "\x01AB") buf;
  Alcotest.(check (result string string))
    "decoded casetype" (Ok "AB")
    (match Codec.decode ~env outer buf 0 with
    | Ok (`Body value) -> Ok value
    | Error error -> err_parse "%a" pp_parse_error error)

let test_param_through_typ_wrappers () =
  (* Fixed-size typ wrappers use the scalar fast path. That path must carry the
     outer context through to an embedded codec's parameterized constraint. *)
  let limit = Param.input "wrapped_limit" uint8 in
  let value = Field.v "value" uint8 in
  let inner =
    Codec.v "ParamWrappedBody" Fun.id
      ~where:Expr.(Field.ref value <= Param.expr limit)
      Codec.[ value $ Fun.id ]
  in
  let wrapped = where Expr.true_ (codec inner) in
  let field = Field.optional "body" ~present:Expr.true_ wrapped in
  let outer = Codec.v "ParamWrappedOuter" Fun.id Codec.[ field $ Fun.id ] in
  let env = Codec.env outer |> Param.bind limit 100 in
  let buf = Bytes.of_string "\x2d" in
  Alcotest.(check (result (option int) string))
    "decoded wrapped codec" (Ok (Some 45))
    (string_error (Codec.decode ~env outer buf 0));
  Codec.encode ~env outer (Some 45) buf 0;
  Alcotest.(check bytes) "encoded wrapped codec" (Bytes.of_string "\x2d") buf

(* -- Concurrent decode -- *)

type barrier = {
  parties : int;
  arrived : int Atomic.t;
  generation : int Atomic.t;
}

let barrier parties =
  { parties; arrived = Atomic.make 0; generation = Atomic.make 0 }

let await barrier =
  let generation = Atomic.get barrier.generation in
  if Atomic.fetch_and_add barrier.arrived 1 = barrier.parties - 1 then begin
    Atomic.set barrier.arrived 0;
    Atomic.incr barrier.generation
  end
  else
    while Atomic.get barrier.generation = generation do
      Domain.cpu_relax ()
    done

let test_multi_domain_decode () =
  (* The mapped fields rendezvous during validation, after their values have
     been read but before they are written into the codec's scratch slots. If
     two domains share that scratch, one domain must either fail [b = a] or
     copy the other domain's output parameter. Independent scratch and eval
     contexts let both decodes complete with their own values. *)
  let validation_barrier = barrier 2 in
  let synchronized_uint8 =
    map uint8 ~decode:Fun.id ~encode:(fun value ->
        await validation_barrier;
        value)
  in
  let limit = Param.input "domain_limit" uint8 in
  let observed = Param.output "domain_observed" uint8 in
  let f_a = Field.v "a" synchronized_uint8 in
  let f_b = Field.v "b" synchronized_uint8 in
  let checked_b =
    Field.v "b" synchronized_uint8
      ~constraint_:Expr.(Field.ref f_b = Field.ref f_a)
      ~action:(Action.on_success [ Action.assign observed (Field.ref f_b) ])
  in
  let codec =
    Codec.v "ConcurrentParams"
      ~where:Expr.(Field.ref f_b <= Param.expr limit)
      (fun a b -> (a, b))
      Codec.[ f_a $ fst; checked_b $ snd ]
  in
  let worker value =
    let env = Codec.env codec |> Param.bind limit value in
    let buf = Bytes.make 2 (Char.chr value) in
    let ok = ref true in
    for _ = 1 to 1_000 do
      match Codec.decode ~env codec buf 0 with
      | Ok decoded ->
          if decoded <> (value, value) || Param.get env observed <> value then
            ok := false
      | Error _ -> ok := false
    done;
    !ok
  in
  let first = Domain.spawn (fun () -> worker 0x11) in
  let second = Domain.spawn (fun () -> worker 0x22) in
  Alcotest.(check bool) "first domain" true (Domain.join first);
  Alcotest.(check bool) "second domain" true (Domain.join second)

(* -- Suite -- *)

let suite =
  ( "param",
    [
      (* construction *)
      Alcotest.test_case "input spec" `Quick test_input_spec;
      Alcotest.test_case "output spec" `Quick test_output_spec;
      (* bindings *)
      Alcotest.test_case "input binding" `Quick test_input_binding;
      Alcotest.test_case "wide input binding error" `Quick
        test_wide_input_binding_error;
      Alcotest.test_case "output binding" `Quick test_output_binding;
      (* runtime: input params *)
      Alcotest.test_case "input param constraint" `Quick
        test_input_param_constraint;
      (* runtime: output params *)
      Alcotest.test_case "output param action" `Quick test_output_param_action;
      Alcotest.test_case "output param computed" `Quick
        test_output_param_computed;
      (* where clause *)
      Alcotest.test_case "where clause pass" `Quick test_where_clause_pass;
      Alcotest.test_case "where clause fail" `Quick test_where_clause_fail;
      Alcotest.test_case "bind_by_name" `Quick test_bind_by_name;
      (* mixed *)
      Alcotest.test_case "mixed input + output" `Quick test_mixed_params;
      (* codec *)
      Alcotest.test_case "codec decode with params" `Quick
        test_codec_param_decode;
      Alcotest.test_case "codec where fail" `Quick test_codec_param_where_fail;
      (* 3D rendering *)
      Alcotest.test_case "3D rendering" `Quick test_3d_rendering;
      (* default *)
      Alcotest.test_case "no params" `Quick test_no_params;
      (* param-driven size *)
      Alcotest.test_case "param: size decode" `Quick test_param_size_decode;
      Alcotest.test_case "param: size bind_by_name" `Quick
        test_param_size_bind_by_name;
      Alcotest.test_case "param: decode rejects unbound" `Quick
        test_decode_rejects_unbound_param;
      Alcotest.test_case "param: different sizes" `Quick
        test_param_size_different_sizes;
      Alcotest.test_case "param: size zero" `Quick test_param_size_zero;
      Alcotest.test_case "param: re-entrant codec" `Quick
        test_param_size_reentrant_codec;
      Alcotest.test_case "param: size inside casetype" `Quick
        test_param_size_in_casetype;
      Alcotest.test_case "param: context through typ wrappers" `Quick
        test_param_through_typ_wrappers;
      Alcotest.test_case "param: multi-domain decode" `Quick
        test_multi_domain_decode;
    ] )
