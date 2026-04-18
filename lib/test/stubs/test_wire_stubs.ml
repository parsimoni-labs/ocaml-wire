(** Tests for Wire_stubs -- OCaml FFI stub generation.

    Every test that generates ML stubs compiles them with ocamlfind to catch
    arity mismatches, syntax errors, and type errors in the generated code. *)

open Wire
open Wire.Everparse.Raw

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(** Compile a generated ML stub to verify it is syntactically and type-valid. *)
let compile_ml_stub ml =
  let dir = Filename.temp_dir "wire_stub_test" "" in
  let ml_path = Filename.concat dir "stub.ml" in
  let cmi_path = Filename.concat dir "stub.cmi" in
  let oc = open_out ml_path in
  output_string oc ml;
  close_out oc;
  let cmd =
    Fmt.str "ocamlfind ocamlc -package wire -c %s -o %s 2>&1" ml_path cmi_path
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  (try Sys.remove ml_path with Sys_error _ -> ());
  (try Sys.remove cmi_path with Sys_error _ -> ());
  (try
     Sys.remove
       (Filename.concat dir
          (Filename.chop_extension (Filename.basename ml_path) ^ ".cmo"))
   with Sys_error _ -> ());
  (try Sys.rmdir dir with Sys_error _ | Unix.Unix_error _ -> ());
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.failf "stub compilation failed:\n%s\nGenerated:\n%s" output ml

(** Generate both to_ml_stub and to_ml_stubs, compile both, check agreement. *)
let check_struct s =
  let single = Wire_stubs.to_ml_stub s in
  let multi = Wire_stubs.to_ml_stubs [ s ] in
  compile_ml_stub single;
  compile_ml_stub multi;
  Alcotest.(check bool) "single has parse" true (contains ~sub:"parse" single);
  Alcotest.(check bool) "multi has parse" true (contains ~sub:"_parse" multi)

(* -- No params -- *)

let test_no_params () =
  check_struct
    (struct_ "SimpleHeader"
       [ field "version" uint8; field "length" uint16; field "flags" uint8 ])

let test_single_field () = check_struct (struct_ "Tiny" [ field "x" uint8 ])

let test_bitfield_struct () =
  check_struct
    (struct_ "BitFields"
       [
         field "a" (bits ~width:3 U8);
         field "b" (bits ~width:5 U8);
         field "c" (bits ~width:16 U32be);
       ])

let test_many_fields () =
  check_struct
    (struct_ "Wide"
       [
         field "a" uint8;
         field "b" uint16be;
         field "c" uint32be;
         field "d" uint8;
         field "e" uint16be;
         field "f" uint32be;
         field "g" uint8;
         field "h" uint16be;
       ])

(* -- Input params only -- *)

let test_one_input () =
  check_struct
    (param_struct "OneInput" [ param "limit" uint8 ] [ field "x" uint8 ])

let test_two_inputs () =
  check_struct
    (param_struct "TwoInputs"
       [ param "lo" uint8; param "hi" uint16be ]
       [ field "x" uint8 ])

(* -- Output params only -- *)

let test_one_output () =
  check_struct
    (param_struct "OneOutput"
       [ mutable_param "out" uint16be ]
       [ field "x" uint8 ])

let test_two_outputs () =
  check_struct
    (param_struct "TwoOutputs"
       [ mutable_param "out_a" uint8; mutable_param "out_b" uint16be ]
       [ field "x" uint8 ])

(* -- Mixed input + output -- *)

let test_mixed () =
  check_struct
    (param_struct "Mixed"
       [ param "max_len" uint16be; mutable_param "out_len" uint16be ]
       [ field "Length" uint16be ])

let test_mixed_many () =
  check_struct
    (param_struct "MixedMany"
       [
         param "a" uint8;
         param "b" uint8;
         mutable_param "out_a" uint8;
         mutable_param "out_b" uint16be;
       ]
       [ field "x" uint8 ])

(* -- >5 args (bytecode path) -- *)

let test_many_params_bytecode () =
  check_struct
    (param_struct "ManyParams"
       [
         param "a" uint8;
         param "b" uint8;
         param "c" uint8;
         param "d" uint8;
         param "e" uint8;
         mutable_param "out" uint8;
       ]
       [ field "x" uint8 ])

let test_exactly_five_args () =
  (* params are ignored in parse stubs -- always bytes -> 'a *)
  let s =
    param_struct "FiveArgs"
      [ param "a" uint8; param "b" uint8; param "c" uint8; param "d" uint8 ]
      [ field "x" uint8 ]
  in
  check_struct s

let test_six_args () =
  (* params are ignored in parse stubs -- always bytes -> 'a *)
  let s =
    param_struct "SixArgs"
      [
        param "a" uint8;
        param "b" uint8;
        param "c" uint8;
        param "d" uint8;
        param "e" uint8;
      ]
      [ field "x" uint8 ]
  in
  check_struct s

(* -- Bitfield params -- *)

let test_bitfield_param () =
  check_struct
    (param_struct "BfParam"
       [ param "mask" (bits ~width:4 U8) ]
       [ field "v" uint8 ])

(* -- Multiple structs in one to_ml_stubs call -- *)

let test_multiple_structs () =
  let s1 = struct_ "Foo" [ field "x" uint8 ] in
  let s2 = struct_ "Bar" [ field "y" uint16be ] in
  let s3 = param_struct "Baz" [ param "limit" uint8 ] [ field "z" uint32be ] in
  let ml = Wire_stubs.to_ml_stubs [ s1; s2; s3 ] in
  compile_ml_stub ml;
  Alcotest.(check bool) "has foo" true (contains ~sub:"foo_parse" ml);
  Alcotest.(check bool) "has bar" true (contains ~sub:"bar_parse" ml);
  Alcotest.(check bool) "has baz" true (contains ~sub:"baz_parse" ml)

(* -- C stubs (string checks, cannot compile without EverParse headers) -- *)

let test_c_stubs_no_params () =
  let s =
    struct_ "SimpleHeader"
      [ field "version" uint8; field "length" uint16; field "flags" uint8 ]
  in
  let c = Wire_stubs.to_c_stubs [ s ] in
  Alcotest.(check bool)
    "has parse" true
    (contains ~sub:"caml_wire_simpleheader_parse" c);
  Alcotest.(check bool)
    "has error handler" true
    (contains ~sub:"simpleheader_err" c);
  Alcotest.(check bool) "has EverParse.h" true (contains ~sub:"EverParse.h" c)

let test_c_stubs_with_params () =
  let s =
    param_struct "Bounded"
      [ param "max_len" uint16be; mutable_param "out_len" uint16be ]
      [ field "Length" uint16be ]
  in
  let c = Wire_stubs.to_c_stubs [ s ] in
  Alcotest.(check bool)
    "has parse" true
    (contains ~sub:"caml_wire_bounded_parse" c);
  Alcotest.(check bool) "has WIRECTX" true (contains ~sub:"WIRECTX" c);
  Alcotest.(check bool) "has error handler" true (contains ~sub:"bounded_err" c)

let test_c_stubs_many_params () =
  let s =
    param_struct "ManyParams"
      [
        param "a" uint8;
        param "b" uint8;
        param "c" uint8;
        param "d" uint8;
        param "e" uint8;
        mutable_param "out" uint8;
      ]
      [ field "x" uint8 ]
  in
  let c = Wire_stubs.to_c_stubs [ s ] in
  Alcotest.(check bool)
    "has parse" true
    (contains ~sub:"caml_wire_manyparams_parse" c);
  Alcotest.(check bool) "has WIRECTX" true (contains ~sub:"WIRECTX" c)

(* -- to_ml_stub_name -- *)

let test_name_camel () =
  Alcotest.(check string)
    "camel" "simple_header"
    (Wire_stubs.to_ml_stub_name (struct_ "SimpleHeader" [ field "x" uint8 ]))

let test_name_allcaps () =
  Alcotest.(check string)
    "allcaps" "c_l_c_w"
    (Wire_stubs.to_ml_stub_name (struct_ "CLCW" [ field "x" uint8 ]))

let test_name_single () =
  Alcotest.(check string)
    "single" "foo"
    (Wire_stubs.to_ml_stub_name (struct_ "Foo" [ field "x" uint8 ]))

(* -- End-to-end: generate -> EverParse -> compile C+ML -> call -- *)

let has_3d = Wire_3d.has_3d_exe ()

let run cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 -> ()
  | _ -> Alcotest.failf "command failed: %s\n%s" cmd output

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

(** End-to-end: .3d -> EverParse -> C stubs -> ML stubs -> compile -> call.
    [test_ml] is the OCaml source that calls the generated stubs and exits 0 on
    success, non-zero on failure. *)
let e2e ~name ~structs ~module_ ~test_ml =
  if not has_3d then ()
  else begin
    let dir = Filename.temp_dir ("wire_e2e_" ^ name) "" in
    (* 1. Generate .3d and run EverParse *)
    let schema =
      Wire.Everparse.Raw.of_module ~name ~module_ ~wire_size:0
      (* wire_size unused for generation *)
    in
    Wire_3d.generate_3d ~outdir:dir [ schema ];
    Wire_3d.run_everparse ~outdir:dir [ schema ];
    (* 2. Generate C and ML stubs *)
    Wire_stubs.of_structs ~schema_dir:dir ~outdir:dir structs;
    write_file (Filename.concat dir "main.ml") test_ml;
    (* 3. Compile *)
    let cmd =
      Fmt.str
        "cd %s && ocamlfind ocamlopt -package wire,wire.stubs -linkpkg -ccopt \
         '-I %s' wire_ffi.c stubs.ml main.ml -o test_e2e 2>&1"
        dir dir
    in
    run cmd;
    (* 4. Run *)
    run (Filename.concat dir "test_e2e")
  end

let test_e2e_no_params () =
  let f_version = Wire.Field.v "version" uint8 in
  let f_length = Wire.Field.v "length" uint16be in
  let codec =
    let open Wire.Codec in
    v "TestHeader"
      (fun version length -> (version, length))
      [ (f_version $ fun (v, _) -> v); (f_length $ fun (_, l) -> l) ]
  in
  let schema = Wire.Everparse.schema codec in
  let s = Wire.Everparse.struct_of_codec codec in
  e2e ~name:"TestHeader" ~structs:[ s ] ~module_:schema.module_
    ~test_ml:
      {|let () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 42;
  let r = Stubs.testheader_parse buf 0 in
  assert (r.Stubs.version = 1);
  assert (r.Stubs.length = 42)
|}

let test_e2e_with_constraint () =
  let f_x_ref = Wire.Field.v "x" uint8 in
  let f_x =
    Wire.Field.v "x" ~constraint_:Expr.(Wire.Field.ref f_x_ref <= int 100) uint8
  in
  let codec =
    let open Wire.Codec in
    v "Constrained" (fun x -> x) [ (f_x $ fun x -> x) ]
  in
  let schema = Wire.Everparse.schema codec in
  let s = Wire.Everparse.struct_of_codec codec in
  e2e ~name:"Constrained" ~structs:[ s ] ~module_:schema.module_
    ~test_ml:
      {|let () =
  let buf = Bytes.create 1 in
  (* x=50 <= 100: passes *)
  Bytes.set_uint8 buf 0 50;
  let r = Stubs.constrained_parse buf 0 in
  assert (r.Stubs.x = 50);
  (* x=200 > 100: fails -- parse raises *)
  Bytes.set_uint8 buf 0 200;
  (try ignore (Stubs.constrained_parse buf 0); assert false
   with Failure _ -> ())
|}

let test_e2e_bitfields () =
  (* Adversarial: drive the EverParse-generated C parser on a real IPv4-style
     byte [0x45] and check that Wire's default [bit_order = Msb_first] makes
     [version] land at the top nibble (=4) and [flags] at the bottom (=5).
     This is the end-to-end interop witness: if Wire and EverParse disagree
     on bit placement, this test breaks. *)
  let f_version = Wire.Field.v "version" (bits ~width:4 U8) in
  let f_flags = Wire.Field.v "flags" (bits ~width:4 U8) in
  let f_length = Wire.Field.v "length" uint16be in
  let codec =
    let open Wire.Codec in
    v "BfHeader"
      (fun version flags length -> (version, flags, length))
      [
        (f_version $ fun (v, _, _) -> v);
        (f_flags $ fun (_, f, _) -> f);
        (f_length $ fun (_, _, l) -> l);
      ]
  in
  let schema = Wire.Everparse.schema codec in
  let s = Wire.Everparse.struct_of_codec codec in
  e2e ~name:"BfHeader" ~structs:[ s ] ~module_:schema.module_
    ~test_ml:
      {|let () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 0x45;
  Bytes.set_uint16_be buf 1 100;
  let r = Stubs.bfheader_parse buf 0 in
  (* Default Msb_first: version = top nibble (4), flags = bottom nibble (5). *)
  assert (r.Stubs.version = 4);
  assert (r.Stubs.flags = 5);
  assert (r.Stubs.length = 100)
|}

(* -- Output pattern invariants --

   Structural tests over the post-[with_output] module. Every assertion
   inspects typed AST data returned by the public [Wire.Everparse] API so
   cosmetic output format changes don't break the tests and real regressions
   are caught precisely. *)

let pp_action_form = function
  | Wire.Everparse.No_action -> "none"
  | On_act -> ":act"
  | On_success -> ":on-success"

let expected_action_form ~named ~is_bitfield =
  if not named then Wire.Everparse.No_action
  else if is_bitfield then Wire.Everparse.On_act
  else Wire.Everparse.On_success

(* Invariant 1: every named field in the output module carries an action
   whose form matches the field's kind (bitfield -> :act, otherwise
   :on-success). Anonymous fields carry no action. *)
let check_action_forms codec =
  let schema = Wire.Everparse.schema codec in
  let ep =
    match Wire.Everparse.entrypoint_struct schema with
    | Some st -> st
    | None -> Alcotest.failf "%s: no entrypoint struct" schema.name
  in
  List.iter
    (fun (name, is_bitfield, form) ->
      let expected =
        expected_action_form ~named:(Option.is_some name) ~is_bitfield
      in
      if form <> expected then
        Alcotest.failf "%s field %s: expected %s, got %s" schema.name
          (Option.value name ~default:"<anon>")
          (pp_action_form expected) (pp_action_form form))
    (Wire.Everparse.field_action_forms ep)

let plug_field_names s =
  List.map (fun f -> f.Wire.Everparse.pf_name) (Wire.Everparse.plug_fields s)

(* Invariant 2: [plug_fields] enumerates exactly the named fields of the
   source struct in declaration order, with consecutive indices starting at
   0. Catches indexing drift between the schema pipeline and the plug
   generator. *)
let check_plug_completeness codec =
  let schema = Wire.Everparse.schema codec in
  let source = Option.get schema.source in
  let expected = Wire.Everparse.Raw.field_names source in
  let got = plug_field_names schema in
  Alcotest.(check (list string))
    "plug_fields matches named source fields" expected got;
  let idxs =
    List.map
      (fun f -> f.Wire.Everparse.pf_idx)
      (Wire.Everparse.plug_fields schema)
  in
  let expected_idxs = List.init (List.length idxs) Fun.id in
  Alcotest.(check (list int)) "indices consecutive from 0" expected_idxs idxs

(* Invariant 3: setter names are namespaced per schema; two schemas' setter
   sets never collide. *)
let check_setter_scoping codec_a codec_b =
  let sa = Wire.Everparse.schema codec_a in
  let sb = Wire.Everparse.schema codec_b in
  let names s = List.map fst (Wire.Everparse.plug_setters s) in
  let na = names sa in
  let nb = names sb in
  let intersection =
    List.filter (fun n -> List.mem n nb) na |> List.sort_uniq String.compare
  in
  Alcotest.(check (list string))
    "no shared setter names across schemas" [] intersection;
  List.iter
    (fun n ->
      if not (String.starts_with ~prefix:sa.name n) then
        Alcotest.failf "setter %s missing schema prefix %s" n sa.name)
    na

(* Invariant 4: every pf_setter appears in plug_setters, and every setter in
   plug_setters is actually declared in the module as an extern_fn. Catches
   drift between what plug_fields claims and what the 3D module declares. *)
let check_setter_declarations codec =
  let schema = Wire.Everparse.schema codec in
  let declared_externs = Wire.Everparse.extern_fn_names schema in
  List.iter
    (fun (setter, _) ->
      if not (List.mem setter declared_externs) then
        Alcotest.failf
          "plug_setters claims %s but module has no Extern_fn for it" setter)
    (Wire.Everparse.plug_setters schema);
  List.iter
    (fun f ->
      let setter = f.Wire.Everparse.pf_setter in
      if not (List.mem_assoc setter (Wire.Everparse.plug_setters schema)) then
        Alcotest.failf "field %s references setter %s not in plug_setters"
          f.Wire.Everparse.pf_name setter)
    (Wire.Everparse.plug_fields schema)

(* Test fixtures. Each codec exercises a specific combination so invariants
   1-4 cover mixed/pure/byte/constrained/action scenarios. *)

let scalars_codec =
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint16be in
  let f_c = Field.v "c" uint32be in
  let open Codec in
  v "PureUint"
    (fun a b c -> (a, b, c))
    [
      (f_a $ fun (a, _, _) -> a);
      (f_b $ fun (_, b, _) -> b);
      (f_c $ fun (_, _, c) -> c);
    ]

let bitfields_codec =
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let open Codec in
  v "PureBitfield"
    (fun hi lo -> (hi, lo))
    [ (f_hi $ fun (h, _) -> h); (f_lo $ fun (_, l) -> l) ]

let mixed_codec =
  let f_version = Field.v "version" (bits ~width:4 U8) in
  let f_flags = Field.v "flags" (bits ~width:4 U8) in
  let f_length = Field.v "length" uint16be in
  let open Codec in
  v "MixedOutput"
    (fun v f l -> (v, f, l))
    [
      (f_version $ fun (v, _, _) -> v);
      (f_flags $ fun (_, f, _) -> f);
      (f_length $ fun (_, _, l) -> l);
    ]

let byte_array_codec =
  let f_tag = Field.v "tag" uint8 in
  let f_payload = Field.v "payload" (byte_array ~size:(int 4)) in
  let open Codec in
  v "ByteArrayOut"
    (fun tag payload -> (tag, payload))
    [ (f_tag $ fun (t, _) -> t); (f_payload $ fun (_, p) -> p) ]

let constrained_codec =
  let f_x_ref = Field.v "x" uint8 in
  let f_x =
    Field.v "x" ~constraint_:Expr.(Field.ref f_x_ref <= int 100) uint8
  in
  let open Codec in
  v "ConstrainedOut" (fun x -> x) [ (f_x $ fun x -> x) ]

let with_existing_action_codec =
  let out_len = Param.output "out_len" uint16be in
  let f_len = Field.v "len" uint16be in
  let f_data =
    Field.v "data" uint8
      ~action:(Action.on_success [ Action.assign out_len (Field.ref f_len) ])
  in
  let open Codec in
  v "WithAction"
    (fun len data -> (len, data))
    [ (f_len $ fun (l, _) -> l); (f_data $ fun (_, d) -> d) ]

let test_action_forms_scalars () = check_action_forms scalars_codec
let test_action_forms_bitfields () = check_action_forms bitfields_codec
let test_action_forms_mixed () = check_action_forms mixed_codec
let test_action_forms_bytes () = check_action_forms byte_array_codec
let test_action_forms_constrained () = check_action_forms constrained_codec

let test_action_forms_with_action () =
  check_action_forms with_existing_action_codec

let test_plug_completeness_scalars () = check_plug_completeness scalars_codec
let test_plug_completeness_mixed () = check_plug_completeness mixed_codec
let test_plug_completeness_bytes () = check_plug_completeness byte_array_codec

let test_setter_scoping () =
  check_setter_scoping scalars_codec mixed_codec;
  check_setter_scoping bitfields_codec byte_array_codec

let test_setter_declarations_scalars () =
  check_setter_declarations scalars_codec

let test_setter_declarations_mixed () = check_setter_declarations mixed_codec

let test_setter_declarations_bytes () =
  check_setter_declarations byte_array_codec

(* -- Full pipeline e2e: 3D -> EverParse -> WireSet* -> compile -> call -- *)

let test_e2e_output_parse () =
  if not has_3d then ()
  else begin
    let dir = Filename.temp_dir "wire_e2e_output" "" in
    (* 1. Build a codec *)
    let f_id = Wire.Field.v "Id" Wire.uint32be in
    let f_len = Wire.Field.v "Length" Wire.uint16be in
    let f_tag = Wire.Field.v "Tag" Wire.uint8 in
    let codec =
      let open Wire.Codec in
      v "TestParse"
        (fun id len tag -> (id, len, tag))
        [
          (f_id $ fun (id, _, _) -> id);
          (f_len $ fun (_, l, _) -> l);
          (f_tag $ fun (_, _, t) -> t);
        ]
    in
    let s = Wire.Everparse.struct_of_codec codec in
    let name = Wire.Everparse.Raw.struct_name s in
    (* 2. Generate 3D with output pattern and run EverParse *)
    let schema = Wire.Everparse.schema codec in
    Wire.Everparse.write_3d ~outdir:dir [ schema ];
    Wire_3d.run_everparse ~outdir:dir [ schema ];
    (* 3. Emit default ExternalTypedefs + _Fields plug (Fields plug is what
       wire.stubs now stack-allocates against in wire_ffi.c). *)
    Wire_3d.write_external_typedefs ~outdir:dir [ schema ];
    Wire_3d.write_fields ~outdir:dir [ schema ];
    ignore name;
    (* 4. Generate parse stubs *)
    Wire_stubs.of_structs ~schema_dir:dir ~outdir:dir [ s ];
    (* 5. Generate ML stubs *)
    let ml_stubs = Wire_stubs.to_ml_stubs [ s ] in
    write_file (Filename.concat dir "stubs.ml") ml_stubs;
    (* 6. Write test program *)
    write_file
      (Filename.concat dir "main.ml")
      {|let () =
  let buf = Bytes.create 7 in
  Bytes.set_int32_be buf 0 0x12345678l;
  Bytes.set_uint16_be buf 4 1000;
  Bytes.set_uint8 buf 6 42;
  let r = Stubs.testparse_parse buf 0 in
  assert (r.Stubs.id = 0x12345678);
  assert (r.Stubs.length = 1000);
  assert (r.Stubs.tag = 42);
  Printf.printf "OK: id=0x%x length=%d tag=%d\n" r.Stubs.id r.Stubs.length r.Stubs.tag
|};
    (* 7. Compile everything *)
    let cmd =
      Fmt.str
        "cd %s && ocamlfind ocamlopt -package wire,wire.stubs -linkpkg -ccopt \
         '-I %s' wire_ffi.c stubs.ml main.ml -o test_output 2>&1"
        dir dir
    in
    run cmd;
    (* 8. Run *)
    run (Filename.concat dir "test_output")
  end

(* -- Suite -- *)

let suite =
  ( "wire_stubs",
    [
      (* ML stubs -- no params *)
      Alcotest.test_case "no params" `Quick test_no_params;
      Alcotest.test_case "single field" `Quick test_single_field;
      Alcotest.test_case "bitfield struct" `Quick test_bitfield_struct;
      Alcotest.test_case "many fields" `Quick test_many_fields;
      (* ML stubs -- input params *)
      Alcotest.test_case "one input" `Quick test_one_input;
      Alcotest.test_case "two inputs" `Quick test_two_inputs;
      (* ML stubs -- output params *)
      Alcotest.test_case "one output" `Quick test_one_output;
      Alcotest.test_case "two outputs" `Quick test_two_outputs;
      (* ML stubs -- mixed *)
      Alcotest.test_case "mixed input+output" `Quick test_mixed;
      Alcotest.test_case "mixed many" `Quick test_mixed_many;
      (* ML stubs -- bytecode threshold *)
      Alcotest.test_case "many params (bytecode)" `Quick
        test_many_params_bytecode;
      Alcotest.test_case "exactly 5 args (no bytecode)" `Quick
        test_exactly_five_args;
      Alcotest.test_case "6 args" `Quick test_six_args;
      (* ML stubs -- other *)
      Alcotest.test_case "bitfield param" `Quick test_bitfield_param;
      Alcotest.test_case "multiple structs" `Quick test_multiple_structs;
      (* C stubs *)
      Alcotest.test_case "c stubs: no params" `Quick test_c_stubs_no_params;
      Alcotest.test_case "c stubs: with params" `Quick test_c_stubs_with_params;
      Alcotest.test_case "c stubs: many params" `Quick test_c_stubs_many_params;
      (* stub name *)
      Alcotest.test_case "name: camelCase" `Quick test_name_camel;
      Alcotest.test_case "name: ALLCAPS" `Quick test_name_allcaps;
      Alcotest.test_case "name: single word" `Quick test_name_single;
      (* end-to-end: generate -> EverParse -> compile -> call *)
      Alcotest.test_case "e2e: no params" `Slow test_e2e_no_params;
      Alcotest.test_case "e2e: constraint" `Slow test_e2e_with_constraint;
      Alcotest.test_case "e2e: bitfields" `Slow test_e2e_bitfields;
      Alcotest.test_case "e2e: output parse" `Slow test_e2e_output_parse;
      (* output pattern invariants *)
      Alcotest.test_case "action forms: scalars" `Quick
        test_action_forms_scalars;
      Alcotest.test_case "action forms: bitfields" `Quick
        test_action_forms_bitfields;
      Alcotest.test_case "action forms: mixed" `Quick test_action_forms_mixed;
      Alcotest.test_case "action forms: bytes" `Quick test_action_forms_bytes;
      Alcotest.test_case "action forms: constrained" `Quick
        test_action_forms_constrained;
      Alcotest.test_case "action forms: with user action" `Quick
        test_action_forms_with_action;
      Alcotest.test_case "plug completeness: scalars" `Quick
        test_plug_completeness_scalars;
      Alcotest.test_case "plug completeness: mixed" `Quick
        test_plug_completeness_mixed;
      Alcotest.test_case "plug completeness: bytes" `Quick
        test_plug_completeness_bytes;
      Alcotest.test_case "setter scoping: disjoint across schemas" `Quick
        test_setter_scoping;
      Alcotest.test_case "setter declarations: scalars" `Quick
        test_setter_declarations_scalars;
      Alcotest.test_case "setter declarations: mixed" `Quick
        test_setter_declarations_mixed;
      Alcotest.test_case "setter declarations: bytes" `Quick
        test_setter_declarations_bytes;
    ] )
