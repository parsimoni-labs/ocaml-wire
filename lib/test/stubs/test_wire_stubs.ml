(** Tests for Wire_stubs — OCaml FFI stub generation.

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

(* ── No params ── *)

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

(* ── Input params only ── *)

let test_one_input () =
  check_struct
    (param_struct "OneInput" [ param "limit" uint8 ] [ field "x" uint8 ])

let test_two_inputs () =
  check_struct
    (param_struct "TwoInputs"
       [ param "lo" uint8; param "hi" uint16be ]
       [ field "x" uint8 ])

(* ── Output params only ── *)

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

(* ── Mixed input + output ── *)

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

(* ── >5 args (bytecode path) ── *)

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
  (* params are ignored in parse stubs — always bytes -> 'a *)
  let s =
    param_struct "FiveArgs"
      [ param "a" uint8; param "b" uint8; param "c" uint8; param "d" uint8 ]
      [ field "x" uint8 ]
  in
  check_struct s

let test_six_args () =
  (* params are ignored in parse stubs — always bytes -> 'a *)
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

(* ── Bitfield params ── *)

let test_bitfield_param () =
  check_struct
    (param_struct "BfParam"
       [ param "mask" (bits ~width:4 U8) ]
       [ field "v" uint8 ])

(* ── Multiple structs in one to_ml_stubs call ── *)

let test_multiple_structs () =
  let s1 = struct_ "Foo" [ field "x" uint8 ] in
  let s2 = struct_ "Bar" [ field "y" uint16be ] in
  let s3 = param_struct "Baz" [ param "limit" uint8 ] [ field "z" uint32be ] in
  let ml = Wire_stubs.to_ml_stubs [ s1; s2; s3 ] in
  compile_ml_stub ml;
  Alcotest.(check bool) "has foo" true (contains ~sub:"foo_parse" ml);
  Alcotest.(check bool) "has bar" true (contains ~sub:"bar_parse" ml);
  Alcotest.(check bool) "has baz" true (contains ~sub:"baz_parse" ml)

(* ── C stubs (string checks, cannot compile without EverParse headers) ── *)

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

(* ── to_ml_stub_name ── *)

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

(* ── End-to-end: generate → EverParse → compile C+ML → call ── *)

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

(** End-to-end: .3d → EverParse → C stubs → ML stubs → compile → call. [test_ml]
    is the OCaml source that calls the generated stubs and exits 0 on success,
    non-zero on failure. *)
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
  (* x=200 > 100: fails — parse raises *)
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

(* ── Output types ── *)

(* Helper: build a codec, generate 3D, check that the generated string
   uses extern typedef + Extern_call (WireSet) pattern, and optionally run
   EverParse to validate. Returns the 3D string. *)
let check_output_3d ?(run_everparse = true) codec =
  let schema = Wire.Everparse.schema codec in
  let s3d = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "has extern typedef" true
    (contains ~sub:"extern typedef" s3d);
  Alcotest.(check bool) "has WireSet" true (contains ~sub:"WireSet" s3d);
  if run_everparse && has_3d then begin
    let dir = Filename.temp_dir "wire_output_test" "" in
    Wire.Everparse.write_3d ~outdir:dir [ schema ];
    Wire_3d.run_everparse ~outdir:dir [ schema ];
    ignore (Sys.command (Fmt.str "rm -rf %s" dir))
  end;
  s3d

let test_output_pure_uint () =
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint16be in
  let f_c = Field.v "c" uint32be in
  let codec =
    let open Codec in
    v "PureUint"
      (fun a b c -> (a, b, c))
      [
        (f_a $ fun (a, _, _) -> a);
        (f_b $ fun (_, b, _) -> b);
        (f_c $ fun (_, _, c) -> c);
      ]
  in
  let s3d = check_output_3d codec in
  Alcotest.(check bool) "has :on-success" true (contains ~sub:":on-success" s3d)

let test_output_pure_bitfield () =
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let codec =
    let open Codec in
    v "PureBitfield"
      (fun hi lo -> (hi, lo))
      [ (f_hi $ fun (h, _) -> h); (f_lo $ fun (_, l) -> l) ]
  in
  let s3d = check_output_3d ~run_everparse:false codec in
  Alcotest.(check bool) "has :act" true (contains ~sub:":act" s3d)

let test_output_mixed () =
  let f_version = Field.v "version" (bits ~width:4 U8) in
  let f_flags = Field.v "flags" (bits ~width:4 U8) in
  let f_length = Field.v "length" uint16be in
  let codec =
    let open Codec in
    v "MixedOutput"
      (fun v f l -> (v, f, l))
      [
        (f_version $ fun (v, _, _) -> v);
        (f_flags $ fun (_, f, _) -> f);
        (f_length $ fun (_, _, l) -> l);
      ]
  in
  let s3d = check_output_3d ~run_everparse:false codec in
  Alcotest.(check bool) "has :act" true (contains ~sub:":act" s3d);
  Alcotest.(check bool) "has :on-success" true (contains ~sub:":on-success" s3d)

let test_output_bool_bitfield () =
  let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
  let f_data = Field.v "data" uint8 in
  let codec =
    let open Codec in
    v "BoolBf"
      (fun flag data -> (flag, data))
      [ (f_flag $ fun (f, _) -> f); (f_data $ fun (_, d) -> d) ]
  in
  (* bool(bits ~width:1 U8) is Map { inner = Bits _; ... } — should be :act *)
  let s3d = check_output_3d ~run_everparse:false codec in
  Alcotest.(check bool) "has :act" true (contains ~sub:":act" s3d);
  Alcotest.(check bool) "has :on-success" true (contains ~sub:":on-success" s3d)

let test_output_constrained () =
  let f_x_ref = Field.v "x" uint8 in
  let f_x =
    Field.v "x" ~constraint_:Expr.(Field.ref f_x_ref <= int 100) uint8
  in
  let codec =
    let open Codec in
    v "ConstrainedOut" (fun x -> x) [ (f_x $ fun x -> x) ]
  in
  let s3d = check_output_3d codec in
  Alcotest.(check bool) "has :on-success" true (contains ~sub:":on-success" s3d)

let test_output_byte_array () =
  let f_tag = Field.v "tag" uint8 in
  let f_payload = Field.v "payload" (byte_array ~size:(int 4)) in
  let codec =
    let open Codec in
    v "ByteArrayOut"
      (fun tag payload -> (tag, payload))
      [ (f_tag $ fun (t, _) -> t); (f_payload $ fun (_, p) -> p) ]
  in
  let s3d = check_output_3d ~run_everparse:false codec in
  Alcotest.(check bool)
    "has WireSetBytes" true
    (contains ~sub:"WireSetBytes" s3d);
  Alcotest.(check bool) "has field_pos" true (contains ~sub:"field_pos" s3d);
  Alcotest.(check bool) "has :on-success" true (contains ~sub:":on-success" s3d)

let test_output_only_bitfields () =
  let f_a = Field.v "a" (bits ~width:3 U8) in
  let f_b = Field.v "b" (bits ~width:5 U8) in
  let f_c = Field.v "c" (bits ~width:8 U16be) in
  let f_d = Field.v "d" (bits ~width:8 U16be) in
  let codec =
    let open Codec in
    v "OnlyBitfields"
      (fun a b c d -> (a, b, c, d))
      [
        (f_a $ fun (a, _, _, _) -> a);
        (f_b $ fun (_, b, _, _) -> b);
        (f_c $ fun (_, _, c, _) -> c);
        (f_d $ fun (_, _, _, d) -> d);
      ]
  in
  let s3d = check_output_3d ~run_everparse:false codec in
  (* All fields are bitfields, so only :act, no :on-success *)
  Alcotest.(check bool)
    "only bitfields: has :act" true (contains ~sub:":act" s3d);
  Alcotest.(check bool)
    "only bitfields: no :on-success" false
    (contains ~sub:":on-success" s3d)

let test_output_only_nonbitfields () =
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint16be in
  let f_c = Field.v "c" uint32be in
  let codec =
    let open Codec in
    v "OnlyNonBf"
      (fun a b c -> (a, b, c))
      [
        (f_a $ fun (a, _, _) -> a);
        (f_b $ fun (_, b, _) -> b);
        (f_c $ fun (_, _, c) -> c);
      ]
  in
  let s3d = check_output_3d codec in
  (* All fields are non-bitfields, so only :on-success, no :act *)
  Alcotest.(check bool)
    "only non-bf: has :on-success" true
    (contains ~sub:":on-success" s3d);
  Alcotest.(check bool) "only non-bf: no :act" false (contains ~sub:":act" s3d)

let test_output_with_existing_action () =
  let out_len = Param.output "out_len" uint16be in
  let f_len = Field.v "len" uint16be in
  let f_data =
    Field.v "data" uint8
      ~action:(Action.on_success [ Action.assign out_len (Field.ref f_len) ])
  in
  let codec =
    let open Codec in
    v "WithAction"
      (fun len data -> (len, data))
      [ (f_len $ fun (l, _) -> l); (f_data $ fun (_, d) -> d) ]
  in
  let s3d = check_output_3d codec in
  (* data field had an existing :on-success action; output should merge *)
  Alcotest.(check bool)
    "existing action: has out_len assign" true
    (contains ~sub:"out_len" s3d);
  Alcotest.(check bool) "has :on-success" true (contains ~sub:":on-success" s3d)

(* ── Full pipeline e2e: 3D → EverParse → WireSet* → compile → call ── *)

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
    (* 3. Generate ExternalTypedefs.h *)
    let ext_h = Wire_stubs.to_external_typedefs name in
    write_file (Filename.concat dir (name ^ "_ExternalTypedefs.h")) ext_h;
    (* 4. Generate parse stubs — wire_setters linked from wire.stubs *)
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

(* ── Suite ── *)

let suite =
  ( "wire_stubs",
    [
      (* ML stubs — no params *)
      Alcotest.test_case "no params" `Quick test_no_params;
      Alcotest.test_case "single field" `Quick test_single_field;
      Alcotest.test_case "bitfield struct" `Quick test_bitfield_struct;
      Alcotest.test_case "many fields" `Quick test_many_fields;
      (* ML stubs — input params *)
      Alcotest.test_case "one input" `Quick test_one_input;
      Alcotest.test_case "two inputs" `Quick test_two_inputs;
      (* ML stubs — output params *)
      Alcotest.test_case "one output" `Quick test_one_output;
      Alcotest.test_case "two outputs" `Quick test_two_outputs;
      (* ML stubs — mixed *)
      Alcotest.test_case "mixed input+output" `Quick test_mixed;
      Alcotest.test_case "mixed many" `Quick test_mixed_many;
      (* ML stubs — bytecode threshold *)
      Alcotest.test_case "many params (bytecode)" `Quick
        test_many_params_bytecode;
      Alcotest.test_case "exactly 5 args (no bytecode)" `Quick
        test_exactly_five_args;
      Alcotest.test_case "6 args" `Quick test_six_args;
      (* ML stubs — other *)
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
      (* end-to-end: generate → EverParse → compile → call *)
      Alcotest.test_case "e2e: no params" `Slow test_e2e_no_params;
      Alcotest.test_case "e2e: constraint" `Slow test_e2e_with_constraint;
      Alcotest.test_case "e2e: bitfields" `Slow test_e2e_bitfields;
      Alcotest.test_case "e2e: output parse" `Slow test_e2e_output_parse;
      (* output types *)
      Alcotest.test_case "output: pure uint" `Slow test_output_pure_uint;
      Alcotest.test_case "output: pure bitfield" `Slow test_output_pure_bitfield;
      Alcotest.test_case "output: mixed bf + non-bf" `Slow test_output_mixed;
      Alcotest.test_case "output: bool mapped bitfield" `Slow
        test_output_bool_bitfield;
      Alcotest.test_case "output: constrained field" `Slow
        test_output_constrained;
      Alcotest.test_case "output: byte_array field" `Slow test_output_byte_array;
      Alcotest.test_case "output: only bitfields" `Slow
        test_output_only_bitfields;
      Alcotest.test_case "output: only non-bitfields" `Slow
        test_output_only_nonbitfields;
      Alcotest.test_case "output: existing action" `Slow
        test_output_with_existing_action;
    ] )
