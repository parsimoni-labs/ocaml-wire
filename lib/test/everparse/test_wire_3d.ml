(** Tests for the EverParse pipeline -- schema generation and 3D output. *)

open Wire
open Wire.Everparse.Raw

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let simple_module = module_ [ typedef ~entrypoint:true simple_struct ]

let test_everparse_name () =
  (* All-caps names get lowercased with first letter capitalized *)
  Alcotest.(check string) "CLCW -> Clcw" "Clcw" (Wire_3d.everparse_name "CLCW");
  Alcotest.(check string)
    "TMFrame -> Tmframe" "Tmframe"
    (Wire_3d.everparse_name "TMFrame");
  (* Standard camelCase is preserved *)
  Alcotest.(check string) "Foo -> Foo" "Foo" (Wire_3d.everparse_name "Foo");
  Alcotest.(check string)
    "myField -> myField" "myField"
    (Wire_3d.everparse_name "myField");
  (* Single uppercase letter at start is not >=2 consecutive uppercase *)
  Alcotest.(check string)
    "Hello -> Hello" "Hello"
    (Wire_3d.everparse_name "Hello");
  (* Underscore-separated: each segment normalized, underscores dropped *)
  Alcotest.(check string)
    "EP_Header -> EpHeader" "EpHeader"
    (Wire_3d.everparse_name "EP_Header");
  Alcotest.(check string)
    "MC_Status_Reply -> McStatusReply" "McStatusReply"
    (Wire_3d.everparse_name "MC_Status_Reply");
  Alcotest.(check string)
    "Foo_Bar -> FooBar" "FooBar"
    (Wire_3d.everparse_name "Foo_Bar");
  (* Empty string *)
  Alcotest.(check string) "empty" "" (Wire_3d.everparse_name "")

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "wire_3d_test" "" in
  let s =
    Wire.Everparse.Raw.of_module ~name:"TestSimple" ~module_:simple_module
      ~wire_size:3
  in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_schema_of_struct () =
  let s = Wire.Everparse.schema_of_struct simple_struct in
  (* generate_3d uses the schema -- check we can produce a .3d file *)
  let tmpdir = Filename.temp_dir "wire_3d_test2" "" in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool)
    "3d file from schema_of_struct" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_ensure_dir () =
  let tmpdir = Filename.temp_dir "wire_3d_ensure" "" in
  Unix.rmdir tmpdir;
  Wire_3d.ensure_dir tmpdir;
  Alcotest.(check bool) "dir created" true (Sys.file_exists tmpdir);
  Unix.rmdir tmpdir

let test_generate_c () =
  (* generate_c requires 3d.exe; skip when not available *)
  if Wire_3d.has_3d_exe () then begin
    let tmpdir = Filename.temp_dir "wire_3d_gen_c" "" in
    let s = Wire.Everparse.schema_of_struct simple_struct in
    Wire_3d.generate_3d ~outdir:tmpdir [ s ];
    Wire_3d.generate_c ~outdir:tmpdir [ s ];
    let c_path = Filename.concat tmpdir "TestSimple.c" in
    Alcotest.(check bool) "C file generated" true (Sys.file_exists c_path);
    let ext_path = Filename.concat tmpdir "TestSimple_ExternalTypedefs.h" in
    Alcotest.(check bool)
      "ExternalTypedefs.h generated" true (Sys.file_exists ext_path)
  end

(* End-to-end compile+run. Generates C for a schema, invokes the same
   cc command [generate_dune] emits, runs the resulting binary. This is
   the one test that catches every kind of name mismatch between what
   wire.3d emits and what EverParse actually produces: filenames in
   [dune.inc], validator function names in [test.c], setter names in
   [_Fields.c], struct types, [#include]s. If any one of them is off,
   cc/ld fails and the test fails with the actual compiler error.
   Parameterised over the schema so tricky names (underscores, all-
   caps prefixes, mixed case) each exercise the pipeline end-to-end. *)
let compile_and_run ~name codec =
  if not (Wire_3d.has_3d_exe ()) then ()
  else begin
    let tmpdir = Filename.temp_dir ("wire_3d_e2e_" ^ name) "" in
    let schema = Everparse.schema codec in
    Wire_3d.generate_3d ~outdir:tmpdir [ schema ];
    Wire_3d.generate_c ~outdir:tmpdir [ schema ];
    let base = String.capitalize_ascii schema.Everparse.name in
    let cmd =
      Fmt.str
        "cd %s && cc -std=c99 -Wall -Wextra -Werror -Wpedantic \
         -Wstrict-prototypes -Wmissing-prototypes -Wshadow -Wcast-qual -o \
         test_bin test.c %s.c %s_Fields.c 2>&1 && ./test_bin"
        tmpdir base base
    in
    let ic = Unix.open_process_in cmd in
    let output = In_channel.input_all ic in
    let status = Unix.close_process_in ic in
    ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
    match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
        Alcotest.failf "%s: compile/run failed with exit %d:\n%s" name n output
    | _ ->
        Alcotest.failf "%s: compile/run terminated abnormally:\n%s" name output
  end

(* Schemas exercising every name-normalization edge case we know about. *)
let e2e_simple_codec =
  let open Wire in
  let f = Field.v "x" uint8 in
  Codec.v "Demo" (fun x -> x) [ Codec.( $ ) f (fun x -> x) ]

let e2e_allcaps_codec =
  let open Wire in
  let f = Field.v "x" uint8 in
  Codec.v "CLCW" (fun x -> x) [ Codec.( $ ) f (fun x -> x) ]

let e2e_tm_codec =
  let open Wire in
  let f = Field.v "version" (bits ~width:4 U8) in
  Codec.v "TMFrame" (fun v -> v) [ Codec.( $ ) f (fun v -> v) ]

let e2e_snake_codec =
  let open Wire in
  let fv = Field.v "v" uint8 in
  let fl = Field.v "len" uint16be in
  Codec.v "rpmsg_endpoint_info"
    (fun v l -> (v, l))
    [ Codec.( $ ) fv fst; Codec.( $ ) fl snd ]

let e2e_mixed_codec =
  let open Wire in
  let fh = Field.v "h" uint8 in
  Codec.v "EP_Header" (fun h -> h) [ Codec.( $ ) fh (fun h -> h) ]

(* Proximity1-style: variable-length data whose size is expressed as
   [length - header]. EverParse's SMT needs a constraint on [length]
   to prove the subtraction doesn't underflow, or verification of the
   generated .fst fails. [?self_constraint] on the length field emits
   that constraint inline. *)
let e2e_underflow_codec =
  let open Wire in
  let header_size = 7 in
  let f_len =
    Field.v "Length" uint16be ~self_constraint:(fun self ->
        Expr.(self >= int header_size))
  in
  let f_data =
    Field.v "Data" (byte_array ~size:Expr.(Field.ref f_len - int header_size))
  in
  let open Codec in
  v "UnderflowCheck" (fun len data -> (len, data)) [ f_len $ fst; f_data $ snd ]

let test_e2e_compile_run () =
  compile_and_run ~name:"Demo" e2e_simple_codec;
  compile_and_run ~name:"CLCW" e2e_allcaps_codec;
  compile_and_run ~name:"TMFrame" e2e_tm_codec;
  compile_and_run ~name:"rpmsg_endpoint_info" e2e_snake_codec;
  compile_and_run ~name:"EP_Header" e2e_mixed_codec;
  compile_and_run ~name:"UnderflowCheck" e2e_underflow_codec

let test_uses_wire_ctx () =
  let s = Wire.Everparse.schema_of_struct simple_struct in
  Alcotest.(check bool)
    "schema_of_struct uses WireCtx" true
    (Wire.Everparse.uses_wire_ctx s);
  let raw =
    Wire.Everparse.Raw.of_module ~name:"TestSimple" ~module_:simple_module
      ~wire_size:3
  in
  Alcotest.(check bool)
    "raw module without extern WireCtx" false
    (Wire.Everparse.uses_wire_ctx raw)

let test_has_3d_exe () =
  (* Just check the function is callable and returns a bool *)
  let result = Wire_3d.has_3d_exe () in
  Alcotest.(check bool) "has_3d_exe returns bool" result result

let test_main_exists () =
  (* main dispatches on Sys.argv; calling it in tests would exit, so we only
     verify that the value exists and has the right type. *)
  let _ = (Wire_3d.main : package:string -> Wire.Everparse.t list -> unit) in
  ()

(* Adversarial: [schema.wire_size] must agree with [Codec.wire_size] AND with
   the byte count we expect by hand for every bitfield layout. Two layers:
   if the projection silently drifts from [Codec.wire_size] we validate the
   wrong amount of data; if the expected value is itself wrong, hand-computed
   constants catch that too. *)
let check_size ~name ~expected codec =
  let schema = Everparse.schema codec in
  let codec_size = Codec.wire_size codec in
  Alcotest.(check int)
    (Fmt.str "%s: Codec.wire_size = %d" name expected)
    expected codec_size;
  Alcotest.(check (option int))
    (Fmt.str "%s: schema.wire_size = %d" name expected)
    (Some expected) schema.Everparse.wire_size

(* SSID-like layout: mix of [bit (bits 1 U8)] and bare [bits N U8] summing to
   exactly one U8. Historical bug: the Map-wrapped fields fell out of the bit
   group and produced spurious anon padding. *)
let ssid_style_codec =
  let f1 = Field.v "a" (bit (bits ~width:1 U8)) in
  let f2 = Field.v "b" (bits ~width:2 U8) in
  let f3 = Field.v "c" (bits ~width:4 U8) in
  let f4 = Field.v "d" (bit (bits ~width:1 U8)) in
  let open Codec in
  v "SsidStyle"
    (fun a b c d -> (a, b, c, d))
    [
      (f1 $ fun (a, _, _, _) -> a);
      (f2 $ fun (_, b, _, _) -> b);
      (f3 $ fun (_, _, c, _) -> c);
      (f4 $ fun (_, _, _, d) -> d);
    ]

(* All [bit] (Map-wrapped), packed into one U8. *)
let all_bool_codec =
  let f1 = Field.v "a" (bit (bits ~width:1 U8)) in
  let f2 = Field.v "b" (bit (bits ~width:1 U8)) in
  let f3 = Field.v "c" (bit (bits ~width:1 U8)) in
  let f4 = Field.v "d" (bit (bits ~width:1 U8)) in
  let open Codec in
  v "AllBool"
    (fun a b c d -> (a, b, c, d))
    [
      (f1 $ fun (a, _, _, _) -> a);
      (f2 $ fun (_, b, _, _) -> b);
      (f3 $ fun (_, _, c, _) -> c);
      (f4 $ fun (_, _, _, d) -> d);
    ]

(* Spilling across a base-word boundary: 5+5 bits on U8 must roll into two U8s. *)
let spill_u8_codec =
  let f1 = Field.v "a" (bits ~width:5 U8) in
  let f2 = Field.v "b" (bits ~width:5 U8) in
  let open Codec in
  v "SpillU8" (fun a b -> (a, b)) [ f1 $ fst; f2 $ snd ]

(* Bit group broken by a non-bitfield field. Two separate U8 groups plus
   the intervening scalar. *)
let broken_by_scalar_codec =
  let f1 = Field.v "a" (bits ~width:4 U8) in
  let f2 = Field.v "b" (bits ~width:4 U8) in
  let f3 = Field.v "sep" uint8 in
  let f4 = Field.v "c" (bits ~width:4 U8) in
  let f5 = Field.v "d" (bits ~width:4 U8) in
  let open Codec in
  v "BrokenByScalar"
    (fun a b sep c d -> (a, b, sep, c, d))
    [
      (f1 $ fun (a, _, _, _, _) -> a);
      (f2 $ fun (_, b, _, _, _) -> b);
      (f3 $ fun (_, _, s, _, _) -> s);
      (f4 $ fun (_, _, _, c, _) -> c);
      (f5 $ fun (_, _, _, _, d) -> d);
    ]

(* U32BE bitfields: total 32 bits -> one U32. *)
let u32_pack_codec =
  let f1 = Field.v "a" (bits ~width:8 U32be) in
  let f2 = Field.v "b" (bits ~width:16 U32be) in
  let f3 = Field.v "c" (bits ~width:8 U32be) in
  let open Codec in
  v "U32Pack"
    (fun a b c -> (a, b, c))
    [
      (f1 $ fun (a, _, _) -> a);
      (f2 $ fun (_, b, _) -> b);
      (f3 $ fun (_, _, c) -> c);
    ]

(* Bitfield wrapped in Where (constraint). Must still coalesce. *)
let where_wrapped_codec =
  let f1_ref = Field.v "a" (bits ~width:4 U8) in
  let f1 =
    Field.v "a" (bits ~width:4 U8) ~constraint_:Expr.(Field.ref f1_ref <= int 5)
  in
  let f2 = Field.v "b" (bits ~width:4 U8) in
  let open Codec in
  v "WhereWrapped" (fun a b -> (a, b)) [ f1 $ fst; f2 $ snd ]

let test_projection_sizes () =
  (* SSID: 1+2+4+1 = 8 bits, one U8. *)
  check_size ~name:"SsidStyle" ~expected:1 ssid_style_codec;
  (* 4 bool bits, one U8. *)
  check_size ~name:"AllBool" ~expected:1 all_bool_codec;
  (* 5+5 = 10 bits, must spill to two U8s. *)
  check_size ~name:"SpillU8" ~expected:2 spill_u8_codec;
  (* Two bit groups of one U8 each + an intervening uint8 = 3 bytes. *)
  check_size ~name:"BrokenByScalar" ~expected:3 broken_by_scalar_codec;
  (* 8+16+8 = 32 bits, one U32be. *)
  check_size ~name:"U32Pack" ~expected:4 u32_pack_codec;
  (* 4+4 = 8 bits, one U8, Where wrapping the first field. *)
  check_size ~name:"WhereWrapped" ~expected:1 where_wrapped_codec

(* Filenames must match what EverParse actually produces from the [.3d]
   (which wire writes as [String.capitalize_ascii name ^ ".3d"]). The
   C identifier inside is [everparse_name name]: strip underscores,
   CamelCase segments. Verify both together for a lowercase-underscore
   schema name like [rpmsg_endpoint_info] that exercises the split. *)
let test_projection_filenames () =
  let s =
    Everparse.Raw.struct_ "rpmsg_endpoint_info"
      [ Everparse.Raw.field "v" uint8; Everparse.Raw.field "id" uint16be ]
  in
  let schema = Everparse.schema_of_struct s in
  let tmpdir = Filename.temp_dir "wire_3d_filenames" "" in
  Wire_3d.generate_dune ~outdir:tmpdir ~package:"pkg" [ schema ];
  let dune_inc =
    let ic = open_in (Filename.concat tmpdir "dune.inc") in
    let n = in_channel_length ic in
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    Bytes.unsafe_to_string buf
  in
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
  let contains_exact sub = Re.execp (Re.compile (Re.str sub)) dune_inc in
  Alcotest.(check bool)
    ".3d uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_info.3d");
  Alcotest.(check bool)
    ".h uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_info.h");
  Alcotest.(check bool)
    "Wrapper.c uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_infoWrapper.c");
  Alcotest.(check bool)
    "_Fields.c uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_info_Fields.c");
  Alcotest.(check bool)
    "raw schema name not used as filename" false
    (contains_exact "rpmsg_endpoint_info.h")

let suite =
  ( "wire_3d",
    [
      Alcotest.test_case "everparse_name" `Quick test_everparse_name;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "schema_of_struct" `Quick test_schema_of_struct;
      Alcotest.test_case "ensure_dir" `Quick test_ensure_dir;
      Alcotest.test_case "generate_c (needs 3d.exe)" `Quick test_generate_c;
      Alcotest.test_case "uses_wire_ctx" `Quick test_uses_wire_ctx;
      Alcotest.test_case "has_3d_exe" `Quick test_has_3d_exe;
      Alcotest.test_case "main exists" `Quick test_main_exists;
      Alcotest.test_case "projection: wire_size matches Codec" `Quick
        test_projection_sizes;
      Alcotest.test_case "projection: filenames match EverParse output" `Quick
        test_projection_filenames;
      Alcotest.test_case "e2e: compile + run across naming conventions" `Slow
        test_e2e_compile_run;
    ] )
