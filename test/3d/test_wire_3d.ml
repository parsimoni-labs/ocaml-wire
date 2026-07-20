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
  (* A consecutive-capital run is collapsed wherever it occurs, not only at the
     start: the [OSF] run in the middle lowercases to [osf]. *)
  Alcotest.(check string)
    "SpaceOSFrame -> SpaceOsframe" "SpaceOsframe"
    (Wire_3d.everparse_name "SpaceOSFrame");
  (* Each segment is capitalized: wire writes the .3d type with a capitalized
     name, so [myField] reaches EverParse as [MyField]. *)
  Alcotest.(check string) "Foo -> Foo" "Foo" (Wire_3d.everparse_name "Foo");
  Alcotest.(check string)
    "myField -> MyField" "MyField"
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
  (* A lowercase segment after an underscore is capitalized, so the identifier
     matches the [GrpcMessage] EverParse generates for a [Grpc_message] type. *)
  Alcotest.(check string)
    "Grpc_message -> GrpcMessage" "GrpcMessage"
    (Wire_3d.everparse_name "Grpc_message");
  Alcotest.(check string)
    "rpmsg_endpoint_info -> RpmsgEndpointInfo" "RpmsgEndpointInfo"
    (Wire_3d.everparse_name "rpmsg_endpoint_info");
  (* Empty string *)
  Alcotest.(check string) "empty" "" (Wire_3d.everparse_name "")

(* [pascal_case] mirrors EverParse's wrapper-symbol mangling. EverParse builds a
   wrapper as [pascal_case (module ^ "_check_" ^ codec)]; these expectations were
   read straight off EverParse's generated [<Module>Wrapper.h]. The digit cases
   are the ones [everparse_name] gets wrong (it keeps the post-digit capital). *)
let test_pascal_case () =
  let check input expected =
    Alcotest.(check string)
      (input ^ " -> " ^ expected)
      expected
      (Wire_3d.pascal_case input)
  in
  check "tpm_check_TPM2B" "TpmCheckTpm2b";
  check "virtio_check_Virtq_desc" "VirtioCheckVirtqDesc";
  check "virtio_check_Virtq_used_elem" "VirtioCheckVirtqUsedElem";
  check "probe_check_Foo2Bar" "ProbeCheckFoo2bar";
  check "probe_check_AB2CD" "ProbeCheckAb2cd";
  check "probe_check_SpaceOSFrame" "ProbeCheckSpaceOsframe";
  (* No underscore: only the first letter is capitalized. *)
  check "Foo" "Foo"

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
  let s = Wire.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
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
    let s = Wire.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
    Wire_3d.generate_3d ~outdir:tmpdir [ s ];
    Wire_3d.generate_c ~outdir:tmpdir [ s ];
    let c_path = Filename.concat tmpdir "TestSimple.c" in
    Alcotest.(check bool) "C file generated" true (Sys.file_exists c_path);
    let ext_path = Filename.concat tmpdir "TestSimple_ExternalTypedefs.h" in
    Alcotest.(check bool)
      "ExternalTypedefs.h generated" true (Sys.file_exists ext_path)
  end

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

let doc_codec name =
  Codec.v name (fun v -> v) Codec.[ (Field.v "v" uint8 $ fun v -> v) ]

let test_generate_dune_standalone () =
  (* The doc pipeline emits a single-file [dune.inc]: the package name becomes a
     CamelCase module name and there is no FFI plug or test harness. *)
  let tmpdir = Filename.temp_dir "wire_3d_dune_doc" "" in
  Wire_3d.generate_dune_standalone ~outdir:tmpdir ~package:"my-pkg"
    [ Wire_3d.pack (doc_codec "Pkt") ];
  let dune_inc = read_file (Filename.concat tmpdir "dune.inc") in
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
  let has sub = Re.execp (Re.compile (Re.str sub)) dune_inc in
  Alcotest.(check bool) "single .3d target" true (has "MyPkg.3d");
  Alcotest.(check bool) "single .c target" true (has "MyPkg.c");
  Alcotest.(check bool) "wrapper target" true (has "MyPkgWrapper.c");
  Alcotest.(check bool)
    "installs under the package" true (has "(package my-pkg)");
  Alcotest.(check bool) "builds a validator archive" true (has "libmypkg.a");
  Alcotest.(check bool)
    "runtest runs the differential check" true
    (has "%{dep:agree} corpus");
  Alcotest.(check bool) "corpus oracle" true (has "%{exe:gen.exe} corpus");
  Alcotest.(check bool)
    "generator invoked via exe macro, not ./gen.exe" false (has "./gen.exe");
  Alcotest.(check bool) "no shell action in the runtest" false (has "(system");
  Alcotest.(check bool)
    "C rule gated behind BUILD_EVERPARSE, not fallback" true
    (has "%{env:BUILD_EVERPARSE=}");
  Alcotest.(check bool)
    "C rule never auto-regenerates" false (has "mode fallback");
  Alcotest.(check bool) "no FFI plug" false (has "_Fields");
  Alcotest.(check bool) "no FFI test harness" false (has "test.c")

let test_generate_dune_standalone_name_override () =
  (* [~name] sets the file base independently of the opam [~package]. *)
  let tmpdir = Filename.temp_dir "wire_3d_dune_name" "" in
  Wire_3d.generate_dune_standalone ~name:"proto-spec" ~outdir:tmpdir
    ~package:"my-pkg"
    [ Wire_3d.pack (doc_codec "Pkt") ];
  let dune_inc = read_file (Filename.concat tmpdir "dune.inc") in
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
  let has sub = Re.execp (Re.compile (Re.str sub)) dune_inc in
  Alcotest.(check bool) "file base from name" true (has "ProtoSpec.3d");
  Alcotest.(check bool) "file base from name (c)" true (has "ProtoSpec.c");
  Alcotest.(check bool) "package not used as file base" false (has "MyPkg.3d");
  Alcotest.(check bool)
    "install still uses the opam package" true (has "(package my-pkg)")

(* Split [s] into its top-level parenthesized stanzas. *)
let top_level_stanzas s =
  let stanzas = ref [] and buf = Buffer.create 256 in
  let depth = ref 0 and in_string = ref false in
  String.iter
    (fun c ->
      if !depth > 0 then Buffer.add_char buf c;
      if !in_string then (if c = '"' then in_string := false)
      else
        match c with
        | '"' -> in_string := true
        | '(' ->
            incr depth;
            if !depth = 1 then Buffer.add_char buf c
        | ')' ->
            decr depth;
            if !depth = 0 then begin
              stanzas := Buffer.contents buf :: !stanzas;
              Buffer.clear buf
            end
        | _ -> ())
    s;
  List.rev !stanzas

let test_generate_dune_standalone_context_policy () =
  (* The archive is a per-target artefact: a consumer links it into a program
     for the target it was built for. So its build and install run in every
     context through that context's own toolchain -- neither is gated on
     [%{context_name}], and there is no placeholder. Only the host-side steps
     are gated: regenerating the committed C from [3d.exe] and the [agree]
     differential test, which runs a built validator on the build machine. *)
  let tmpdir = Filename.temp_dir "wire_3d_dune_ctx" "" in
  Wire_3d.generate_dune_standalone ~outdir:tmpdir ~package:"my-pkg"
    [ Wire_3d.pack (doc_codec "Pkt") ];
  let dune_inc = read_file (Filename.concat tmpdir "dune.inc") in
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
  let has sub s = Re.execp (Re.compile (Re.str sub)) s in
  let stanzas = top_level_stanzas dune_inc in
  Alcotest.(check bool) "dune.inc has stanzas" true (stanzas <> []);
  (* The two archive build rules -- one per [%{ocaml-config:system}] branch --
     run in all contexts, so a cross build produces a target archive. *)
  let archive_rules = List.filter (has "%{ocaml-config:system}") stanzas in
  Alcotest.(check int) "two archive build rules" 2 (List.length archive_rules);
  List.iter
    (fun rule ->
      Alcotest.(check bool)
        "archive build is not host-gated" false
        (has "%{context_name}" rule);
      Alcotest.(check bool)
        "archive build uses the context compiler" true
        (has "%{ocaml-config:c_compiler}" rule))
    archive_rules;
  (* One install stanza, ungated: the archive installs into whichever toolchain
     the build targeted. *)
  let installs = List.filter (has "(install") stanzas in
  Alcotest.(check int) "one install stanza" 1 (List.length installs);
  Alcotest.(check bool)
    "install runs in all contexts" false
    (has "%{context_name}" (List.hd installs));
  (* No empty placeholder: the real archive builds in every context now. *)
  Alcotest.(check bool)
    "no placeholder archive" false
    (has "(write-file libmypkg.a" dune_inc);
  (* The host-only steps -- C regeneration and the differential test -- stay
     gated on the host context. *)
  let host_gated = List.filter (has "(= %{context_name} default)") stanzas in
  Alcotest.(check bool)
    "host-side steps stay host-gated" true
    (List.length host_gated >= 1)

let test_generate_standalone () =
  (* generate_standalone needs 3d.exe; skip when not available. *)
  if Wire_3d.has_3d_exe () then begin
    let tmpdir = Filename.temp_dir "wire_3d_gen_doc" "" in
    Wire_3d.generate_standalone ~outdir:tmpdir ~package:"my-pkg"
      [ Wire_3d.pack (doc_codec "Pkt"); Wire_3d.pack (doc_codec "Pkt2") ];
    let exists f = Sys.file_exists (Filename.concat tmpdir f) in
    Alcotest.(check bool) "single merged .3d" true (exists "MyPkg.3d");
    Alcotest.(check bool) "single validator .c" true (exists "MyPkg.c");
    let c_src = read_file (Filename.concat tmpdir "MyPkg.c") in
    Alcotest.(check bool)
      "validator is FFI-free" false
      (Re.execp (Re.compile (Re.str "WireSet")) c_src);
    ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir)
  end

(* Differential self-check for the doc pipeline: the FFI-free validator must
   accept exactly the inputs the OCaml codec accepts. These codecs each carry a
   constraint so a fuzzed corpus straddles accept and reject -- an enum
   membership, a scalar range, and a bitfield range whose byte also exercises
   the Msb-first reordering [U8] needs. Skipped without 3d.exe and a C
   compiler. *)
let diff_enum_codec =
  let open Wire in
  let color = enum "Color" [ ("Red", 0); ("Green", 1); ("Blue", 2) ] uint8 in
  let f = Field.v "hue" color in
  Codec.v "HueMsg" (fun v -> v) [ Codec.( $ ) f (fun v -> v) ]

let diff_range_codec =
  let open Wire in
  let fn =
    Field.v "n" uint8 ~self_constraint:(fun self -> Expr.(self < int 100))
  in
  let fm = Field.v "m" uint8 in
  Codec.v "RangeMsg"
    (fun n m -> (n, m))
    [ Codec.( $ ) fn fst; Codec.( $ ) fm snd ]

let diff_bits_codec =
  let open Wire in
  let fa =
    Field.v "a" (bits ~width:3 U8) ~self_constraint:(fun self ->
        Expr.(self < int 5))
  in
  let fb = Field.v "b" (bits ~width:5 U8) in
  Codec.v "BitMsg"
    (fun a b -> (a, b))
    [ Codec.( $ ) fa fst; Codec.( $ ) fb snd ]

(* A parameterized codec: the harness must bind the input param in the OCaml
   oracle and pass the same value to the EverParse validator, or a length-bound
   frame can never be checked (the blocker for CCSDS TC/AOS/TM/USLP). *)
let diff_param_codec =
  let open Wire in
  let max_len = Param.input "max_len" uint8 in
  let f_length = Field.v "Length" uint8 in
  let f_data = Field.v "Data" (byte_array ~size:(Field.ref f_length)) in
  Codec.v "Bounded"
    ~where:Expr.(Field.ref f_length <= Param.expr max_len)
    (fun len data -> (len, data))
    [ Codec.( $ ) f_length fst; Codec.( $ ) f_data snd ]

let differential_ok ~name ~package ~corpus codecs =
  let tmpdir = Filename.temp_dir ("wire_3d_diff_" ^ package) "" in
  Wire_3d.generate_standalone ~outdir:tmpdir ~name ~package codecs;
  let oc = open_out (Filename.concat tmpdir "corpus") in
  (match corpus with
  | `Fuzz count ->
      let ppf = Format.formatter_of_out_channel oc in
      Wire_3d.generate_corpus ~count ppf codecs
  | `Lines lines -> List.iter (fun l -> output_string oc (l ^ "\n")) lines);
  close_out oc;
  let base = String.capitalize_ascii name in
  let cmd =
    Fmt.str
      "cd %s && cc %s %s -c %s.c %sWrapper.c && ar rcs lib.a %s.o %sWrapper.o \
       && cc %s %s agree.c lib.a -o agree && ./agree corpus 2>&1"
      tmpdir Wire_3d.strict_cc_flags Wire_3d.everparse_type_defines base base
      base base Wire_3d.strict_cc_flags Wire_3d.everparse_type_defines
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Alcotest.failf "differential %s failed with exit %d:\n%s" name n output
  | _ -> Alcotest.failf "differential %s terminated abnormally:\n%s" name output

let test_doc_differential () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else
    differential_ok ~name:"protospec" ~package:"demo-doc" ~corpus:(`Fuzz 400)
      [
        Wire_3d.pack diff_enum_codec;
        Wire_3d.pack diff_range_codec;
        Wire_3d.pack diff_bits_codec;
        Wire_3d.pack diff_param_codec;
      ]

let test_doc_differential_no_params () =
  (* Regression: an all-non-parameterized package must still compile. The agree
     harness must not emit an unused [parse_params] helper (a -Werror under the
     strict flags) when no codec takes parameters. *)
  if not (Wire_3d.has_3d_exe ()) then ()
  else
    differential_ok ~name:"plainspec" ~package:"plain-doc" ~corpus:(`Fuzz 100)
      [ Wire_3d.pack diff_enum_codec; Wire_3d.pack diff_range_codec ]

(* The installed standalone archive must export only the checked
   [<Base>Check<Codec>] wrappers, not the raw [<Base>Validate*] entry points
   whose EverParse-emitted preamble underflows on [StartPosition > InputLength].
   Build the archive exactly as the generated dune rule does for this platform
   (via {!Wire_3d.archive_link_steps}) and assert [nm] shows no global [Validate]
   symbol. This runs the platform's real link step on CI (macOS and Linux). Needs
   3d.exe for the C. *)
let test_archive_hides_raw_validators () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else begin
    let name = "symspec" and package = "sym-doc" in
    let codec =
      Codec.v "Sample"
        (fun v -> v)
        [ Codec.( $ ) (Field.v "X" uint32be) (fun v -> v) ]
    in
    let codecs = [ Wire_3d.pack codec ] in
    let tmpdir = Filename.temp_dir "wire_3d_sym_" "" in
    Wire_3d.generate_standalone ~outdir:tmpdir ~name ~package codecs;
    let base = String.capitalize_ascii name in
    let archive = "lib" ^ String.lowercase_ascii base ^ ".a" in
    let wrappers = Wire_3d.wrapper_symbols base codecs in
    let macos =
      let ic = Unix.open_process_in "uname -s" in
      let s = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);
      String.equal s "Darwin"
    in
    let compile =
      Fmt.str "cc %s %s -c %s.c %sWrapper.c" Wire_3d.strict_cc_flags
        Wire_3d.everparse_type_defines base base
    in
    let steps =
      compile
      :: Wire_3d.archive_link_steps ~macos ~pack_linker:"ld -r -o"
           ~objcopy:"objcopy" ~ar:"ar" ~archive ~base ~wrappers
    in
    let run cmd =
      let ic = Unix.open_process_in (Fmt.str "cd %s && %s 2>&1" tmpdir cmd) in
      let out = In_channel.input_all ic in
      (out, Unix.close_process_in ic)
    in
    let build_out, build_status = run (String.concat " && " steps) in
    let nm_out, _ = run (Fmt.str "nm %s" archive) in
    ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
    (match build_status with
    | Unix.WEXITED 0 -> ()
    | _ -> Alcotest.failf "localized archive build failed:\n%s" build_out);
    (* Global text symbols are the uppercase-[T] [nm] lines. *)
    let globals =
      String.split_on_char '\n' nm_out
      |> List.filter (fun l -> Re.execp (Re.compile (Re.str " T ")) l)
    in
    let any sub =
      List.exists (fun l -> Re.execp (Re.compile (Re.str sub)) l) globals
    in
    Alcotest.(check bool) "raw Validate not exported" false (any "Validate");
    Alcotest.(check bool) "Check wrapper exported" true (any "Check")
  end

(* A signed field with an ordering constraint: it projects to an unsigned UINT,
   so the refinement must be the two's-complement form or the C validator and the
   OCaml decoder disagree on bytes whose top bit is set (200 = signed -56). The
   corpus straddles the whole byte range. *)
let diff_signed_codec =
  let open Wire in
  let f =
    Field.v "x" int8 ~self_constraint:(fun self -> Expr.(self < int 100))
  in
  Codec.v "SignedMsg" (fun v -> v) [ Codec.( $ ) f (fun v -> v) ]

let test_doc_differential_signed () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else
    differential_ok ~name:"signedspec" ~package:"signed-doc" ~corpus:(`Fuzz 256)
      [ Wire_3d.pack diff_signed_codec ]

(* An 8192-byte payload makes each corpus line 16384 hex chars; the agree
   reader's hex buffer must hold the line, or it truncates and the verdict
   misparses into a false mismatch. *)
let diff_large_payload_codec =
  let open Wire in
  Codec.v "SharedMem"
    (fun d -> d)
    [ Codec.( $ ) (Field.v "data" (byte_array ~size:(int 8192))) (fun d -> d) ]

let test_doc_differential_large_payload () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else
    differential_ok ~name:"sharedspec" ~package:"shared-doc" ~corpus:(`Fuzz 40)
      [ Wire_3d.pack diff_large_payload_codec ]

(* Interior consecutive caps: EverParse normalizes the validator symbol to
   [SpacewireCheckSpaceOsframe], so agree.c must build the same name through
   [everparse_name] or its call links to an undeclared function. *)
let diff_caps_name_codec =
  let open Wire in
  Codec.v "SpaceOSFrame"
    (fun v -> v)
    [ Codec.( $ ) (Field.v "v" uint8) (fun v -> v) ]

let test_doc_differential_caps_name () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else
    differential_ok ~name:"spacewire" ~package:"space-wire" ~corpus:(`Fuzz 40)
      [ Wire_3d.pack diff_caps_name_codec ]

(* A valid record followed by trailing bytes must be rejected. EverParse's
   stock [Check] wrapper returns TRUE on any successful validation -- success
   is the consumed position, so a valid prefix passes -- and the pipeline
   hardens it to require full consumption. The handwritten corpus pins the
   exact boundary: the 2-byte RangeMsg accepts exactly-sized input and rejects
   both one byte over and one byte short. *)
let test_doc_differential_trailing_bytes () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else
    differential_ok ~name:"trailspec" ~package:"trail-doc"
      ~corpus:
        (`Lines
           [ "RangeMsg - 0102 1"; "RangeMsg - 010203 0"; "RangeMsg - 01 0" ])
      [ Wire_3d.pack diff_range_codec ]

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
    let schema = Everparse.project ~mode:`Ffi codec in
    Wire_3d.generate_3d ~outdir:tmpdir [ schema ];
    Wire_3d.generate_c ~outdir:tmpdir [ schema ];
    let base = String.capitalize_ascii schema.Everparse.name in
    let cmd =
      (* Compile with the exact strict-C11 flags the generated dune emits, so
         the e2e checks the validators under the same standard a downstream
         caller would. *)
      Fmt.str
        "cd %s && cc %s -o test_bin test.c %s.c %s_Fields.c 2>&1 && ./test_bin"
        tmpdir Wire_3d.strict_cc_flags base base
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

(* [Wire.casetype] in a codec field. The Everparse projection auto-emits
   a [casetype_decl] dispatch + wrapper typedef per unique int-tagged
   [Casetype], then references the wrapper as the field type. End-to-end
   check that 3d.exe accepts the synthesised module and the generated C
   compiles. *)
type ep_case_v = [ `U16 of int | `Default of int ]

let e2e_casetype_codec =
  let open Wire in
  let body : ep_case_v typ =
    casetype "CtBody" uint8
      [
        case ~index:1 uint16
          ~inject:(fun v -> `U16 v)
          ~project:(function `U16 v -> Some v | _ -> None);
        default uint8
          ~inject:(fun _tag v -> `Default v)
          ~project:(function `Default v -> Some (0xFF, v) | _ -> None);
      ]
  in
  let f = Field.v "msg" body in
  Codec.v "Ctt" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* String-tagged casetype: the 3D projection rewrites the casetype field
   into two adjacent byte spans (tag and body) so dispatch happens in
   caller code, mirroring OpenSSH's two-step pattern. No 3D-side extern,
   scratch slot or runtime helper -- just two SetBytes setters. *)
type ssh_auth = [ `Publickey of int | `Other of int ]

let e2e_ssh_casetype_codec =
  let open Wire in
  let body : ssh_auth typ =
    casetype "AuthMethod"
      (byte_array ~size:(int 9))
      [
        case ~index:"publickey" uint8
          ~inject:(fun v -> `Publickey v)
          ~project:(function `Publickey v -> Some v | _ -> None);
        default uint8
          ~inject:(fun _tag v -> `Other v)
          ~project:(function `Other v -> Some ("xxxxxxxxx", v) | _ -> None);
      ]
  in
  let f = Field.v "method" body in
  Codec.v "Sshauth" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* Casetype with a sub-codec body that itself contains a trailing
   [all_bytes]. The 3D projection emits the sub-codec as its own typedef
   before the casetype's dispatch decl, and [all_bytes] inside the
   sub-codec consumes through end-of-buffer -- correct because the
   casetype is constrained to be the last field of its parent. *)
type session = { recipient : int; rest : string }
type channel_confirm = Session of session | Tcpip of int

let session_codec =
  let open Wire in
  let f_recip = Field.v "recipient" uint32be in
  let f_rest = Field.v "rest" all_bytes in
  Codec.v "Session"
    (fun r b -> { recipient = Wire.Private.UInt32.to_int r; rest = b })
    [
      Codec.( $ ) f_recip (fun s -> Wire.Private.UInt32.of_int s.recipient);
      Codec.( $ ) f_rest (fun s -> s.rest);
    ]

let e2e_codec_in_casetype =
  let open Wire in
  let body : channel_confirm typ =
    casetype "Confirm" uint8
      [
        case ~index:1 (codec session_codec)
          ~inject:(fun s -> Session s)
          ~project:(function Session s -> Some s | _ -> None);
        case ~index:2 uint16be
          ~inject:(fun n -> Tcpip n)
          ~project:(function Tcpip n -> Some n | _ -> None);
      ]
  in
  let f = Field.v "msg" body in
  Codec.v "ChannelOpen" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* [Field.repeat] over a sub-codec whose own size is dynamic (here a
   length-prefixed name). Projects to a byte-budget repeat
   [Ext exts[:byte-size total]] in 3D. *)
type ext = { name : string }

let ext_codec =
  let open Wire in
  let f_len = Field.v "name_len" uint8 in
  let f_name = Field.v "name" (byte_array ~size:(Field.ref f_len)) in
  Codec.v "Ext"
    (fun _l n -> { name = n })
    [
      Codec.( $ ) f_len (fun e -> String.length e.name);
      Codec.( $ ) f_name (fun e -> e.name);
    ]

let e2e_repeat_var_elem =
  let open Wire in
  let f_total = Field.v "total" uint8 in
  let f_exts =
    Field.repeat "exts" ~size:(Field.ref f_total) (codec ext_codec)
  in
  Codec.v "Exts"
    (fun _t xs -> xs)
    [
      Codec.( $ ) f_total (fun xs ->
          List.fold_left (fun a e -> a + 1 + String.length e.name) 0 xs);
      Codec.( $ ) f_exts (fun xs -> xs);
    ]

(* [Field.repeat] over a [casetype] with mixed-shape cases: bare [unit] tags
   (PAD/END) beside a length-prefixed sub-codec body (DHCP-options TLV). The
   3D projection emits the casetype dispatch decl plus [DhcpOpt opts[:byte-size
   total]]; this is the one shape that earlier tripped the codec's repeat
   element path. *)
type dhcp_opt = Pad | End | Generic of string

let dhcp_opt_body_codec =
  let open Wire in
  let f_len = Field.v "len" uint8 in
  let f_data = Field.v "data" (byte_array ~size:(Field.ref f_len)) in
  Codec.v "DhcpOptBody"
    (fun _l d -> d)
    [
      Codec.( $ ) f_len (fun d -> String.length d);
      Codec.( $ ) f_data (fun d -> d);
    ]

let e2e_repeat_casetype_codec =
  let open Wire in
  let opt : dhcp_opt typ =
    casetype "DhcpOpt" uint8
      [
        case ~index:0 empty
          ~inject:(fun () -> Pad)
          ~project:(function Pad -> Some () | _ -> None);
        case ~index:255 empty
          ~inject:(fun () -> End)
          ~project:(function End -> Some () | _ -> None);
        case ~index:53
          (codec dhcp_opt_body_codec)
          ~inject:(fun d -> Generic d)
          ~project:(function Generic d -> Some d | _ -> None);
      ]
  in
  let size = function Pad | End -> 1 | Generic d -> 2 + String.length d in
  let f_total = Field.v "total" uint8 in
  let f_opts = Field.repeat "opts" ~size:(Field.ref f_total) opt in
  Codec.v "DhcpOpts"
    (fun _t xs -> xs)
    [
      Codec.( $ ) f_total (List.fold_left (fun a o -> a + size o) 0);
      Codec.( $ ) f_opts (fun xs -> xs);
    ]

(* Zero-terminated strings: [name] runs to a NUL, [tag] is NUL-terminated
   within a fixed region, then a trailing scalar. Projects to the 3D
   [field[:zeroterm]] and [field[:zeroterm-byte-size-at-most n]] forms. *)
let e2e_zeroterm_codec =
  let open Wire in
  let f_name = Field.v "name" zeroterm in
  let f_tag = Field.v "tag" (zeroterm_at_most ~size:(int 8)) in
  let f_n = Field.v "n" uint8 in
  Codec.v "ZtRec"
    (fun name tag n -> (name, tag, n))
    [
      Codec.( $ ) f_name (fun (s, _, _) -> s);
      Codec.( $ ) f_tag (fun (_, t, _) -> t);
      Codec.( $ ) f_n (fun (_, _, n) -> n);
    ]

(* Embedded variable-size sub-codec via the WireSet* setter path, with a
   field after it (SSH Disconnect shape: a length-prefixed [SshString] is not
   "readable", so it must be handed to the setter by offset rather than by
   value; otherwise EverParse fails with "Parse_with_dep_action: tag not
   readable"). *)
let e2e_embedded_var_codec =
  let open Wire in
  let f_len = Field.v "Length" uint32be in
  let ssh_string =
    Codec.v "SshString"
      (fun len data -> (len, data))
      [
        Codec.( $ ) f_len (fun (l, _) -> l);
        Codec.( $ )
          (Field.v "Bytes" (byte_slice ~size:(Field.ref f_len)))
          (fun (_, d) -> d);
      ]
  in
  let f_reason = Field.v "ReasonCode" uint32be in
  let f_desc = Field.v "Description" (codec ssh_string) in
  let f_lang = Field.v "Language" (codec ssh_string) in
  let open Codec in
  v "Disconnect"
    (fun r d l -> (r, d, l))
    [
      (f_reason $ fun (r, _, _) -> r);
      (f_desc $ fun (_, d, _) -> d);
      (f_lang $ fun (_, _, l) -> l);
    ]

(* [Field.repeat] over a zeroterm element: a count-prefixed list of
   NUL-terminated strings. Projects through a synthesised element struct. *)
let e2e_repeat_zeroterm_codec =
  let open Wire in
  let f_n = Field.v "N" uint16be in
  let f_names = Field.repeat "Names" ~size:(Field.ref f_n) zeroterm in
  let open Codec in
  v "ZtRep" (fun n names -> (n, names)) [ f_n $ fst; f_names $ snd ]

(* A dynamic [Field.optional] over a variable-size inner: a self-delimiting
   sub-codec (projects as a gate-dispatched casetype) and a byte array sized by
   a prior field (projects as a conditional byte region). Both gated on the same
   flag. *)
let e2e_optional_var_codec =
  let open Wire in
  let f_len = Field.v "Length" uint32be in
  let opt_str =
    Codec.v "OptStr"
      (fun len data -> (len, data))
      [
        Codec.( $ ) f_len (fun (l, _) -> l);
        Codec.( $ )
          (Field.v "Bytes" (byte_slice ~size:(Field.ref f_len)))
          (fun (_, d) -> d);
      ]
  in
  let f_gate = Field.v "Gate" uint8 in
  let cond = Expr.(Field.ref f_gate <> int 0) in
  let f_desc = Field.optional "Desc" ~present:cond (codec opt_str) in
  let f_blen = Field.v "BodyLen" uint8 in
  let f_body =
    Field.optional "Body" ~present:cond (byte_array ~size:(Field.ref f_blen))
  in
  let open Codec in
  v "OptVarRec"
    (fun g d bl b -> (g, d, bl, b))
    [
      (f_gate $ fun (g, _, _, _) -> g);
      (f_desc $ fun (_, d, _, _) -> d);
      (f_blen $ fun (_, _, bl, _) -> bl);
      (f_body $ fun (_, _, _, b) -> b);
    ]

(* [Wire.array] over a fixed byte_array element: a fixed-count list of n-byte
   chunks (e.g. an array of IPv4 addresses). Projects the element as bare
   [UINT8] under a [len * width] byte budget. *)
let e2e_array_byte_chunks_codec =
  let open Wire in
  let f_tag = Field.v "Tag" uint8 in
  let f_addrs =
    Field.v "Addrs" (array ~len:(int 3) (byte_array ~size:(int 4)))
  in
  let open Codec in
  v "ArrChunks" (fun tag addrs -> (tag, addrs)) [ f_tag $ fst; f_addrs $ snd ]

(* [Field.repeat] over a fixed byte_array element: a count-prefixed list of
   n-byte chunks. Projects the element as bare [UINT8] under the byte budget. *)
let e2e_repeat_byte_chunks_codec =
  let open Wire in
  let f_n = Field.v "N" uint16be in
  let f_chunks =
    Field.repeat "Chunks" ~size:(Field.ref f_n) (byte_array ~size:(int 4))
  in
  let open Codec in
  v "RepChunks" (fun n chunks -> (n, chunks)) [ f_n $ fst; f_chunks $ snd ]

(* A codec whose name carries a capital [V] before the [Validate] keyword
   (e.g. [VeritySuperblockValidateVeritySuperblock] in the generated [.h]).
   [read_validate_name] must find [Validate] past that leading [V] rather than
   stopping at the first [V] it sees, or C generation fails outright. *)
let e2e_vname_codec =
  let open Wire in
  let f_sig = Field.v "signature" (byte_array ~size:(int 8)) in
  let f_version = Field.v "version" uint32 in
  let open Codec in
  v "VeritySuperblock" (fun s v -> (s, v)) [ f_sig $ fst; f_version $ snd ]

let test_e2e_compile_run () =
  compile_and_run ~name:"Demo" e2e_simple_codec;
  compile_and_run ~name:"VeritySuperblock" e2e_vname_codec;
  compile_and_run ~name:"ZtRep" e2e_repeat_zeroterm_codec;
  compile_and_run ~name:"RepChunks" e2e_repeat_byte_chunks_codec;
  compile_and_run ~name:"ArrChunks" e2e_array_byte_chunks_codec;
  compile_and_run ~name:"Disconnect" e2e_embedded_var_codec;
  compile_and_run ~name:"OptVarRec" e2e_optional_var_codec;
  compile_and_run ~name:"DhcpOpts" e2e_repeat_casetype_codec;
  compile_and_run ~name:"ZtRec" e2e_zeroterm_codec;
  compile_and_run ~name:"CLCW" e2e_allcaps_codec;
  compile_and_run ~name:"TMFrame" e2e_tm_codec;
  compile_and_run ~name:"Ctt" e2e_casetype_codec;
  compile_and_run ~name:"Sshauth" e2e_ssh_casetype_codec;
  compile_and_run ~name:"ChannelOpen" e2e_codec_in_casetype;
  compile_and_run ~name:"Exts" e2e_repeat_var_elem;
  compile_and_run ~name:"rpmsg_endpoint_info" e2e_snake_codec;
  compile_and_run ~name:"EP_Header" e2e_mixed_codec;
  compile_and_run ~name:"UnderflowCheck" e2e_underflow_codec

let test_uses_wire_ctx () =
  let s = Wire.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
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
  let _ =
    (Wire_3d.main
      : ?name:string ->
        mode:[ `Ffi | `Standalone ] ->
        package:string ->
        Wire_3d.packed list ->
        unit)
  in
  ()

(* Adversarial: [schema.wire_size] must agree with [Codec.wire_size] AND with
   the byte count we expect by hand for every bitfield layout. Two layers:
   if the projection silently drifts from [Codec.wire_size] we validate the
   wrong amount of data; if the expected value is itself wrong, hand-computed
   constants catch that too. *)
let check_size ~name ~expected codec =
  let schema = Everparse.project ~mode:`Ffi codec in
  let codec_size = Codec.wire_size codec in
  Alcotest.(check int)
    (Fmt.str "%s: Codec.wire_size = %d" name expected)
    expected codec_size;
  Alcotest.(check (option int))
    (Fmt.str "%s: schema.wire_size = %d" name expected)
    (Some expected) schema.Everparse.wire_size

let pack_four_u8 name ta tb tc td =
  let fa = Field.v "a" ta in
  let fb = Field.v "b" tb in
  let fc = Field.v "c" tc in
  let fd = Field.v "d" td in
  let open Codec in
  v name
    (fun a b c d -> (a, b, c, d))
    [
      (fa $ fun (a, _, _, _) -> a);
      (fb $ fun (_, b, _, _) -> b);
      (fc $ fun (_, _, c, _) -> c);
      (fd $ fun (_, _, _, d) -> d);
    ]

(* SSID-like layout: mix of [bit (bits 1 U8)] and bare [bits N U8] summing to
   exactly one U8. Historical bug: the Map-wrapped fields fell out of the bit
   group and produced spurious anon padding. *)
let ssid_style_codec =
  pack_four_u8 "SsidStyle"
    (bit (bits ~width:1 U8))
    (bits ~width:2 U8) (bits ~width:4 U8)
    (bit (bits ~width:1 U8))

(* All [bit] (Map-wrapped), packed into one U8. *)
let all_bool_codec =
  let b = bit (bits ~width:1 U8) in
  pack_four_u8 "AllBool" b b b b

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
  let schema = Everparse.Raw.project_struct ~mode:`Ffi s in
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
    (contains_exact "rpmsg_endpoint_info.h");
  Alcotest.(check bool)
    "C rule gated behind BUILD_EVERPARSE, not fallback" true
    (contains_exact "%{env:BUILD_EVERPARSE=}");
  Alcotest.(check bool)
    "C rule never auto-regenerates" false
    (contains_exact "mode fallback")

(* -- Adversarial projection tests for 3D-shape transformations -- *)

let to_3d_string s =
  let m = Wire.Everparse.Raw.module_ [ Wire.Everparse.Raw.typedef s ] in
  Wire.Everparse.Raw.to_3d m

let contains s sub =
  let lsub = String.length sub in
  let n = String.length s - lsub in
  let rec go i = i <= n && (String.sub s i lsub = sub || go (i + 1)) in
  lsub = 0 || go 0

(* [Action.var name e] must not project to [var name = e;] -- 3D's [var]
   binds an atomic action, not a pure expression. The DSL combinator [Var]
   has to be lowered by inlining the bound expression at every use site. *)
let test_var_inlines () =
  let f_a = field "Tag" uint8 in
  let f_b = field "Value" uint16be in
  let f_x = field "x" uint16be in
  let s =
    let out = Param.output "out" uint32be in
    param_struct "VarTest"
      [ Param.decl out ]
      [
        field "Tag" uint8;
        field "Value"
          ~action:
            (Action.on_act
               [
                 Action.var "x" Expr.(field_ref f_a + field_ref f_b);
                 Action.assign out Expr.(field_ref f_x + int 1);
               ])
          uint16be;
      ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "no [var x = ...] in 3D output" false (contains out "var x");
  Alcotest.(check bool)
    "x usage replaced by inlined expression" true
    (contains out "(Tag + Value) + 1")

(* Var bindings inside if-branches must not leak between sibling branches. *)
let test_var_scoping_in_if () =
  let f_a = field "Tag" uint8 in
  let f_x = field "x" uint16be in
  let s =
    let out = Param.output "out" uint32be in
    param_struct "VarIf"
      [ Param.decl out ]
      [
        field "Tag" uint8;
        field "Value"
          ~action:
            (Action.on_act
               [
                 Action.var "x" Expr.(field_ref f_a + int 7);
                 Action.if_
                   Expr.(field_ref f_x > int 100)
                   [ Action.assign out (field_ref f_x) ]
                   (Some [ Action.assign out (int 0) ]);
               ])
          uint8;
      ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "no var leak" false (contains out "var x");
  Alcotest.(check bool)
    "inlined inside then-branch" true
    (contains out "*out = (Tag + 7);");
  Alcotest.(check bool)
    "inlined inside cond" true
    (contains out "(Tag + 7) > 100")

(* Struct-level [where] referencing fields must lower to a constraint on
   the latest referenced field, not stay at struct level. 3D's [where]
   only sees parameters. *)
let test_where_lowers_to_field () =
  let f_len = field "Length" uint16be in
  let f_max = field "max" uint16be in
  let s =
    param_struct "WhereTest"
      [ param "max" uint16be ]
      ~where:Expr.(field_ref f_len <= field_ref f_max)
      [ field "Length" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "no top-level where" false (contains out "where (");
  Alcotest.(check bool)
    "constraint moved onto the field" true
    (contains out "Length <= max")

(* A param-only [where] must remain at the struct level. *)
let test_where_param_only_stays () =
  let s =
    let max_ = Param.input "max" uint16be in
    let lim = Param.input "lim" uint16be in
    param_struct "WhereParam"
      [ Param.decl max_; Param.decl lim ]
      ~where:Expr.(Param.expr max_ <= Param.expr lim)
      [ field "Length" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "where stays at struct level" true (contains out "where (")

(* The [where] must combine with any pre-existing field constraint via AND. *)
let test_where_combines_field_constraint () =
  let f_self = field "Length" uint16be in
  let f_len =
    field "Length" ~constraint_:Expr.(field_ref f_self > int 0) uint16be
  in
  let s =
    param_struct "WhereAnd" []
      ~where:Expr.(field_ref f_len < int 1024)
      [ f_len ]
  in
  let out = to_3d_string s in
  (* Both clauses must end up next to the field. *)
  Alcotest.(check bool) "left clause present" true (contains out "Length > 0");
  Alcotest.(check bool)
    "right clause present" true
    (contains out "Length < 1024")

(* Param/field names that collide with 3D or F* keywords must be escaped
   in the projected 3D output (not rejected at the API). The user keeps
   writing [Param.input "total" ...], the 3D output reads [total_]. *)
let test_keyword_param_escaped () =
  let p = Param.input "total" uint32be in
  let f = field "data" (byte_array ~size:(Param.expr p)) in
  let s = param_struct "Esc" [ Param.decl p ] [ f ] in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "param decl uses escaped name" true
    (contains out "UINT32BE total_");
  Alcotest.(check bool)
    "param ref uses escaped name" true
    (contains out "byte-size total_");
  Alcotest.(check bool)
    "raw [total] keyword does not appear" false
    (Re.execp (Re.compile (Re.seq [ Re.bow; Re.str "total"; Re.eow ])) out)

let expect_invalid_arg ~combinator f =
  match
    try
      let _ = f () in
      Ok ()
    with Invalid_argument msg -> Error msg
  with
  | Ok () ->
      Alcotest.failf "%s should have rejected the construction" combinator
  | Error _ -> ()

(* [array ~len:N (byte_array ~size:M)] is valid -- it projects to
   [UINT8 name[:byte-size (N * M)]]. The smart constructor only refuses
   element types that have no derivable wire size expression at all. *)
let test_array_accepts_sized_elem () =
  let s =
    struct_ "ArrSized"
      [ field "Items" (array ~len:(int 3) (byte_array ~size:(int 5))) ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "byte-size suffix uses len * elem_size" true (contains out "byte-size")

let test_array_rejects_unsized_elem () =
  expect_invalid_arg ~combinator:"array (all_bytes)" (fun () ->
      array ~len:(int 4) all_bytes)

(* [Field.optional] accepts any element with a derivable size -- plain
   scalars, fixed bitfields, AND already-sized payloads like
   [byte_array {size}]. The projection emits a [byte-size if(cond, N, 0)]
   suffix that uses the inner's size expression. *)
let test_field_optional_byte_array () =
  let f_present = Field.v "present" uint8 in
  let cond = Expr.(Field.ref f_present <> int 0) in
  let codec =
    Codec.v "OptByteArr"
      (fun present body -> (present, body))
      Codec.
        [
          f_present $ fst;
          Field.optional "Body" ~present:cond (byte_array ~size:(int 8)) $ snd;
        ]
  in
  let s = Everparse.Raw.struct_of_codec codec in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "byte-size suffix uses inner size" true (contains out "8")

(* A [nested ~size] / [nested_at_most] field projects through EverParse:
   byte-span and enum inners go through a synthesised wrapper struct, and scalar
   and sub-record inners render inline (rather than as an extern setter param
   typed [UINT8[:byte-size-single-element-array]], which is invalid 3D).
   [generate_c] runs [3d.exe] and raises if the .3d is rejected, so this is the
   regression guard. *)
let test_nested_field_projects () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else begin
    let inner =
      Codec.v "NInner"
        (fun a b -> (a, b))
        Codec.[ Field.v "a" uint8 $ fst; Field.v "b" uint16be $ snd ]
    in
    let codec =
      Codec.v "NestedProj"
        (fun scalar bytes sub -> (scalar, bytes, sub))
        Codec.
          [
            (Field.v "scalar" (nested ~size:(int 4) uint8) $ fun (s, _, _) -> s);
            ( Field.v "bytes" (nested ~size:(int 4) (byte_array ~size:(int 3)))
            $ fun (_, b, _) -> b );
            ( Field.v "sub" (nested ~size:(int 8) (codec inner))
            $ fun (_, _, s) -> s );
          ]
    in
    let dir = Filename.temp_dir "wire_3d_nested" "" in
    let s = Everparse.project ~mode:`Ffi codec in
    Wire_3d.generate_3d ~outdir:dir [ s ];
    (* Runs 3d.exe; raises if EverParse rejects the generated .3d. *)
    Wire_3d.generate_c ~outdir:dir [ s ];
    Alcotest.(check bool)
      "C generated for nested fields" true
      (Sys.file_exists (Filename.concat dir "NestedProj.c"))
  end

let test_keyword_field_escaped () =
  let s =
    struct_ "Esc2"
      [ field "let" uint8; field "match" uint8; field "type" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "let_ field" true (contains out "let_");
  Alcotest.(check bool) "match_ field" true (contains out "match_");
  Alcotest.(check bool) "type_ field" true (contains out "type_")

let suite =
  ( "wire_3d",
    [
      Alcotest.test_case "everparse_name" `Quick test_everparse_name;
      Alcotest.test_case "pascal_case" `Quick test_pascal_case;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "schema_of_struct" `Quick test_schema_of_struct;
      Alcotest.test_case "ensure_dir" `Quick test_ensure_dir;
      Alcotest.test_case "generate_c (needs 3d.exe)" `Quick test_generate_c;
      Alcotest.test_case "generate_dune_standalone" `Quick
        test_generate_dune_standalone;
      Alcotest.test_case "generate_dune_standalone name override" `Quick
        test_generate_dune_standalone_name_override;
      Alcotest.test_case "generate_dune_standalone context policy" `Quick
        test_generate_dune_standalone_context_policy;
      Alcotest.test_case "generate_standalone (needs 3d.exe)" `Quick
        test_generate_standalone;
      Alcotest.test_case "archive hides raw validators (needs 3d.exe)" `Quick
        test_archive_hides_raw_validators;
      Alcotest.test_case "doc differential (needs 3d.exe)" `Quick
        test_doc_differential;
      Alcotest.test_case "doc differential no params (needs 3d.exe)" `Quick
        test_doc_differential_no_params;
      Alcotest.test_case "doc differential signed (needs 3d.exe)" `Quick
        test_doc_differential_signed;
      Alcotest.test_case "doc differential large payload (needs 3d.exe)" `Quick
        test_doc_differential_large_payload;
      Alcotest.test_case "doc differential caps name (needs 3d.exe)" `Quick
        test_doc_differential_caps_name;
      Alcotest.test_case "doc differential trailing bytes (needs 3d.exe)" `Quick
        test_doc_differential_trailing_bytes;
      Alcotest.test_case "uses_wire_ctx" `Quick test_uses_wire_ctx;
      Alcotest.test_case "has_3d_exe" `Quick test_has_3d_exe;
      Alcotest.test_case "main exists" `Quick test_main_exists;
      Alcotest.test_case "projection: wire_size matches Codec" `Quick
        test_projection_sizes;
      Alcotest.test_case "projection: filenames match EverParse output" `Quick
        test_projection_filenames;
      Alcotest.test_case "projection: var inlines into uses" `Quick
        test_var_inlines;
      Alcotest.test_case "projection: var scoping inside if" `Quick
        test_var_scoping_in_if;
      Alcotest.test_case "projection: where lowers to field constraint" `Quick
        test_where_lowers_to_field;
      Alcotest.test_case "projection: param-only where stays at struct" `Quick
        test_where_param_only_stays;
      Alcotest.test_case "projection: where ANDs with field constraint" `Quick
        test_where_combines_field_constraint;
      Alcotest.test_case "projection: F*-keyword param escaped" `Quick
        test_keyword_param_escaped;
      Alcotest.test_case "projection: F*-keyword field escaped" `Quick
        test_keyword_field_escaped;
      Alcotest.test_case "ctor: array accepts sized variable elem" `Quick
        test_array_accepts_sized_elem;
      Alcotest.test_case "ctor: array rejects unsized elem" `Quick
        test_array_rejects_unsized_elem;
      Alcotest.test_case "Field.optional: byte_array inner projects" `Quick
        test_field_optional_byte_array;
      Alcotest.test_case "nested field projects through EverParse" `Slow
        test_nested_field_projects;
      Alcotest.test_case "e2e: compile + run across naming conventions" `Slow
        test_e2e_compile_run;
    ] )
