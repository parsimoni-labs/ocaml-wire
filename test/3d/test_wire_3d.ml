(** Tests for the EverParse pipeline -- schema generation and 3D output. *)

open Wire
open Wire.Everparse.Raw

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let simple_module = module_ [ typedef ~entrypoint:true simple_struct ]

(* Everything below that shells out to EverParse needs [3d.exe]. Reporting those
   cases as [OK] when the binary is absent would print a green tick for an
   assertion that never ran, so they skip instead: the four default [dune
   runtest] lanes have no EverParse and must say so. *)
let needs_3d_exe () = if not (Wire_3d.has_3d_exe ()) then Alcotest.skip ()

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
  Alcotest.(check string) "empty" "" (Wire_3d.everparse_name "");
  (* A capital after a digit is lowercased, as EverParse does. *)
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string) name expected (Wire_3d.everparse_name name))
    [
      ("Foo2Bar", "Foo2bar");
      ("TPM2B", "Tpm2b");
      ("ScanBitGuardsU16BEMsb", "ScanBitGuardsU16bemsb");
    ]

(* [pascal_case] mirrors EverParse's wrapper-symbol mangling. EverParse builds a
   wrapper as [pascal_case (module ^ "_check_" ^ codec)]; these expectations were
   read straight off EverParse's generated [<Module>Wrapper.h]. *)
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

(* Read to the end rather than [in_channel_length] bytes: on Windows a text
   channel turns CRLF into LF, so it holds fewer bytes than the file. *)
let read_file path = In_channel.with_open_text path In_channel.input_all

let test_generate_c () =
  needs_3d_exe ();
  begin
    let tmpdir = Filename.temp_dir "wire_3d_gen_c" "" in
    let s = Wire.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
    Wire_3d.generate_3d ~outdir:tmpdir [ s ];
    Wire_3d.generate_c ~outdir:tmpdir [ s ];
    let c_path = Filename.concat tmpdir "TestSimple.c" in
    Alcotest.(check bool) "C file generated" true (Sys.file_exists c_path);
    let ext_path = Filename.concat tmpdir "TestSimple_ExternalTypedefs.h" in
    Alcotest.(check bool)
      "ExternalTypedefs.h generated" true (Sys.file_exists ext_path);
    let provenance = Filename.concat tmpdir "TestSimple.provenance" in
    Alcotest.(check bool)
      "provenance generated" true
      (Sys.file_exists provenance);
    let contents = read_file provenance in
    Alcotest.(check bool)
      "provenance records schema BLAKE2b-256" true
      (Re.execp
         (Re.compile
            (Re.Perl.re
               "schema-blake2b-256: [0-9a-f]{64}\\neverparse: EverParse/3d "))
         contents)
  end

(* 3d.exe writes its own [EverParseEndianness.h] on every run, so the
   freestanding byte-order branch is only useful if it is spliced into that
   copy afterwards. Without it the shipped header stops at [#error
   "Unsupported platform"] for any target that is neither Linux/glibc nor
   Apple, which is the OS-less cross target the standalone C archive exists to
   serve. *)
let test_endianness_freestanding () =
  if not (Wire_3d.has_3d_exe ()) then ()
  else begin
    let tmpdir = Filename.temp_dir "wire_3d_endianness" "" in
    let s = Wire.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
    Wire_3d.generate_3d ~outdir:tmpdir [ s ];
    Wire_3d.generate_c ~outdir:tmpdir [ s ];
    let header = read_file (Filename.concat tmpdir "EverParseEndianness.h") in
    let has sub = Re.execp (Re.compile (Re.str sub)) header in
    Alcotest.(check bool)
      "byte order comes from the compiler" true (has "__BYTE_ORDER__");
    Alcotest.(check bool)
      "byte swapping uses compiler builtins" true (has "__builtin_bswap32");
    (* The [#error] fallthrough stays as the last resort for a compiler that
       exposes neither an OS nor [__BYTE_ORDER__]; what matters is that a
       freestanding GCC or Clang target no longer reaches it. Compile one to
       prove it, undefining the two OS macros the header keys on. Skip the
       compile where the host's own system headers do not survive those
       undefines: the text assertions above still stand. *)
    let write path contents =
      Out_channel.with_open_bin path (fun oc ->
          Out_channel.output_string oc contents)
    in
    write (Filename.concat tmpdir "probe.c") "#include <stdint.h>\n";
    write
      (Filename.concat tmpdir "freestanding.c")
      "#include \"EverParseEndianness.h\"\n\
       uint32_t swap(uint32_t x);\n\
       uint32_t swap(uint32_t x) { return be32toh(x); }\n";
    let compile file =
      Fmt.kstr Sys.command
        "cd %s && cc -std=c11 -U__APPLE__ -U__linux__ -I . -c %s -o %s > %s \
         2>&1"
        (Filename.quote tmpdir) file Filename.null Filename.null
    in
    let freestanding_host = compile "probe.c" = 0 in
    let status = if freestanding_host then compile "freestanding.c" else 0 in
    ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
    if freestanding_host then
      Alcotest.(check int) "compiles for a target with no OS endian.h" 0 status
  end

(* Both the [.3d] file name and the C identifier are many-to-one mangles of
   the codec name, so a collision has one codec's artifacts overwrite
   another's with no diagnostic: the verified C then enforces the surviving
   spec while the OCaml side still parses with both. *)
let schema_named name =
  Wire.Everparse.Raw.project_struct ~mode:`Ffi
    (struct_ name [ field "x" uint8 ])

let test_name_collision () =
  let tmpdir = Filename.temp_dir "wire_3d_collision" "" in
  let rejects what generate names substrings =
    match generate (List.map schema_named names) with
    | () -> Alcotest.failf "%s: collision accepted" what
    | exception Invalid_argument msg ->
        List.iter
          (fun sub ->
            if not (Re.execp (Re.compile (Re.str sub)) msg) then
              Alcotest.failf "%s: error %S does not mention %S" what msg sub)
          substrings
  in
  let generate_3d schemas = Wire_3d.generate_3d ~outdir:tmpdir schemas in
  let generate_dune schemas =
    Wire_3d.generate_dune ~outdir:tmpdir ~package:"pkg" schemas
  in
  (* [header] and [Header] both write [Header.3d]. *)
  rejects "same .3d file" generate_3d [ "header"; "Header" ]
    [ "\"header\""; "\"Header\""; "Header.3d" ];
  rejects "same .3d file (dune)" generate_dune [ "header"; "Header" ]
    [ "Header.3d" ];
  (* [TMFrame] and [Tmframe] write two files that share the [TmframeFields]
     plug struct tag and the [TMFRAME_FIELDS_H] include guard, so a
     translation unit including both takes one plug layout for the other. *)
  rejects "same C identifier" generate_3d [ "TMFrame"; "Tmframe" ]
    [ "\"TMFrame\""; "\"Tmframe\""; "Tmframe" ];
  rejects "same C identifier (plug)"
    (fun schemas -> Wire_3d.write_fields ~outdir:tmpdir schemas)
    [ "TMFrame"; "Tmframe" ] [ "Tmframe" ];
  rejects "post-digit C identifier" generate_3d [ "Foo2Bar"; "Foo2bar" ]
    [ "Foo2bar" ];
  (* [FooBar.3d] and [Foobar.3d] are one file where the file system folds
     case. *)
  rejects "files differing in case" generate_3d [ "FooBar"; "Foobar" ]
    [ "\"FooBar\""; "\"Foobar\""; "Foobar.3d" ];
  (* Distinct names still generate. *)
  generate_3d [ schema_named "Alpha"; schema_named "Beta" ];
  Alcotest.(check bool)
    "distinct names generate" true
    (Sys.file_exists (Filename.concat tmpdir "Alpha.3d")
    && Sys.file_exists (Filename.concat tmpdir "Beta.3d"));
  Wire_3d.rm_rf tmpdir

(* Schema names become 3D module names, generated C identifiers, dune atoms,
   and output file basenames. Reject anything outside their common portable
   identifier subset before opening even an earlier schema's output file. *)
let test_output_name_validation () =
  let schema name =
    Wire.Everparse.Raw.of_module ~name ~module_:simple_module ~wire_size:3
  in
  let valid = "mIxEd_Case9" in
  let invalid =
    [
      "";
      "../escaped";
      "dir/escaped";
      "dir\\escaped";
      "/absolute";
      "bad\nname";
      "has-hyphen";
      "9starts_with_digit";
      "NUL";
      "com1";
    ]
  in
  let with_tmp f =
    let tmpdir = Filename.temp_dir "wire_3d_name" "" in
    Fun.protect ~finally:(fun () -> Wire_3d.rm_rf tmpdir) (fun () -> f tmpdir)
  in
  let rejects mode name generate =
    with_tmp (fun outdir ->
        match generate outdir with
        | () -> Alcotest.failf "%s accepted invalid output name %S" mode name
        | exception Invalid_argument msg ->
            Alcotest.(check bool)
              (Fmt.str "%s error identifies %S" mode name)
              true
              (Re.execp (Re.compile (Fmt.kstr Re.str "%S" name)) msg);
            Alcotest.(check (array string))
              (Fmt.str "%s writes no files for %S" mode name)
              [||] (Sys.readdir outdir))
  in
  List.iter
    (fun name ->
      rejects "FFI" name (fun outdir ->
          Wire.Everparse.write ~mode:`Ffi ~outdir [ schema valid; schema name ]);
      rejects "standalone" name (fun outdir ->
          Wire.Everparse.write ~mode:`Standalone ~outdir ~name [ schema valid ]))
    invalid;
  let expected = "MIxEd_Case9.3d" in
  with_tmp (fun outdir ->
      Wire.Everparse.write ~mode:`Ffi ~outdir [ schema valid ];
      Alcotest.(check bool)
        "FFI accepts mixed-case identifier" true
        (Sys.file_exists (Filename.concat outdir expected)));
  with_tmp (fun outdir ->
      Wire.Everparse.write ~mode:`Standalone ~outdir ~name:valid
        [ schema valid ];
      Alcotest.(check bool)
        "standalone accepts mixed-case identifier" true
        (Sys.file_exists (Filename.concat outdir expected)))

let raises_failure f =
  match f () with () -> false | exception Failure _ -> true

(* The stamp is what stops a committed validator outliving the spec it was
   generated from, so cover both verdicts here rather than only the rule text.
   No 3d.exe needed: the check is deliberately hermetic. *)
let test_check_provenance () =
  let tmpdir = Filename.temp_dir "wire_3d_provenance" "" in
  let three_d = "Foo.3d" in
  let schema = Filename.concat tmpdir three_d in
  let stamp = Filename.concat tmpdir "Foo.provenance" in
  let write path contents =
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc contents)
  in
  write schema "entrypoint\ntypedef struct WireFoo\n{\n   UINT8 A;\n} Foo;\n";
  let stamp_of digest =
    Fmt.str "schema-blake2b-256: %s\neverparse: EverParse/3d vtest\n" digest
  in
  write stamp (stamp_of Digest.BLAKE256.(to_hex (file schema)));
  Wire_3d.check_provenance ~outdir:tmpdir [ three_d ];
  write schema "entrypoint\ntypedef struct WireFoo\n{\n   UINT16 A;\n} Foo;\n";
  Alcotest.(check bool)
    "spec edited after generation is rejected" true
    (raises_failure (fun () ->
         Wire_3d.check_provenance ~outdir:tmpdir [ three_d ]));
  write stamp "everparse: EverParse/3d vtest\n";
  Alcotest.(check bool)
    "stamp without a recorded hash is rejected" true
    (raises_failure (fun () ->
         Wire_3d.check_provenance ~outdir:tmpdir [ three_d ]));
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir)

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
    "exports the wrapper derived from the struct tag" true
    (has "MyPkgCheckWirePkt");
  Alcotest.(check bool)
    "runtest runs the differential check" true
    (has "%{dep:agree} corpus");
  Alcotest.(check bool) "corpus oracle" true (has "%{exe:gen.exe} corpus");
  Alcotest.(check bool)
    "runtest regenerates the spec" true
    (has "targets MyPkg.3d.gen");
  Alcotest.(check bool)
    "runtest diffs the committed spec" true
    (has "(diff MyPkg.3d MyPkg.3d.gen)");
  (* The install stanza depends on the committed spec, so a rule that produces
     it runs on any build that installs anything, and promotion overwrites the
     file the diff above is meant to police. Keep it a plain source: the diff
     then reports drift, and [dune promote] writes the new bytes on request. *)
  Alcotest.(check bool)
    "committed spec is a source, not a rule target" false
    (has "(targets MyPkg.3d)");
  Alcotest.(check bool)
    "runtest regenerates dune.inc" true
    (has "targets dune.inc.gen");
  Alcotest.(check bool)
    "runtest diffs the committed dune.inc" true
    (has "(diff dune.inc dune.inc.gen)");
  Alcotest.(check bool)
    "C regeneration writes provenance" true (has "MyPkg.provenance");
  Alcotest.(check bool)
    "runtest checks provenance" true
    (has "%{exe:gen.exe} provenance-check");
  Alcotest.(check bool)
    "stale provenance is not promotable" false
    (has "MyPkg.provenance.gen");
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
  needs_3d_exe ();
  begin
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

let diff_nested_exact_codec =
  let open Wire in
  Codec.v "NestedExact" Fun.id
    Codec.[ Field.v "value" (nested ~size:(int 4) zeroterm) $ Fun.id ]

let diff_nested_at_most_codec =
  let open Wire in
  Codec.v "NestedAtMost" Fun.id
    Codec.[ Field.v "value" (nested_at_most ~size:(int 4) zeroterm) $ Fun.id ]

let diff_array_exact_codec =
  let open Wire in
  Codec.v "ArrayExact" Fun.id
    Codec.[ Field.v "items" (array ~len:(int 3) uint8) $ Fun.id ]

let diff_repeat_exact_codec =
  let open Wire in
  Codec.v "RepeatExact" Fun.id
    Codec.[ Field.repeat "items" ~size:(int 4) uint16be $ Fun.id ]

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

(* Build the package's validator and [agree] once, then pass [f] a function that
   runs [agree] on a corpus and returns its exit status and output. *)
let with_agree ~name ~package codecs f =
  let tmpdir = Filename.temp_dir ("wire_3d_diff_" ^ package) "" in
  Fun.protect
    ~finally:(fun () -> ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir))
    (fun () ->
      Wire_3d.generate_standalone ~outdir:tmpdir ~name ~package codecs;
      let sh cmd =
        let ic = Fmt.kstr Unix.open_process_in "cd %s && %s 2>&1" tmpdir cmd in
        let output = In_channel.input_all ic in
        (Unix.close_process_in ic, output)
      in
      let base = String.capitalize_ascii name in
      let run corpus =
        let oc = open_out (Filename.concat tmpdir "corpus") in
        (match corpus with
        | `Fuzz count ->
            let ppf = Format.formatter_of_out_channel oc in
            Wire_3d.generate_corpus ~count ppf codecs
        | `Lines lines -> List.iter (fun l -> output_string oc (l ^ "\n")) lines);
        close_out oc;
        sh "./agree corpus"
      in
      match
        Fmt.kstr sh
          "cc %s %s -c %s.c %sWrapper.c && ar rcs lib.a %s.o %sWrapper.o && cc \
           %s %s agree.c lib.a -o agree"
          Wire_3d.strict_cc_flags Wire_3d.everparse_type_defines base base base
          base Wire_3d.strict_cc_flags Wire_3d.everparse_type_defines
      with
      | Unix.WEXITED 0, _ -> f run
      | _, output ->
          Alcotest.failf "agree for %s does not build:\n%s" name output)

let differential_ok ~name ~package ~corpus codecs =
  let status, output =
    with_agree ~name ~package codecs (fun run -> run corpus)
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Alcotest.failf "differential %s failed with exit %d:\n%s" name n output
  | _ -> Alcotest.failf "differential %s terminated abnormally:\n%s" name output

let test_doc_differential () =
  needs_3d_exe ();
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
  needs_3d_exe ();
  differential_ok ~name:"plainspec" ~package:"plain-doc" ~corpus:(`Fuzz 100)
    [ Wire_3d.pack diff_enum_codec; Wire_3d.pack diff_range_codec ]

let test_doc_differential_nested_regions () =
  needs_3d_exe ();
  differential_ok ~name:"nestedspec" ~package:"nested-doc"
    ~corpus:
      (`Lines
         [
           "NestedExact - 41000000 0";
           "NestedExact - 41424300 1";
           "NestedAtMost - 41000000 1";
           "NestedAtMost - 41424300 1";
         ])
    [
      Wire_3d.pack diff_nested_exact_codec;
      Wire_3d.pack diff_nested_at_most_codec;
    ]

(* A constrained Msb_first group in a sub-codec, embedded as a field and as an
   array element: the C validator must read the same bits the codec does, so
   the byte the differential fuzzer found ([0xab]: [a = 5], [b = 11] from the
   top of the byte, [3] and [21] from the bottom) is accepted on both sides. *)
let diff_msb_inner base name =
  let open Wire in
  Codec.v name
    (fun a b -> (a, b))
    Codec.
      [
        Field.v "a"
          ~self_constraint:(fun r -> Expr.(r <= int 5))
          (bits ~width:3 base)
        $ fst;
        Field.v "b"
          ~self_constraint:(fun r -> Expr.(r <= int 20))
          (bits ~width:5 base)
        $ snd;
      ]

let diff_msb_field_codec =
  let open Wire in
  let inner = diff_msb_inner U8 "MsbInner" in
  Codec.v "MsbField"
    (fun g x -> (g, x))
    Codec.[ Field.v "g" (codec inner) $ fst; Field.v "x" uint8 $ snd ]

let diff_msb_array_codec =
  let open Wire in
  let inner = diff_msb_inner U16 "MsbInner16" in
  Codec.v "MsbArray" Fun.id
    Codec.[ Field.v "items" (array ~len:(int 2) (codec inner)) $ Fun.id ]

let test_doc_differential_msb_sub_codec () =
  needs_3d_exe ();
  differential_ok ~name:"msbspec" ~package:"msb-doc"
    ~corpus:
      (`Lines
         [
           "MsbField - ab00 1";
           "MsbField - c000 0";
           "MsbField - 1500 0";
           "MsbArray - 00ab00ab 1";
           "MsbArray - 0015a000 0";
         ])
    [ Wire_3d.pack diff_msb_field_codec; Wire_3d.pack diff_msb_array_codec ]

let test_doc_differential_region_cardinality () =
  needs_3d_exe ();
  differential_ok ~name:"invariantspec" ~package:"invariant-doc"
    ~corpus:
      (`Lines
         [
           "ArrayExact - 010203 1";
           "ArrayExact - 0102 0";
           "ArrayExact - 01020304 0";
           "RepeatExact - 00010002 1";
           "RepeatExact - 0001 0";
           "RepeatExact - 000100020003 0";
           "NestedExact - 41424300 1";
           "NestedExact - 41000000 0";
         ])
    [
      Wire_3d.pack diff_array_exact_codec;
      Wire_3d.pack diff_repeat_exact_codec;
      Wire_3d.pack diff_nested_exact_codec;
    ]

(* The installed standalone archive must export only the checked
   [<Base>CheckWire<Codec>] wrappers, not the raw [<Base>Validate*] entry points
   whose EverParse-emitted preamble underflows on [StartPosition > InputLength].
   Build the archive exactly as the generated dune rule does for this platform
   (via {!Wire_3d.archive_link_steps}) and assert [nm] shows no global [Validate]
   symbol. This runs the platform's real link step on CI (macOS and Linux). Needs
   3d.exe for the C. *)
let test_archive_hides_raw_validators () =
  needs_3d_exe ();
  begin
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
      Fmt.kstr
        (fun command ->
          let ic = Unix.open_process_in command in
          let out = In_channel.input_all ic in
          (out, Unix.close_process_in ic))
        "cd %s && %s 2>&1" tmpdir cmd
    in
    let build_out, build_status = run (String.concat " && " steps) in
    let nm_out, _ = Fmt.kstr run "nm %s" archive in
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
  needs_3d_exe ();
  differential_ok ~name:"signedspec" ~package:"signed-doc" ~corpus:(`Fuzz 256)
    [ Wire_3d.pack diff_signed_codec ]

(* A union switches on an earlier field in both validators: a byte tag with a
   default branch and fields either side of the union, and a bitfield tag with
   none, where an unclaimed tag is a rejection the two have to agree on. *)
let diff_union_codec =
  let open Wire in
  let f_kind = Field.v "kind" uint8 in
  let body =
    Field.union "body" ~on:f_kind ~casetype:"UnionBody"
      [
        case ~index:(UInt8.v 1) uint16be
          ~inject:(fun v -> `Short v)
          ~project:(function `Short v -> Some v | _ -> None);
        case ~index:(UInt8.v 2) uint32be
          ~inject:(fun v -> `Long v)
          ~project:(function `Long v -> Some v | _ -> None);
        default uint8
          ~inject:(fun tag v -> `Other (tag, v))
          ~project:(function `Other (tag, v) -> Some (tag, v) | _ -> None);
      ]
  in
  Codec.v "UnionMsg"
    (fun kind sum body tail -> (kind, sum, body, tail))
    Codec.
      [
        (f_kind $ fun (k, _, _, _) -> k);
        (Field.v "sum" uint32be $ fun (_, s, _, _) -> s);
        (body $ fun (_, _, b, _) -> b);
        (Field.v "tail" uint8 $ fun (_, _, _, t) -> t);
      ]

let diff_union_bits_codec =
  let open Wire in
  let f_kind = Field.v "kind" (bits ~width:4 U8) in
  Codec.v "UnionBits"
    (fun kind flags body -> (kind, flags, body))
    Codec.
      [
        (f_kind $ fun (k, _, _) -> k);
        (Field.v "flags" (bits ~width:4 U8) $ fun (_, f, _) -> f);
        ( Field.union "body" ~on:f_kind ~casetype:"UnionBitsBody"
            [
              case ~index:3 uint16be ~inject:Either.left
                ~project:Either.find_left;
              case ~index:5 uint8 ~inject:Either.right
                ~project:Either.find_right;
            ]
        $ fun (_, _, b) -> b );
      ]

let test_doc_differential_union () =
  needs_3d_exe ();
  differential_ok ~name:"unionspec" ~package:"union-doc" ~corpus:(`Fuzz 400)
    [ Wire_3d.pack diff_union_codec; Wire_3d.pack diff_union_bits_codec ]

(* An 8192-byte payload makes each corpus line 16384 hex chars; the agree
   reader's hex buffer must hold the line, or it truncates and the verdict
   misparses into a false mismatch. *)
let diff_large_payload_codec =
  let open Wire in
  Codec.v "SharedMem"
    (fun d -> d)
    [ Codec.( $ ) (Field.v "data" (byte_array ~size:(int 8192))) (fun d -> d) ]

let test_doc_differential_large_payload () =
  needs_3d_exe ();
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
  needs_3d_exe ();
  differential_ok ~name:"spacewire" ~package:"space-wire" ~corpus:(`Fuzz 40)
    [ Wire_3d.pack diff_caps_name_codec ]

(* A valid record followed by trailing bytes must be rejected. EverParse's
   stock [Check] wrapper returns TRUE on any successful validation -- success
   is the consumed position, so a valid prefix passes -- and the pipeline
   hardens it to require full consumption. The handwritten corpus pins the
   exact boundary: the 2-byte RangeMsg accepts exactly-sized input and rejects
   both one byte over and one byte short. *)
let test_doc_differential_trailing_bytes () =
  needs_3d_exe ();
  differential_ok ~name:"trailspec" ~package:"trail-doc"
    ~corpus:
      (`Lines [ "RangeMsg - 0102 1"; "RangeMsg - 010203 0"; "RangeMsg - 01 0" ])
    [ Wire_3d.pack diff_range_codec ]

(* agree.c decodes the corpus hex itself. Both cases of a digit decode alike:
   RangeMsg accepts n < 100, so a mis-decoded [A] or [F] in the first byte flips
   the verdict. A non-hex character or an odd digit count refuses the corpus
   with exit 2 instead of feeding a wrong byte to the validator. *)
let test_doc_differential_hex () =
  needs_3d_exe ();
  with_agree ~name:"hexspec" ~package:"hex-doc"
    [ Wire_3d.pack diff_range_codec ]
    (fun run ->
      (match
         run
           (`Lines
              [
                "RangeMsg - 5f00 1";
                "RangeMsg - 5F00 1";
                "RangeMsg - 6a00 0";
                "RangeMsg - 6A00 0";
                "RangeMsg - 63fF 1";
                "RangeMsg - 64Ab 0";
              ])
       with
      | Unix.WEXITED 0, _ -> ()
      | _, output -> Alcotest.failf "mixed-case hex disagrees:\n%s" output);
      List.iter
        (fun hex ->
          match run (`Lines [ "RangeMsg - " ^ hex ^ " 1" ]) with
          | Unix.WEXITED 2, output
            when Re.execp (Re.compile (Re.str "bad hex")) output ->
              ()
          | _, output ->
              Alcotest.failf "agree did not refuse hex %S:\n%s" hex output)
        [ "0g01"; "G001"; "010"; "+101" ])

(* EverParse renamed the wrapper's status local between releases ([result] up
   to v2026.02.25, [ep_status] by v2026.08.21). The consumption check has to
   name whichever one the wrapper actually binds; naming a fixed one emitted C
   referencing an undeclared identifier, which failed to compile. *)
let wrapper_src var =
  Fmt.str
    "BOOLEAN WCheckW(uint8_t *base, uint32_t len) {\n\
     \tWFields frame;\n\
     \tuint64_t %s = WValidateW( (uint8_t*)&frame, &DefaultErrorHandler, base, \
     len, 0);\n\
     \tif (EverParseIsError(%s))\n\
     \t{\n\
     \t\treturn FALSE;\n\
     \t}\n\
     \treturn TRUE;\n\
     }"
    var var

let test_wrapper_hardening_names_the_status_var () =
  List.iter
    (fun var ->
      let hardened = Wire_3d.harden_wrapper_source (wrapper_src var) in
      let want = Fmt.str "if (%s != (uint64_t) len)" var in
      if not (Re.execp (Re.compile (Re.str want)) hardened) then
        Alcotest.failf "hardened wrapper for %S lacks %S:\n%s" var want hardened)
    [ "result"; "ep_status" ]

let test_wrapper_hardening_is_idempotent () =
  let once = Wire_3d.harden_wrapper_source (wrapper_src "ep_status") in
  Alcotest.(check string)
    "already hardened" once
    (Wire_3d.harden_wrapper_source once)

let test_wrapper_hardening_refuses_unknown_shape () =
  match
    Wire_3d.harden_wrapper_source
      "BOOLEAN WCheckW(uint8_t *base, uint32_t len) { return TRUE; }"
  with
  | _ -> Alcotest.fail "expected an unrecognized wrapper shape to be refused"
  | exception Failure _ -> ()

(* End-to-end compile+run. Generates C for a schema, invokes the same
   cc command [generate_dune] emits, runs the resulting binary. This is
   the one test that catches every kind of name mismatch between what
   wire.3d emits and what EverParse actually produces: filenames in
   [dune.inc], validator function names in [test.c], setter names in
   [_Fields.c], struct types, [#include]s. If any one of them is off,
   cc/ld fails and the test fails with the actual compiler error.
   Parameterised over the schema so tricky names (underscores, all-
   caps prefixes, mixed case) each exercise the pipeline end-to-end. *)
let compile_generated ?(run = true) ~name codec =
  needs_3d_exe ();
  begin
    let tmpdir = Filename.temp_dir ("wire_3d_e2e_" ^ name) "" in
    let schema = Everparse.project ~mode:`Ffi codec in
    Wire_3d.generate_3d ~outdir:tmpdir [ schema ];
    Wire_3d.generate_c ~outdir:tmpdir [ schema ];
    let base = String.capitalize_ascii schema.Everparse.name in
    let cmd =
      (* Compile with the exact strict-C11 flags the generated dune emits, so
         the e2e checks the validators under the same standard a downstream
         caller would. *)
      Fmt.str "cd %s && cc %s -o test_bin test.c %s.c %s_Fields.c 2>&1%s" tmpdir
        Wire_3d.strict_cc_flags base base
        (if run then " && ./test_bin" else "")
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

let compile_and_run = compile_generated
let compile_only = compile_generated ~run:false

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
type ep_case_v = [ `U16 of Wire.UInt16.t | `Default of Wire.UInt8.t ]

let e2e_casetype_codec =
  let open Wire in
  let body : ep_case_v typ =
    casetype "CtBody" uint8
      [
        case ~index:(UInt8.v 1) uint16
          ~inject:(fun v -> `U16 v)
          ~project:(function `U16 v -> Some v | _ -> None);
        default uint8
          ~inject:(fun _tag v -> `Default v)
          ~project:(function `Default v -> Some (UInt8.v 0xFF, v) | _ -> None);
      ]
  in
  let f = Field.v "msg" body in
  Codec.v "Ctt" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* String-tagged casetype: the 3D projection rewrites the casetype field
   into two adjacent byte spans (tag and body) so dispatch happens in
   caller code, mirroring OpenSSH's two-step pattern. No 3D-side extern,
   scratch slot or runtime helper -- just two SetBytes setters. *)
type ssh_auth = [ `Publickey of Wire.UInt8.t | `Other of Wire.UInt8.t ]

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
type channel_confirm = Session of session | Tcpip of Wire.UInt16.t

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
        case ~index:(UInt8.v 1) (codec session_codec)
          ~inject:(fun s -> Session s)
          ~project:(function Session s -> Some s | _ -> None);
        case ~index:(UInt8.v 2) uint16be
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
      Codec.( $ ) f_len (fun e -> UInt8.v (String.length e.name));
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
          UInt8.v
            (List.fold_left (fun a e -> a + 1 + String.length e.name) 0 xs));
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
      Codec.( $ ) f_len (fun d -> UInt8.v (String.length d));
      Codec.( $ ) f_data (fun d -> d);
    ]

let e2e_repeat_casetype_codec =
  let open Wire in
  let opt : dhcp_opt typ =
    casetype "DhcpOpt" uint8
      [
        case ~index:(UInt8.v 0) empty
          ~inject:(fun () -> Pad)
          ~project:(function Pad -> Some () | _ -> None);
        case ~index:(UInt8.v 255) empty
          ~inject:(fun () -> End)
          ~project:(function End -> Some () | _ -> None);
        case ~index:(UInt8.v 53)
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
      Codec.( $ ) f_total (fun xs ->
          UInt8.v (List.fold_left (fun a o -> a + size o) 0 xs));
      Codec.( $ ) f_opts (fun xs -> xs);
    ]

(* A [zeroterm] field extracts only as a one-field struct (EverParse #321). *)
let e2e_zeroterm_codec =
  let open Wire in
  let f_name = Field.v "name" zeroterm in
  Codec.v "ZtRec" Fun.id Codec.[ f_name $ Fun.id ]

(* The bounded form extracts beside other fields. *)
let e2e_zeroterm_at_most_codec =
  let open Wire in
  let f_tag = Field.v "tag" (zeroterm_at_most ~size:(int 8)) in
  let f_n = Field.v "n" uint8 in
  Codec.v "ZtAtMostRec"
    (fun tag n -> (tag, n))
    [ Codec.( $ ) f_tag fst; Codec.( $ ) f_n snd ]

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
  (* The bounded terminator is a value constraint, so arbitrary random bytes
     are not all valid. Compiling still exercises the extraction path. *)
  compile_only ~name:"ZtAtMostRec" e2e_zeroterm_at_most_codec;
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

(* [has_3d_exe] is the gate every EverParse-dependent case reads, so it is
   checked against the search it documents -- PATH, then the
   [~/.local/everparse/bin] fallback -- resolved here independently of
   [Wire_3d]'s own lookup. *)
let test_has_3d_exe () =
  let on_path = Sys.command "command -v 3d.exe > /dev/null 2>&1" = 0 in
  let fallback =
    Sys.file_exists
      (Filename.concat (Sys.getenv "HOME") ".local/everparse/bin/3d.exe")
  in
  Alcotest.(check bool)
    "has_3d_exe agrees with PATH plus the ~/.local/everparse fallback"
    (on_path || fallback) (Wire_3d.has_3d_exe ())

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
  let dune_inc = read_file (Filename.concat tmpdir "dune.inc") in
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
    (contains_exact "mode fallback");
  Alcotest.(check bool)
    "runtest regenerates every .3d" true
    (contains_exact "targets Rpmsg_endpoint_info.3d.gen");
  Alcotest.(check bool)
    "runtest diffs every committed .3d" true
    (contains_exact "(diff Rpmsg_endpoint_info.3d Rpmsg_endpoint_info.3d.gen)");
  Alcotest.(check bool)
    "runtest regenerates dune.inc" true
    (contains_exact "targets dune.inc.gen");
  Alcotest.(check bool)
    "runtest diffs the committed dune.inc" true
    (contains_exact "(diff dune.inc dune.inc.gen)");
  Alcotest.(check bool)
    "C regeneration writes provenance" true
    (contains_exact "Rpmsg_endpoint_info.provenance");
  Alcotest.(check bool)
    "runtest checks provenance" true
    (contains_exact "%{exe:gen.exe} provenance-check");
  Alcotest.(check bool)
    "stale provenance is not promotable" false
    (contains_exact "Rpmsg_endpoint_info.provenance.gen")

let suite =
  ( "wire_3d",
    [
      Alcotest.test_case "everparse_name" `Quick test_everparse_name;
      Alcotest.test_case "pascal_case" `Quick test_pascal_case;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "schema_of_struct" `Quick test_schema_of_struct;
      Alcotest.test_case "ensure_dir" `Quick test_ensure_dir;
      Alcotest.test_case "generate_c (needs 3d.exe)" `Quick test_generate_c;
      Alcotest.test_case "freestanding endianness header (needs 3d.exe)" `Quick
        test_endianness_freestanding;
      Alcotest.test_case "generated-name collision" `Quick test_name_collision;
      Alcotest.test_case "output-name validation" `Quick
        test_output_name_validation;
      Alcotest.test_case "check_provenance" `Quick test_check_provenance;
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
      Alcotest.test_case "doc differential nested regions (needs 3d.exe)" `Quick
        test_doc_differential_nested_regions;
      Alcotest.test_case "doc differential region/cardinality (needs 3d.exe)"
        `Quick test_doc_differential_region_cardinality;
      Alcotest.test_case "doc differential Msb_first sub-codec (needs 3d.exe)"
        `Quick test_doc_differential_msb_sub_codec;
      Alcotest.test_case "doc differential signed (needs 3d.exe)" `Quick
        test_doc_differential_signed;
      Alcotest.test_case "doc differential union (needs 3d.exe)" `Quick
        test_doc_differential_union;
      Alcotest.test_case "doc differential large payload (needs 3d.exe)" `Quick
        test_doc_differential_large_payload;
      Alcotest.test_case "doc differential caps name (needs 3d.exe)" `Quick
        test_doc_differential_caps_name;
      Alcotest.test_case "doc differential trailing bytes (needs 3d.exe)" `Quick
        test_doc_differential_trailing_bytes;
      Alcotest.test_case "doc differential hex decode (needs 3d.exe)" `Quick
        test_doc_differential_hex;
      Alcotest.test_case "uses_wire_ctx" `Quick test_uses_wire_ctx;
      Alcotest.test_case "has_3d_exe" `Quick test_has_3d_exe;
      Alcotest.test_case "main exists" `Quick test_main_exists;
      Alcotest.test_case "projection: wire_size matches Codec" `Quick
        test_projection_sizes;
      Alcotest.test_case "projection: filenames match EverParse output" `Quick
        test_projection_filenames;
      Alcotest.test_case "wrapper hardening names the status var" `Quick
        test_wrapper_hardening_names_the_status_var;
      Alcotest.test_case "wrapper hardening is idempotent" `Quick
        test_wrapper_hardening_is_idempotent;
      Alcotest.test_case "wrapper hardening refuses an unknown shape" `Quick
        test_wrapper_hardening_refuses_unknown_shape;
      Alcotest.test_case "e2e: compile + run across naming conventions" `Slow
        test_e2e_compile_run;
    ] )
