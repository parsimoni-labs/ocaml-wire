(** Generate verified C libraries from Wire codecs via EverParse. *)

open Wire.Everparse

let is_upper c = Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c

(* Apply EverParse's normalization to a single identifier segment (no
   underscores): if the segment begins with two or more uppercase letters,
   lowercase the whole segment and capitalize the first letter ([SSID ->
   Ssid], [TMFrame -> Tmframe]); otherwise leave it alone. *)
let normalize_segment seg =
  let len = String.length seg in
  let rec count_upper i =
    if i < len && is_upper seg.[i] then count_upper (i + 1) else i
  in
  if len > 0 && count_upper 0 >= 2 then
    String.init len (fun i ->
        let c = Char.lowercase_ascii seg.[i] in
        if i = 0 then Char.uppercase_ascii c else c)
  else seg

(* EverParse strips underscores when generating C identifiers and
   CamelCases each segment. [EP_Header -> EpHeader],
   [MC_Status_Reply -> McStatusReply], [SSID -> Ssid]. *)
let everparse_name name =
  String.split_on_char '_' name
  |> List.map normalize_segment |> String.concat ""

(* EverParse derives the C output filename from the [.3d] filename, which
   [Wire.Everparse.filename] already writes with [String.capitalize_ascii].
   All filenames wire.3d emits or references must go through the same
   capitalization so dune targets match the files EverParse actually
   produces. Identifiers inside C code -- the [<Name>Set*] setters and the
   typed struct name -- use [everparse_name], which also strips underscores
   and CamelCases segments ([rpmsg_endpoint_info] -> [RpmsgEndpointInfo]).
   Keep the two concerns separate: [file_base] for filenames, [c_ident] for
   C identifiers. *)
let file_base s = String.capitalize_ascii s.name
let c_ident s = everparse_name s.name

(* EverParse normalizes extern callback names in ways that are awkward to
   mirror exactly (runs of uppercase after a digit get lowercased, trailing
   uppercase runs get lowercased, ...). Rather than re-implement EverParse's
   rule and drift from it over time, we read the normalized names straight
   out of the [_ExternalAPI.h] file EverParse has just generated. *)
let read_extern_names ~outdir s =
  let path = Filename.concat outdir (file_base s ^ "_ExternalAPI.h") in
  let ic = open_in path in
  let names = ref [] in
  (try
     while true do
       let line = input_line ic in
       match
         ( String.index_opt line '(',
           String.index_opt line ' ',
           String.length line )
       with
       | Some lp, _, _ when String.length line >= 11 ->
           let prefix = "extern void " in
           let plen = String.length prefix in
           if
             String.length line > plen
             && String.sub line 0 plen = prefix
             && lp > plen
           then
             let name = String.sub line plen (lp - plen) in
             names := name :: !names
       | _ -> ()
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !names

(* EverParse's top-level validator function follows its own normalization
   rule (different from extern callbacks: it preserves underscores when the
   name doesn't start with 2+ uppercase, strips them when it does). Rather
   than duplicate EverParse's logic, extract the actual name from the
   generated [<Name>.h]: [uint64_t <Name>Validate<Name>(...)]. *)
let read_validate_name ~outdir s =
  let path = Filename.concat outdir (file_base s ^ ".h") in
  let ic = open_in path in
  let found = ref None in
  (try
     while !found = None do
       let line = input_line ic in
       let line = String.trim line in
       let needle = "Validate" in
       match String.index_opt line 'V' with
       | Some i
         when i > 0
              && String.length line >= i + String.length needle
              && String.sub line i (String.length needle) = needle ->
           (* the prefix before "Validate" is the base name *)
           found := Some (String.sub line 0 i)
       | _ -> ()
     done
   with End_of_file -> ());
  close_in ic;
  match !found with
  | Some n -> n
  | None -> Fmt.failwith "could not find Validate function name in %s" path

let write_3d = Wire.Everparse.write_3d

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let oc = open_out_bin dst in
  output_bytes oc buf;
  close_out oc

let locate_3d_exe () =
  let ic = Unix.open_process_in "command -v 3d.exe 2>/dev/null" in
  let path = try Some (input_line ic) with End_of_file -> None in
  ignore (Unix.close_process_in ic);
  match path with
  | Some p -> Some p
  | None ->
      let local =
        Filename.concat (Sys.getenv "HOME") ".local/everparse/bin/3d.exe"
      in
      if Sys.file_exists local then Some local else None

let everparse_dir () =
  match locate_3d_exe () with
  | Some exe -> Filename.dirname exe |> Filename.dirname
  | None -> failwith "3d.exe not found"

let copy_everparse_endianness ~outdir =
  let dst = Filename.concat outdir "EverParseEndianness.h" in
  if not (Sys.file_exists dst) then begin
    let ep_dir = everparse_dir () in
    let src = Filename.concat ep_dir "src/3d/EverParseEndianness.h" in
    if Sys.file_exists src then copy_file ~src ~dst
    else Fmt.failwith "Cannot find EverParseEndianness.h at %s" src
  end

let has_3d_exe () = locate_3d_exe () <> None

(* The [_ExternalTypedefs.h] header seen by the EverParse-generated validator
   and wrapper. The default shipped by wire.3d is a forward declaration that
   ties WIRECTX to the matching [<Name>_Fields] plug struct. Users who want
   their own plug (e.g. {!Wire_stubs} for OCaml FFI) overwrite this file with
   a different WIRECTX definition; they must then also omit the default
   [<Name>_Fields.c] from their link. *)
let write_external_typedefs ~outdir schemas =
  List.iter
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then begin
        let path =
          Filename.concat outdir (file_base s ^ "_ExternalTypedefs.h")
        in
        let oc = open_out path in
        Fmt.pf
          (Format.formatter_of_out_channel oc)
          "#ifndef WIRECTX_DEFINED@\n\
           #define WIRECTX_DEFINED@\n\
           typedef struct %sFields WIRECTX;@\n\
           #endif@\n"
          (c_ident s);
        close_out oc
      end)
    schemas

(* Typed-struct plug: one C struct per schema with one member per named field,
   plus a [WireSet*] implementation that switches on idx to populate it. *)
let write_fields_header ~outdir s =
  let fields = Wire.Everparse.plug_fields s in
  let base = file_base s in
  let ident = c_ident s in
  let path = Filename.concat outdir (base ^ "_Fields.h") in
  let oc = open_out path in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  let guard =
    String.uppercase_ascii ident ^ "_FIELDS_H" |> fun g ->
    String.map (fun c -> if c = '-' then '_' else c) g
  in
  let prefix =
    String.uppercase_ascii ident |> fun p ->
    String.map (fun c -> if c = '-' then '_' else c) p
  in
  pr "#ifndef %s@\n" guard;
  pr "#define %s@\n" guard;
  pr "#include <stdint.h>@\n@\n";
  pr "/* Field indices -- use with the schema's WireSet* callbacks in a@\n";
  pr "   custom [WIRECTX] if you only want to capture a subset. */@\n";
  List.iter
    (fun f ->
      pr "#define %s_IDX_%s %d@\n" prefix
        (String.uppercase_ascii f.Wire.Everparse.pf_name)
        f.pf_idx)
    fields;
  if fields <> [] then pr "@\n";
  pr "/* Default plug: one typed member per named field. Pass a pointer to@\n";
  pr "   [%sFields] as [WIRECTX *] when you want every field populated. */@\n"
    ident;
  pr "typedef struct %sFields {@\n" ident;
  List.iter
    (fun f -> pr "  %s %s;@\n" f.Wire.Everparse.pf_c_type f.pf_name)
    fields;
  if fields = [] then pr "  int _unused;@\n";
  pr "} %sFields;@\n" ident;
  pr "#endif@\n";
  Format.pp_print_flush ppf ();
  close_out oc

let write_fields_impl ~outdir s =
  let fields = Wire.Everparse.plug_fields s in
  let setters = Wire.Everparse.plug_setters s in
  let base = file_base s in
  let ident = c_ident s in
  (* EverParse renames some setter symbols when emitting [.c] (for example,
     uppercase runs after a digit are lowercased). Read the actual symbol
     names from the just-generated [_ExternalAPI.h] rather than re-deriving
     them. Order matches the declaration order of the extern functions,
     which matches [plug_setters]. *)
  let physical_names = read_extern_names ~outdir s in
  let path = Filename.concat outdir (base ^ "_Fields.c") in
  let oc = open_out path in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "#include <stdint.h>@\n";
  pr "#include \"%s_Fields.h\"@\n" base;
  pr "#include \"%s_ExternalTypedefs.h\"@\n" base;
  pr "#include \"%s_ExternalAPI.h\"@\n@\n" base;
  (* Cast [WIRECTX *] to the schema's concrete struct type. In a translation
     unit that includes multiple schemas' [_Fields.c] files, only the first
     [_ExternalTypedefs.h] defines [WIRECTX]; subsequent headers are skipped
     by the include guard. The explicit cast to [<Name>Fields *] makes each
     setter work regardless of which schema's typedef won. *)
  List.iter2
    (fun (logical, val_c_type) physical ->
      pr "void %s(WIRECTX *ctx, uint32_t idx, %s v) {@\n" physical val_c_type;
      pr "  %sFields *f = (%sFields *) ctx;@\n" ident ident;
      pr "  switch (idx) {@\n";
      List.iter
        (fun f ->
          if String.equal f.Wire.Everparse.pf_setter logical then
            pr "    case %d: f->%s = (%s) v; break;@\n" f.pf_idx f.pf_name
              f.pf_c_type)
        fields;
      pr "    default: (void) f; (void) v; break;@\n";
      pr "  }@\n";
      pr "}@\n@\n")
    setters physical_names;
  Format.pp_print_flush ppf ();
  close_out oc

let write_fields ~outdir schemas =
  List.iter
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then begin
        write_fields_header ~outdir s;
        write_fields_impl ~outdir s
      end)
    schemas

(* Files shipped with a schema whose validator depends on the WireCtx contract:
   the forward-decl header, the EverParse-emitted API / wrapper, and the
   default plug pair. Every file in this list is installed and accounted for
   in the dune rules; wrapper artefacts are only needed at install time,
   while [_Fields.{c,h}] also link into the runtest. *)
let wire_ctx_files schemas =
  List.concat_map
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then
        let base = file_base s in
        [
          base ^ "_ExternalTypedefs.h";
          base ^ "_ExternalAPI.h";
          base ^ "Wrapper.c";
          base ^ "Wrapper.h";
          base ^ "_Fields.h";
          base ^ "_Fields.c";
        ]
      else [])
    schemas

let fields_c_files schemas =
  List.filter_map
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then Some (file_base s ^ "_Fields.c")
      else None)
    schemas

let run_everparse ?(quiet = true) ~outdir schemas =
  let exe =
    match locate_3d_exe () with
    | Some e -> e
    | None -> failwith "3d.exe not found in PATH or ~/.local/everparse/bin/"
  in
  List.iter
    (fun s ->
      let f = Wire.Everparse.filename s in
      let redirect = if quiet then " > /dev/null 2>&1" else "" in
      let cmd = Fmt.str "cd %s && %s --batch %s%s" outdir exe f redirect in
      let ret = Sys.command cmd in
      if ret <> 0 then Fmt.failwith "EverParse failed on %s with code %d" f ret)
    schemas;
  copy_everparse_endianness ~outdir

let emit_sanity_check ppf ~name ~ep ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  (* Sanity: the OCaml codec's wire_size must match what the EverParse
     validator consumes. A mismatch means the .3d projection of the codec
     packs to a different size than the codec declares -- almost always a
     bug in the codec's bitfield declarations. Later checks are meaningless
     if this fails, so abort the whole test binary with a clear message. *)
  pr "    r = %sValidate%s(%sNULL, counting_error_handler, buf, %d, 0);\n" ep ep
    ctx_arg wire_size;
  pr "    if (!EverParseIsSuccess(r) || r != %d) {\n" wire_size;
  pr "      fprintf(stderr,\n";
  pr "        \"FATAL: %s wire_size mismatch -- codec declared %d bytes, \"\n"
    name wire_size;
  pr "        \"EverParse validator returned %%llu. Fix the OCaml codec's \"\n";
  pr "        \"wire_size or the .3d projection.\\n\",\n";
  pr "        (unsigned long long) r);\n";
  pr "      return 2;\n";
  pr "    }\n"

let emit_truncation_checks ppf ~ep ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  pr "    r = %sValidate%s(%sNULL, counting_error_handler, buf, %d, 0);\n" ep ep
    ctx_arg (wire_size * 2);
  pr "    CHECK(\"larger buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position is %d not %d\", r == %d);\n" wire_size
    (wire_size * 2) wire_size;
  pr "\n";
  pr "    for (uint64_t len = 0; len < %d; len++) {\n" wire_size;
  pr "      error_count = 0;\n";
  pr "      r = %sValidate%s(%sNULL, counting_error_handler, buf, len, 0);\n" ep
    ep ctx_arg;
  pr "      CHECK(\"truncated to len fails\", EverParseIsError(r));\n";
  pr "    }\n";
  pr "\n";
  pr "    r = %sValidate%s(%sNULL, counting_error_handler, buf, 0, 0);\n" ep ep
    ctx_arg;
  pr "    CHECK(\"empty input fails\", EverParseIsError(r));\n"

let emit_random_checks ppf ~ep ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  pr "    srand(42);\n";
  pr "    for (int i = 0; i < 1000; i++) {\n";
  pr "      for (int j = 0; j < %d; j++)\n" wire_size;
  pr "        buf[j] = (uint8_t)(rand() & 0xff);\n";
  pr "      r = %sValidate%s(%sNULL, counting_error_handler, buf, %d, 0);\n" ep
    ep ctx_arg wire_size;
  pr "      CHECK(\"random buffer validates\", EverParseIsSuccess(r));\n";
  pr "      CHECK(\"random position correct\", r == %d);\n" wire_size;
  pr "    }\n"

let emit_schema_test ?outdir ppf s wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  (* Read the validator name straight out of EverParse's generated [.h]
     -- the one authoritative source. EverParse applies its own naming
     rules (different for the top-level Validate function vs. the extern
     callbacks); any attempt to re-implement them here has drifted before. *)
  let ep =
    match outdir with
    | Some dir -> read_validate_name ~outdir:dir s
    | None -> file_base s
  in
  let lower = String.lowercase_ascii s.name in
  let uses_ctx = Wire.Everparse.uses_wire_ctx s in
  let ctx_arg = if uses_ctx then "(WIRECTX *) &ctx, " else "" in
  pr "\n  /* %s (%d bytes) */\n" s.name wire_size;
  pr "  {\n";
  pr "    int pass = 0, fail = 0;\n";
  pr "    uint8_t buf[%d];\n" wire_size;
  pr "    uint64_t r;\n";
  if uses_ctx then pr "    %sFields ctx = {0};\n" (c_ident s);
  pr "\n";
  pr "    memset(buf, 0, %d);\n" wire_size;
  emit_sanity_check ppf ~name:s.name ~ep ~ctx_arg wire_size;
  pr "    CHECK(\"zero buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position advanced to %d\", r == %d);\n" wire_size wire_size;
  pr "\n";
  emit_truncation_checks ppf ~ep ~ctx_arg wire_size;
  pr "\n";
  emit_random_checks ppf ~ep ~ctx_arg wire_size;
  pr "\n";
  if uses_ctx then pr "    (void) ctx;\n";
  pr "    printf(\"%s: %%d passed, %%d failed\\n\", pass, fail);\n" lower;
  pr "    failures += fail;\n";
  pr "  }\n"

let generate_test ~outdir schemas =
  let oc = open_out (Filename.concat outdir "test.c") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "#include <stdio.h>\n";
  pr "#include <stdlib.h>\n";
  pr "#include <stdint.h>\n";
  pr "#include <string.h>\n";
  pr "#include \"EverParse.h\"\n";
  let fixed_schemas =
    List.filter_map
      (fun s -> Option.map (fun ws -> (s, ws)) s.wire_size)
      schemas
  in
  List.iter
    (fun (s, _) ->
      let base = file_base s in
      pr "#include \"%s.h\"\n" base;
      if Wire.Everparse.uses_wire_ctx s then
        pr "#include \"%s_Fields.h\"\n" base)
    fixed_schemas;
  pr "\nstatic int error_count;\n\n";
  pr "static void counting_error_handler(\n";
  pr "  EVERPARSE_STRING t, EVERPARSE_STRING f, EVERPARSE_STRING r,\n";
  pr "  uint64_t c, uint8_t *ctx, uint8_t *i, uint64_t p) {\n";
  pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
  pr "  error_count++;\n";
  pr "}\n\n";
  pr "#define CHECK(msg, cond) do { \\\n";
  pr "  if (cond) { pass++; } \\\n";
  pr "  else { fail++; fprintf(stderr, \"  FAIL: %%s\\n\", msg); } \\\n";
  pr "} while(0)\n\n";
  pr "int main(void) {\n";
  pr "  int failures = 0;\n";
  List.iter (fun (s, ws) -> emit_schema_test ~outdir ppf s ws) fixed_schemas;
  pr "\n  if (failures == 0)\n";
  pr "    printf(\"All tests passed.\\n\");\n";
  pr "  else\n";
  pr "    printf(\"%%d test(s) failed.\\n\", failures);\n";
  pr "  return failures ? 1 : 0;\n";
  pr "}\n";
  Format.pp_print_flush ppf ();
  close_out oc

let ensure_dir outdir =
  try Unix.mkdir outdir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let generate_3d ~outdir schemas =
  ensure_dir outdir;
  write_3d ~outdir schemas

let generate_c ?(quiet = true) ~outdir schemas =
  ensure_dir outdir;
  if has_3d_exe () then begin
    run_everparse ~quiet ~outdir schemas;
    write_external_typedefs ~outdir schemas;
    write_fields ~outdir schemas;
    generate_test ~outdir schemas
  end
  else
    failwith
      "3d.exe not found in PATH. Install EverParse to regenerate C files."

let run ?(quiet = true) ~outdir schemas =
  generate_3d ~outdir schemas;
  generate_c ~quiet ~outdir schemas

let strict_cc_flags =
  "-std=c99 -Wall -Wextra -Werror -Wpedantic -Wstrict-prototypes \
   -Wmissing-prototypes -Wshadow -Wcast-qual"

let emit_gen_rules ppf three_d_files c_files ctx_files =
  Fmt.pf ppf
    "(rule\n\
    \ (alias gen)\n\
    \ (mode promote)\n\
    \ (targets %s)\n\
    \ (deps gen.exe)\n\
    \ (action\n\
    \  (run ./gen.exe 3d)))\n\n\
     (rule\n\
    \ (alias gen)\n\
    \ (mode fallback)\n\
    \ (targets EverParse.h EverParseEndianness.h %s test.c)\n\
    \ (deps gen.exe %s)\n\
    \ (action\n\
    \  (run ./gen.exe c)))\n\n"
    (String.concat " " three_d_files)
    (String.concat " " (c_files @ ctx_files))
    (String.concat " " three_d_files)

let emit_runtest_rule ppf ~test_bin ~all_deps ~c_srcs =
  Fmt.pf ppf
    "(rule\n\
    \ (alias runtest)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (system\n\
    \   \"cc %s -o %s test.c %s && ./%s\")))\n\n"
    (String.concat " " all_deps)
    strict_cc_flags test_bin (String.concat " " c_srcs) test_bin

let emit_install_stanza ppf ~package ~three_d_files ~c_files ~ctx_files =
  let pr fmt = Fmt.pf ppf fmt in
  pr "(install\n (package %s)\n (section lib)\n (files\n" package;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) three_d_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) c_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) ctx_files;
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n"

let generate_dune ~outdir ~package schemas =
  let oc = open_out (Filename.concat outdir "dune.inc") in
  let ppf = Format.formatter_of_out_channel oc in
  let names = List.map file_base schemas in
  let c_files = List.concat_map (fun n -> [ n ^ ".h"; n ^ ".c" ]) names in
  let ctx_files = wire_ctx_files schemas in
  let fields_srcs = fields_c_files schemas in
  let three_d_files = List.map (fun n -> n ^ ".3d") names in
  let test_bin =
    "test_" ^ String.map (fun c -> if c = '-' then '_' else c) package
  in
  let all_deps =
    [ "test.c"; "EverParse.h"; "EverParseEndianness.h" ] @ c_files @ ctx_files
  in
  let c_srcs = List.map (fun n -> n ^ ".c") names @ fields_srcs in
  emit_gen_rules ppf three_d_files c_files ctx_files;
  emit_runtest_rule ppf ~test_bin ~all_deps ~c_srcs;
  emit_install_stanza ppf ~package ~three_d_files ~c_files ~ctx_files;
  Format.pp_print_flush ppf ();
  close_out oc

let main ~package schemas =
  match Array.to_list Sys.argv with
  | [ _; "3d" ] -> generate_3d ~outdir:"." schemas
  | [ _; "c" ] -> generate_c ~outdir:"." schemas
  | [ _; "dune" ] -> generate_dune ~outdir:"." ~package schemas
  | _ -> run ~outdir:"." schemas
