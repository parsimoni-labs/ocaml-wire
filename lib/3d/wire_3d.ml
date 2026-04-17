(** Generate verified C libraries from Wire codecs via EverParse. *)

open Wire.Everparse

let everparse_name name =
  let is_upper c = Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c in
  let len = String.length name in
  let rec count_upper i =
    if i < len && is_upper name.[i] then count_upper (i + 1) else i
  in
  if len > 0 && count_upper 0 >= 2 then
    String.init len (fun i ->
        let c = Char.lowercase_ascii name.[i] in
        if i = 0 then Char.uppercase_ascii c else c)
  else name

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

let external_typedefs_content =
  "#ifndef WIRECTX_DEFINED\n\
   #define WIRECTX_DEFINED\n\
   #include <stdint.h>\n\
   typedef struct { int64_t *fields; } WIRECTX;\n\
   #endif\n"

let write_external_typedefs ~outdir schemas =
  List.iter
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then begin
        let path = Filename.concat outdir (s.name ^ "_ExternalTypedefs.h") in
        let oc = open_out path in
        output_string oc external_typedefs_content;
        close_out oc
      end)
    schemas

let external_typedefs_files schemas =
  List.filter_map
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then
        Some (s.name ^ "_ExternalTypedefs.h")
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

let emit_schema_test ppf s wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  let ep = everparse_name s.name in
  let lower = String.lowercase_ascii s.name in
  pr "\n  /* %s (%d bytes) */\n" s.name wire_size;
  pr "  {\n";
  pr "    int pass = 0, fail = 0;\n";
  pr "    uint8_t buf[%d];\n" wire_size;
  pr "    uint64_t r;\n\n";
  pr "    memset(buf, 0, %d);\n" wire_size;
  pr "    r = %sValidate%s(NULL, counting_error_handler, buf, %d, 0);\n" ep ep
    wire_size;
  pr "    CHECK(\"zero buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position advanced to %d\", r == %d);\n" wire_size wire_size;
  pr "\n";
  pr "    r = %sValidate%s(NULL, counting_error_handler, buf, %d, 0);\n" ep ep
    (wire_size * 2);
  pr "    CHECK(\"larger buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position is %d not %d\", r == %d);\n" wire_size
    (wire_size * 2) wire_size;
  pr "\n";
  pr "    for (uint64_t len = 0; len < %d; len++) {\n" wire_size;
  pr "      error_count = 0;\n";
  pr "      r = %sValidate%s(NULL, counting_error_handler, buf, len, 0);\n" ep
    ep;
  pr "      CHECK(\"truncated to len fails\", EverParseIsError(r));\n";
  pr "    }\n";
  pr "\n";
  pr "    r = %sValidate%s(NULL, counting_error_handler, buf, 0, 0);\n" ep ep;
  pr "    CHECK(\"empty input fails\", EverParseIsError(r));\n";
  pr "\n";
  pr "    srand(42);\n";
  pr "    for (int i = 0; i < 1000; i++) {\n";
  pr "      for (int j = 0; j < %d; j++)\n" wire_size;
  pr "        buf[j] = (uint8_t)(rand() & 0xff);\n";
  pr "      r = %sValidate%s(NULL, counting_error_handler, buf, %d, 0);\n" ep ep
    wire_size;
  pr "      CHECK(\"random buffer validates\", EverParseIsSuccess(r));\n";
  pr "      CHECK(\"random position correct\", r == %d);\n" wire_size;
  pr "    }\n";
  pr "\n";
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
  List.iter (fun (s, _) -> pr "#include \"%s.h\"\n" s.name) fixed_schemas;
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
  List.iter (fun (s, ws) -> emit_schema_test ppf s ws) fixed_schemas;
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
    generate_test ~outdir schemas
  end
  else
    failwith
      "3d.exe not found in PATH. Install EverParse to regenerate C files."

let run ?(quiet = true) ~outdir schemas =
  generate_3d ~outdir schemas;
  generate_c ~quiet ~outdir schemas

let generate_dune ~outdir ~package schemas =
  let oc = open_out (Filename.concat outdir "dune.inc") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  let names = List.map (fun s -> s.name) schemas in
  let c_files = List.concat_map (fun n -> [ n ^ ".h"; n ^ ".c" ]) names in
  let ext_files = external_typedefs_files schemas in
  let three_d_files = List.map (fun n -> n ^ ".3d") names in
  let test_bin =
    "test_" ^ String.map (fun c -> if c = '-' then '_' else c) package
  in
  pr "(rule\n";
  pr " (alias gen)\n";
  pr " (mode promote)\n";
  pr " (targets %s)\n" (String.concat " " three_d_files);
  pr " (deps gen.exe)\n";
  pr " (action\n";
  pr "  (run ./gen.exe 3d)))\n\n";
  pr "(rule\n";
  pr " (alias gen)\n";
  pr " (mode fallback)\n";
  pr " (targets EverParse.h EverParseEndianness.h %s test.c)\n"
    (String.concat " " (c_files @ ext_files));
  pr " (deps gen.exe %s)\n" (String.concat " " three_d_files);
  pr " (action\n";
  pr "  (run ./gen.exe c)))\n\n";
  let all_deps =
    [ "test.c"; "EverParse.h"; "EverParseEndianness.h" ] @ c_files @ ext_files
  in
  let c_srcs = List.map (fun n -> n ^ ".c") names in
  pr "(rule\n";
  pr " (alias runtest)\n";
  pr " (deps %s)\n" (String.concat " " all_deps);
  pr " (action\n";
  pr "  (system\n";
  pr "   \"cc -std=c11 -Wall -Wextra -Werror -o %s test.c %s && ./%s\")))\n\n"
    test_bin (String.concat " " c_srcs) test_bin;
  pr "(install\n";
  pr " (package %s)\n" package;
  pr " (section lib)\n";
  pr " (files\n";
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) three_d_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) c_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) ext_files;
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n";
  Format.pp_print_flush ppf ();
  close_out oc

let main ~package schemas =
  match Array.to_list Sys.argv with
  | [ _; "3d" ] -> generate_3d ~outdir:"." schemas
  | [ _; "c" ] -> generate_c ~outdir:"." schemas
  | [ _; "dune" ] -> generate_dune ~outdir:"." ~package schemas
  | _ -> run ~outdir:"." schemas
