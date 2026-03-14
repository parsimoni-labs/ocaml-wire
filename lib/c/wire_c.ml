(** Generate verified C libraries from Wire codecs via EverParse. *)

type schema = { name : string; module_ : Wire.module_; wire_size : int }

let schema ~name ~module_ ~wire_size = { name; module_; wire_size }

let generate_3d_files ~outdir schemas =
  List.iter
    (fun s ->
      Wire.to_3d_file (Filename.concat outdir (s.name ^ ".3d")) s.module_)
    schemas

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let oc = open_out_bin dst in
  output_bytes oc buf;
  close_out oc

let everparse_dir () =
  let ic = Unix.open_process_in "which 3d.exe" in
  let path = input_line ic in
  ignore (Unix.close_process_in ic);
  Filename.dirname path |> Filename.dirname

let copy_everparse_endianness ~outdir =
  let dst = Filename.concat outdir "EverParseEndianness.h" in
  if not (Sys.file_exists dst) then begin
    let ep_dir = everparse_dir () in
    let src = Filename.concat ep_dir "src/3d/EverParseEndianness.h" in
    if Sys.file_exists src then copy_file ~src ~dst
    else Fmt.failwith "Cannot find EverParseEndianness.h at %s" src
  end

let has_3d_exe () = Sys.command "command -v 3d.exe > /dev/null 2>&1" = 0

let run_everparse ~outdir schemas =
  List.iter
    (fun s ->
      let f = s.name ^ ".3d" in
      let cmd = Fmt.str "cd %s && 3d.exe --batch %s" outdir f in
      let ret = Sys.command cmd in
      if ret <> 0 then Fmt.failwith "EverParse failed on %s with code %d" f ret)
    schemas;
  copy_everparse_endianness ~outdir

(** Extract the validate function name from an EverParse-generated header.

    EverParse normalizes C identifiers in a way that depends on consecutive
    uppercase runs (e.g., [SpaceOSFrame] becomes [SpaceOsframe]). Rather than
    replicating the algorithm, we read the generated header files. *)
let extract_validate_fn ~outdir name =
  let header = Filename.concat outdir (name ^ ".h") in
  let ic = open_in header in
  let result = ref None in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if String.length trimmed > 0 && String.contains trimmed '(' then begin
         let fn = String.sub trimmed 0 (String.index trimmed '(') in
         let fn = String.trim fn in
         let has_validate =
           let vlen = String.length "Validate" in
           let rec check i =
             if i + vlen > String.length fn then false
             else if String.sub fn i vlen = "Validate" then true
             else check (i + 1)
           in
           check 0
         in
         if has_validate && fn <> "" && fn.[0] <> '#' && fn.[0] <> '*' then
           result := Some fn
       end
     done
   with End_of_file -> ());
  close_in ic;
  match !result with
  | Some fn -> fn
  | None -> Fmt.failwith "Could not find Validate function in %s" header

let generate_test ~outdir schemas =
  let oc = open_out (Filename.concat outdir "test.c") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "#include <stdio.h>\n";
  pr "#include <stdlib.h>\n";
  pr "#include <stdint.h>\n";
  pr "#include <string.h>\n";
  pr "#include \"EverParse.h\"\n";
  List.iter (fun s -> pr "#include \"%s.h\"\n" s.name) schemas;
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
  List.iter
    (fun s ->
      let validate_fn = extract_validate_fn ~outdir s.name in
      let lower = String.lowercase_ascii s.name in
      pr "\n  /* %s (%d bytes) */\n" s.name s.wire_size;
      pr "  {\n";
      pr "    int pass = 0, fail = 0;\n";
      pr "    uint8_t buf[%d];\n" s.wire_size;
      pr "    uint64_t r;\n\n";
      (* Test 1: zero-filled buffer validates *)
      pr "    memset(buf, 0, %d);\n" s.wire_size;
      pr "    r = %s(NULL, counting_error_handler, buf, %d, 0);\n" validate_fn
        s.wire_size;
      pr "    CHECK(\"zero buffer validates\", EverParseIsSuccess(r));\n";
      pr "    CHECK(\"position advanced to %d\", r == %d);\n" s.wire_size
        s.wire_size;
      pr "\n";
      (* Test 2: exact size with non-zero start position *)
      pr "    r = %s(NULL, counting_error_handler, buf, %d, 0);\n" validate_fn
        (s.wire_size * 2);
      pr "    CHECK(\"larger buffer validates\", EverParseIsSuccess(r));\n";
      pr "    CHECK(\"position is %d not %d\", r == %d);\n" s.wire_size
        (s.wire_size * 2) s.wire_size;
      pr "\n";
      (* Test 3: truncated buffers fail *)
      pr "    for (uint64_t len = 0; len < %d; len++) {\n" s.wire_size;
      pr "      error_count = 0;\n";
      pr "      r = %s(NULL, counting_error_handler, buf, len, 0);\n"
        validate_fn;
      pr "      CHECK(\"truncated to len fails\", EverParseIsError(r));\n";
      pr "    }\n";
      pr "\n";
      (* Test 4: empty input returns error *)
      pr "    r = %s(NULL, counting_error_handler, buf, 0, 0);\n" validate_fn;
      pr "    CHECK(\"empty input fails\", EverParseIsError(r));\n";
      pr "\n";
      (* Test 5: fuzz — random buffers never crash *)
      pr "    srand(42);\n";
      pr "    for (int i = 0; i < 1000; i++) {\n";
      pr "      for (int j = 0; j < %d; j++)\n" s.wire_size;
      pr "        buf[j] = (uint8_t)(rand() & 0xff);\n";
      pr "      r = %s(NULL, counting_error_handler, buf, %d, 0);\n" validate_fn
        s.wire_size;
      pr "      CHECK(\"random buffer validates\", EverParseIsSuccess(r));\n";
      pr "      CHECK(\"random position correct\", r == %d);\n" s.wire_size;
      pr "    }\n";
      pr "\n";
      pr "    printf(\"%s: %%d passed, %%d failed\\n\", pass, fail);\n" lower;
      pr "    failures += fail;\n";
      pr "  }\n")
    schemas;
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
  generate_3d_files ~outdir schemas

let generate_c ~outdir schemas =
  ensure_dir outdir;
  if has_3d_exe () then begin
    run_everparse ~outdir schemas;
    generate_test ~outdir schemas
  end
  else
    failwith
      "3d.exe not found in PATH. Install EverParse to regenerate C files."

let generate ~outdir schemas =
  generate_3d ~outdir schemas;
  generate_c ~outdir schemas

let generate_dune ~outdir ~package schemas =
  let oc = open_out (Filename.concat outdir "dune.inc") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  let names = List.map (fun s -> s.name) schemas in
  let c_files = List.concat_map (fun n -> [ n ^ ".h"; n ^ ".c" ]) names in
  let three_d_files = List.map (fun n -> n ^ ".3d") names in
  let test_bin =
    "test_" ^ String.map (fun c -> if c = '-' then '_' else c) package
  in
  (* Rule: ml → .3d *)
  pr "(rule\n";
  pr " (alias gen)\n";
  pr " (mode promote)\n";
  pr " (targets %s)\n" (String.concat " " three_d_files);
  pr " (deps gen.exe)\n";
  pr " (action\n";
  pr "  (run ./gen.exe 3d)))\n\n";
  (* Rule: .3d → C (fallback: use existing promoted files unless deleted) *)
  pr "(rule\n";
  pr " (alias gen)\n";
  pr " (mode fallback)\n";
  pr " (targets EverParse.h EverParseEndianness.h %s test.c)\n"
    (String.concat " " c_files);
  pr " (deps gen.exe %s)\n" (String.concat " " three_d_files);
  pr " (action\n";
  pr "  (run ./gen.exe c)))\n\n";
  (* Rule: runtest *)
  let all_deps =
    [ "test.c"; "EverParse.h"; "EverParseEndianness.h" ] @ c_files
  in
  let c_srcs = List.map (fun n -> n ^ ".c") names in
  pr "(rule\n";
  pr " (alias runtest)\n";
  pr " (deps %s)\n" (String.concat " " all_deps);
  pr " (action\n";
  pr "  (system\n";
  pr "   \"cc -std=c11 -Wall -Wextra -Werror -o %s test.c %s && ./%s\")))\n\n"
    test_bin (String.concat " " c_srcs) test_bin;
  (* Install *)
  pr "(install\n";
  pr " (package %s)\n" package;
  pr " (section lib)\n";
  pr " (files\n";
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) three_d_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) c_files;
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n";
  Format.pp_print_flush ppf ();
  close_out oc

let main ~package schemas =
  match Array.to_list Sys.argv with
  | [ _; "3d" ] -> generate_3d ~outdir:"." schemas
  | [ _; "c" ] -> generate_c ~outdir:"." schemas
  | [ _; "dune" ] -> generate_dune ~outdir:"." ~package schemas
  | _ -> generate ~outdir:"." schemas
