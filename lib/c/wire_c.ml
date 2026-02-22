(** Generate verified C libraries from Wire codecs via EverParse. *)

type schema = { name : string; module_ : Wire.module_; wire_size : int }

let schema ~name ~module_ ~wire_size = { name; module_; wire_size }

let generate_3d_files ~outdir schemas =
  List.iter
    (fun s ->
      Wire.to_3d_file (Filename.concat outdir (s.name ^ ".3d")) s.module_)
    schemas

let run_everparse ~outdir schemas =
  List.iter
    (fun s ->
      let f = s.name ^ ".3d" in
      let cmd = Fmt.str "cd %s && 3d.exe --batch %s" outdir f in
      let ret = Sys.command cmd in
      if ret <> 0 then
        failwith (Fmt.str "EverParse failed on %s with code %d" f ret))
    schemas

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
  | None -> failwith (Fmt.str "Could not find Validate function in %s" header)

let generate_test ~outdir schemas =
  let oc = open_out (Filename.concat outdir "test.c") in
  let pr fmt = Printf.fprintf oc fmt in
  pr "#include <stdio.h>\n";
  pr "#include <stdlib.h>\n";
  pr "#include <stdint.h>\n";
  pr "#include <string.h>\n";
  pr "#include \"EverParse.h\"\n\n";
  pr "static void noop_error_handler(\n";
  pr "  const char *t, const char *f, const char *r,\n";
  pr "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {\n";
  pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
  pr "}\n\n";
  List.iter
    (fun s ->
      pr "#include \"%s.h\"\n" s.name;
      pr "#include \"%s.c\"\n" s.name;
      pr
        "void %sEverParseError(const char *s, const char *f, const char *r) { \
         (void)s; (void)f; (void)r; }\n\n"
        s.name)
    schemas;
  pr "static int test_validate(const char *name,\n";
  pr
    "  uint64_t (*validate)(uint8_t *, void (*)(const char *, const char *, \
     const char *, uint64_t, uint8_t *, EVERPARSE_INPUT_BUFFER, uint64_t), \
     uint8_t *, uint32_t, uint64_t),\n";
  pr "  uint32_t wire_size) {\n";
  pr "  int pass = 0, fail = 0;\n";
  pr "  uint8_t *buf = calloc(wire_size, 1);\n";
  pr "  if (!buf) return 1;\n\n";
  pr "  /* zero-filled buffer should validate */\n";
  pr "  uint64_t r = validate(NULL, noop_error_handler, buf, wire_size, 0);\n";
  pr "  if (EverParseIsSuccess(r)) pass++; else fail++;\n\n";
  pr "  /* truncated buffer should fail */\n";
  pr "  if (wire_size > 0) {\n";
  pr "    r = validate(NULL, noop_error_handler, buf, wire_size - 1, 0);\n";
  pr "    if (!EverParseIsSuccess(r)) pass++; else fail++;\n";
  pr "  }\n\n";
  pr "  /* random buffers: ensure no crash */\n";
  pr "  srand(42);\n";
  pr "  for (int i = 0; i < 1000; i++) {\n";
  pr "    for (uint32_t j = 0; j < wire_size; j++)\n";
  pr "      buf[j] = (uint8_t)(rand() & 0xff);\n";
  pr "    r = validate(NULL, noop_error_handler, buf, wire_size, 0);\n";
  pr "    (void)r;\n";
  pr "    pass++;\n";
  pr "  }\n\n";
  pr "  free(buf);\n";
  pr "  printf(\"%%s: %%d passed, %%d failed\\n\", name, pass, fail);\n";
  pr "  return fail;\n";
  pr "}\n\n";
  pr "int main(void) {\n";
  pr "  int failures = 0;\n";
  List.iter
    (fun s ->
      let validate_fn = extract_validate_fn ~outdir s.name in
      pr "  failures += test_validate(\"%s\", %s, %d);\n" s.name validate_fn
        s.wire_size)
    schemas;
  pr "  if (failures == 0)\n";
  pr "    printf(\"All tests passed.\\n\");\n";
  pr "  else\n";
  pr "    printf(\"%%d test(s) failed.\\n\", failures);\n";
  pr "  return failures;\n";
  pr "}\n";
  close_out oc

let generate ~outdir schemas =
  (try Unix.mkdir outdir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  generate_3d_files ~outdir schemas;
  run_everparse ~outdir schemas;
  generate_test ~outdir schemas;
  (* Clean up intermediate .3d files *)
  List.iter
    (fun s ->
      let f = Filename.concat outdir (s.name ^ ".3d") in
      try Unix.unlink f with Unix.Unix_error _ -> ())
    schemas
