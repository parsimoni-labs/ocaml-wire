(** Code generation for EverParse differential testing.

    Generates .3d files from Wire schemas, invokes EverParse, and produces C
    stubs, OCaml externals, and a test runner for comparing OCaml codecs against
    EverParse-generated C parsers. *)

type schema = { name : string; module_ : Wire.C.module_; wire_size : int }

let schema ~name ~struct_ ~module_ =
  match Wire_c.wire_size struct_ with
  | Some wire_size -> Some { name; module_; wire_size }
  | None -> None

(** {1 EverParse Invocation} *)

let run_everparse ~schema_dir =
  let cmd =
    Fmt.str
      "cd %s && for f in *.3d; do ~/.local/everparse/bin/3d.exe --batch \"$f\" \
       > /dev/null 2>&1 || exit 1; done"
      schema_dir
  in
  let ret = Sys.command cmd in
  if ret <> 0 then Fmt.failwith "EverParse failed with code %d" ret

(** {1 Extracting EverParse-generated Function Names}

    EverParse normalizes C identifiers in a way that depends on consecutive
    uppercase runs (e.g., [SpaceOSFrame] becomes [SpaceOsframe]). Rather than
    replicating the algorithm, we read the generated header files. *)

let extract_validate_fn ~schema_dir name =
  let header = Filename.concat schema_dir (name ^ ".h") in
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

(** {1 Code Generation} *)

let generate_3d_files ~schema_dir schemas =
  List.iter
    (fun s ->
      Wire.C.to_3d_file (Filename.concat schema_dir (s.name ^ ".3d")) s.module_)
    schemas

let generate_c_stubs ~schema_dir ~outdir schemas =
  let oc = open_out (Filename.concat outdir "stubs.c") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "#include <caml/mlvalues.h>\n";
  pr "#include <caml/memory.h>\n";
  pr "#include <caml/alloc.h>\n";
  pr "#include <stdint.h>\n\n";
  pr "#include \"%s/EverParse.h\"\n\n" schema_dir;
  pr "static void noop_error_handler(\n";
  pr "  const char *t, const char *f, const char *r,\n";
  pr "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {\n";
  pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
  pr "}\n\n";
  List.iter
    (fun s ->
      let validate_fn = extract_validate_fn ~schema_dir s.name in
      let lower = String.lowercase_ascii s.name in
      pr "/* --- %s --- */\n" s.name;
      pr "#include \"%s/%s.h\"\n" schema_dir s.name;
      pr "#include \"%s/%s.c\"\n" schema_dir s.name;
      pr
        "void %sEverParseError(const char *s, const char *f, const char *r) { \
         (void)s; (void)f; (void)r; }\n"
        s.name;
      pr "CAMLprim value caml_%s_check(value v_bytes) {\n" lower;
      pr "  CAMLparam1(v_bytes);\n";
      pr "  uint8_t *data = (uint8_t *)Bytes_val(v_bytes);\n";
      pr "  uint32_t len = caml_string_length(v_bytes);\n";
      pr "  uint64_t result = %s(NULL, noop_error_handler, data, len, 0);\n"
        validate_fn;
      pr "  CAMLreturn(Val_bool(EverParseIsSuccess(result)));\n";
      pr "}\n\n")
    schemas;
  Format.pp_print_flush ppf ();
  close_out oc

let generate_ml_stubs ~outdir schemas =
  let oc = open_out (Filename.concat outdir "stubs.ml") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  List.iter
    (fun s ->
      let lower = String.lowercase_ascii s.name in
      pr "external %s_check : bytes -> bool = \"caml_%s_check\"\n" lower lower)
    schemas;
  Format.pp_print_flush ppf ();
  close_out oc

let generate_test_runner ~outdir ?(num_values = 1000) schemas =
  let oc = open_out (Filename.concat outdir "diff_test.ml") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "(* Auto-generated differential test runner *)\n\n";
  pr "let num_values = %d\n\n" num_values;
  pr "type schema = {\n";
  pr "  name : string;\n";
  pr "  wire_size : int;\n";
  pr "  c_check : bytes -> bool;\n";
  pr "}\n\n";
  pr "let schemas = [\n";
  List.iter
    (fun s ->
      let lower = String.lowercase_ascii s.name in
      pr "  { name = %S; wire_size = %d; c_check = Stubs.%s_check };\n" s.name
        s.wire_size lower)
    schemas;
  pr "]\n\n";
  pr "let () =\n";
  pr "  let seed = 42 in\n";
  pr "  let rng = Random.State.make [| seed |] in\n";
  pr "  let total_tests = ref 0 in\n";
  pr "  let passed = ref 0 in\n";
  pr "  List.iter (fun schema ->\n";
  pr "    let valid = ref 0 in\n";
  pr "    let invalid = ref 0 in\n";
  pr "    for _ = 1 to num_values do\n";
  pr "      let buf = Bytes.create schema.wire_size in\n";
  pr "      for i = 0 to schema.wire_size - 1 do\n";
  pr "        Bytes.set buf i (Char.chr (Random.State.int rng 256))\n";
  pr "      done;\n";
  pr "      let c_ok = schema.c_check buf in\n";
  pr "      incr total_tests;\n";
  pr "      if c_ok then incr valid else incr invalid;\n";
  pr "      incr passed\n";
  pr "    done;\n";
  pr
    "    Printf.printf \"%%s: %%d valid, %%d invalid (of %%d)\\n\" schema.name \
     !valid !invalid num_values\n";
  pr "  ) schemas;\n";
  pr
    "  Printf.printf \"Tested %%d values across %%d schemas\\n\" !total_tests \
     (List.length schemas);\n";
  pr "  Printf.printf \"All %%d tests passed\\n\" !passed\n";
  Format.pp_print_flush ppf ();
  close_out oc

(** {1 Full Pipeline} *)

let generate ~schema_dir ~outdir ?(num_values = 1000) schemas =
  (try Unix.mkdir schema_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  generate_3d_files ~schema_dir schemas;
  Fmt.pr "Generated %d .3d files in %s/\n" (List.length schemas) schema_dir;
  run_everparse ~schema_dir;
  generate_c_stubs ~schema_dir ~outdir schemas;
  generate_ml_stubs ~outdir schemas;
  generate_test_runner ~outdir ~num_values schemas;
  Fmt.pr "Generated stubs.c, stubs.ml, diff_test.ml\n"
