(** Generate EverParse schemas and benchmark infrastructure.

    Usage: gen_stubs.exe <schema_dir>

    1. Generates .3d files with output pattern (extern callbacks) 2. Runs
    EverParse to produce C validators 3. Generates WireSet* implementations +
    parse stubs + timed C loops *)

let schema_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "schemas"

type entry = E : string * 'r Wire.Codec.t * Wire.C.Raw.struct_ -> entry

let entries =
  [
    E ("Minimal", Demo.minimal_codec, Demo.minimal_struct);
    E ("Bitfield8", Demo.bf8_codec, Demo.bf8_struct);
    E ("Bitfield16", Demo.bf16_codec, Demo.bf16_struct);
    E ("BoolFields", Demo.bool_fields_codec, Demo.bool_fields_struct);
    E ("Bitfield32", Demo.bf32_codec, Demo.bf32_struct);
    E ("AllInts", Demo.all_ints_codec, Demo.all_ints_struct);
    E ("LargeMixed", Demo.large_mixed_codec, Demo.large_mixed_struct);
    E ("Mapped", Demo.mapped_codec, Demo.mapped_struct);
    E ("CasesDemo", Demo.cases_demo_codec, Demo.cases_demo_struct);
    E ("EnumDemo", Demo.enum_demo_codec, Demo.enum_demo_struct);
    E ("Constrained", Demo.constrained_codec, Demo.constrained_struct);
    E ("CLCW", Space.clcw_codec, Space.clcw_struct);
    E ("SpacePacket", Space.packet_codec, Space.packet_struct);
    E ("TMFrame", Space.tm_frame_codec, Space.tm_frame_struct);
    E ("Ethernet", Net.ethernet_codec, Net.ethernet_struct);
    E ("IPv4", Net.ipv4_codec, Net.ipv4_struct);
    E ("TCP", Net.tcp_codec, Net.tcp_struct);
  ]

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let () =
  let structs = List.map (fun (E (_, _, s)) -> s) entries in
  let schemas =
    List.map (fun (E (_, codec, _)) -> Wire.C.schema codec) entries
  in

  (* 1. Generate .3d + ExternalTypedefs.h *)
  Wire_3d.generate_3d ~outdir:schema_dir schemas;
  List.iter
    (fun s ->
      let name = Wire.C.Raw.struct_name s in
      write_file
        (Filename.concat schema_dir (name ^ "_ExternalTypedefs.h"))
        (Wire_stubs.to_external_typedefs name))
    structs;

  (* 2. Run EverParse *)
  let quiet = Sys.getenv_opt "EVERPARSE_VERBOSE" = None in
  Wire_3d.run_everparse ~quiet ~outdir:schema_dir schemas;

  (* 3. Generate c_stubs.c: WireSet* + parse stubs + timed C loops *)
  let oc = open_out "c_stubs.c" in
  output_string oc (Wire_stubs.to_wire_setters ());
  output_string oc (Wire_stubs.to_c_stubs structs);

  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in

  pr "\n/* ── Timed C benchmark loops ── */\n\n";
  pr "#include <time.h>\n\n";
  pr "static inline int64_t now_ns(void) {\n";
  pr "  struct timespec ts;\n";
  pr "  clock_gettime(CLOCK_MONOTONIC, &ts);\n";
  pr "  return ts.tv_sec * 1000000000LL + ts.tv_nsec;\n";
  pr "}\n\n";
  pr "static void bench_err(const char *t, const char *f, const char *r,\n";
  pr "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {\n";
  pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
  pr "}\n\n";
  List.iter
    (fun s ->
      let name = Wire.C.Raw.struct_name s in
      let ep = Wire_3d.everparse_name name in
      let lower = String.lowercase_ascii name in
      let n_fields = List.length (Wire.C.Raw.field_names s) in
      pr "CAMLprim value ep_loop_%s(value v_buf, value v_off, value v_n) {\n"
        lower;
      pr "  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);\n";
      pr "  uint32_t len = caml_string_length(v_buf) - Int_val(v_off);\n";
      pr "  int count = Int_val(v_n);\n";
      pr "  volatile uint64_t result;\n";
      pr "  WIRECTX ctx;\n";
      pr "  ctx.v_record = caml_alloc(%d, 0);\n" n_fields;
      pr "  int64_t t0 = now_ns();\n";
      pr "  for (int i = 0; i < count; i++) {\n";
      pr "    result = %sValidate%s(&ctx, NULL, bench_err, buf, len, 0);\n" ep
        ep;
      pr "  }\n";
      pr "  (void)result;\n";
      pr "  int64_t t1 = now_ns();\n";
      pr "  return Val_int(t1 - t0);\n";
      pr "}\n\n")
    structs;

  Format.pp_print_flush ppf ();
  close_out oc;

  (* 4. Generate c_stubs.ml: parse externals + loop externals *)
  let oc = open_out "c_stubs.ml" in
  output_string oc (Wire_stubs.to_ml_stubs structs);

  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "(* Timed C benchmark loops *)\n\n";
  List.iter
    (fun s ->
      let lower = String.lowercase_ascii (Wire.C.Raw.struct_name s) in
      pr "external %s_loop : bytes -> int -> int -> int = \"ep_loop_%s\"\n\n"
        lower lower)
    structs;
  Format.pp_print_flush ppf ();
  close_out oc;

  Fmt.pr "Generated %d schemas in %s/@." (List.length structs) schema_dir;
  Fmt.pr "Generated c_stubs.c, c_stubs.ml@."
