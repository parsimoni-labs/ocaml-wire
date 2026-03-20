(** Generate EverParse .3d files, run EverParse, and generate C/OCaml FFI stubs
    and benchmark loops for the schemas used in bench_demo.ml.

    Usage: gen_stubs.exe <schema_dir>

    Outputs:
    - <schema_dir>/*.3d -- EverParse 3D schema files
    - c_stubs.c -- C validation stubs + noop stubs + timed C loops
    - c_stubs.ml -- OCaml externals for all stubs *)

let schema_dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "schemas"

(* Each entry: (name, struct, wire_size, extra_decls).
   extra_decls are emitted before the typedef in the .3d module — needed for
   enum declarations that the struct fields reference. *)
let entries =
  [
    (* Demo: synthetic schemas covering every Wire type *)
    ("Minimal", Demo.minimal_struct, Demo.minimal_size, []);
    ("Bitfield8", Demo.bf8_struct, Demo.bf8_size, []);
    ("Bitfield16", Demo.bf16_struct, Demo.bf16_size, []);
    ("BoolFields", Demo.bool_fields_struct, Demo.bool_fields_size, []);
    ("Bitfield32", Demo.bf32_struct, Demo.bf32_size, []);
    ("AllInts", Demo.all_ints_struct, Demo.all_ints_size, []);
    ("LargeMixed", Demo.large_mixed_struct, Demo.large_mixed_size, []);
    (* Demo: type combinators *)
    ("Mapped", Demo.mapped_struct, Demo.mapped_size, []);
    ("CasesDemo", Demo.cases_demo_struct, Demo.cases_demo_size, []);
    ("EnumDemo", Demo.enum_demo_struct, Demo.enum_demo_size, []);
    ("Constrained", Demo.constrained_struct, Demo.constrained_size, []);
    (* Space: real protocols *)
    ("CLCW", Space.clcw_struct, Space.clcw_size, []);
    ("SpacePacket", Space.packet_struct, Space.packet_size, []);
    ("TMFrame", Space.tm_frame_struct, Space.tm_frame_size, []);
    (* Net: TCP/IP headers *)
    ("Ethernet", Net.ethernet_struct, Net.ethernet_size, []);
    ("IPv4", Net.ipv4_struct, Net.ipv4_size, []);
    ("TCP", Net.tcp_struct, Net.tcp_size, []);
  ]

let () =
  let structs = List.map (fun (_, s, _, _) -> s) entries in
  let schemas =
    List.map
      (fun (name, s, wire_size, extra_decls) ->
        Wire_3d.schema ~name
          ~module_:
            (Wire.C.Raw.module_
               (extra_decls @ [ Wire.C.Raw.typedef ~entrypoint:true s ]))
          ~wire_size)
      entries
  in

  (* Step 1: Generate .3d files and run EverParse *)
  Wire_3d.generate_3d ~outdir:schema_dir schemas;
  let quiet = Sys.getenv_opt "EVERPARSE_VERBOSE" = None in
  Wire_3d.run_everparse ~quiet ~outdir:schema_dir schemas;

  (* Step 2: Generate c_stubs.c *)
  let oc = open_out "c_stubs.c" in
  output_string oc (Wire_stubs.to_c_stubs structs);
  let pr fmt = Printf.fprintf oc fmt in

  pr "\n/* ── Noop FFI stubs (measure call overhead) ── */\n\n";
  pr "CAMLprim value ep_noop(value v_buf) {\n";
  pr "  (void)v_buf;\n";
  pr "  return Val_true;\n";
  pr "}\n\n";
  pr "CAMLprim value ep_noop_safe(value v_buf) {\n";
  pr "  CAMLparam1(v_buf);\n";
  pr "  CAMLreturn(Val_true);\n";
  pr "}\n\n";

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
      pr "CAMLprim value ep_loop_%s(value v_buf, value v_off, value v_n) {\n"
        lower;
      pr "  uint8_t *buf = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);\n";
      pr "  uint32_t len = caml_string_length(v_buf) - Int_val(v_off);\n";
      pr "  int count = Int_val(v_n);\n";
      pr "  volatile uint64_t result;\n";
      pr "  int64_t t0 = now_ns();\n";
      pr "  for (int i = 0; i < count; i++) {\n";
      pr "    result = %sValidate%s(NULL, bench_err, buf, len, 0);\n" ep ep;
      pr "  }\n";
      pr "  (void)result;\n";
      pr "  int64_t t1 = now_ns();\n";
      pr "  return Val_int(t1 - t0);\n";
      pr "}\n\n")
    structs;
  close_out oc;

  (* Step 3: Generate c_stubs.ml *)
  let oc = open_out "c_stubs.ml" in
  output_string oc (Wire_stubs.to_ml_stubs structs);
  let pr fmt = Printf.fprintf oc fmt in
  pr "(* Noop FFI stubs *)\n\n";
  pr "external noop : bytes -> bool = \"ep_noop\" [@@noalloc]\n\n";
  pr "external noop_safe : bytes -> bool = \"ep_noop_safe\"\n\n";
  pr "(* Timed C benchmark loops *)\n\n";
  List.iter
    (fun s ->
      let lower = String.lowercase_ascii (Wire.C.Raw.struct_name s) in
      pr "external %s_loop : bytes -> int -> int -> int = \"ep_loop_%s\"\n\n"
        lower lower)
    structs;
  close_out oc;

  Printf.printf "Generated %d schemas in %s/\n" (List.length structs) schema_dir;
  Printf.printf "Generated c_stubs.c, c_stubs.ml\n"
