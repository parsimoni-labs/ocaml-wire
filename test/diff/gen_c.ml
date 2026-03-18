(* Generate .3d files from random wire schemas for EverParse.

   All schemas are randomly generated with deterministic seeds. Fields of any
   type may get constraints (~25% probability per field). *)

(* ---- Field type metadata ---- *)

type ft = {
  make_field : string -> bool Wire.expr option -> Wire.field;
  wire_size : int;
  gen_constraint : Random.State.t -> int;
  big_endian : bool;
}

let gen_uint8 rng = Random.State.int rng 256
let gen_uint16 rng = Random.State.int rng 65536

let gen_uint32 rng =
  Int32.unsigned_to_int (Random.State.bits32 rng) |> Option.get

let gen_uint64 rng =
  Int64.to_int (Int64.logand (Random.State.bits64 rng) 0x3FFF_FFFF_FFFF_FFFFL)

let field_types =
  [|
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint8);
      wire_size = 1;
      gen_constraint = gen_uint8;
      big_endian = false;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint16);
      wire_size = 2;
      gen_constraint = gen_uint16;
      big_endian = false;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint16be);
      wire_size = 2;
      gen_constraint = gen_uint16;
      big_endian = true;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint32);
      wire_size = 4;
      gen_constraint = gen_uint32;
      big_endian = false;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint32be);
      wire_size = 4;
      gen_constraint = gen_uint32;
      big_endian = true;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint64);
      wire_size = 8;
      gen_constraint = gen_uint64;
      big_endian = false;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint64be);
      wire_size = 8;
      gen_constraint = gen_uint64;
      big_endian = true;
    };
  |]

(* ---- Random schema generation ---- *)

type random_field = {
  name : string;
  ft : ft;
  constraint_val : int option;
  big_endian : bool;
}

type random_schema = {
  struct_ : Wire.C.struct_;
  fields : random_field list;
  total_wire_size : int;
}

let random_struct seed =
  let rng = Random.State.make [| seed |] in
  let n = 1 + Random.State.int rng 6 in
  let fields =
    List.init n (fun i ->
        let ft =
          field_types.(Random.State.int rng (Array.length field_types))
        in
        let name = Fmt.str "f%d" i in
        let constraint_val =
          if Random.State.int rng 4 = 0 then Some (ft.gen_constraint rng)
          else None
        in
        { name; ft; constraint_val; big_endian = ft.big_endian })
  in
  let struct_name = Fmt.str "Random%d" seed in
  let wire_fields =
    List.map
      (fun rf ->
        let constraint_ =
          Option.map
            (fun k -> Wire.Expr.(Wire.field_ref rf.name <= Wire.int k))
            rf.constraint_val
        in
        rf.ft.make_field rf.name constraint_)
      fields
  in
  let total_wire_size =
    List.fold_left (fun acc rf -> acc + rf.ft.wire_size) 0 fields
  in
  { struct_ = Wire.C.struct_ struct_name wire_fields; fields; total_wire_size }

(* ---- Code generation for differential testing ---- *)

let generate_c_stubs ~schema_dir outdir schemas =
  let oc = open_out (Filename.concat outdir "stubs.c") in
  let pr fmt = Printf.fprintf oc fmt in
  pr "#include <caml/mlvalues.h>\n";
  pr "#include <caml/memory.h>\n";
  pr "#include <caml/alloc.h>\n";
  pr "#include <stdint.h>\n\n";
  (* Include all wrapper headers - they declare the check functions *)
  List.iter
    (fun rs ->
      let name = Wire.C.struct_name rs.struct_ in
      pr "#include \"%s/%sWrapper.h\"\n" schema_dir name)
    schemas;
  pr "\n";
  (* Include wrapper implementations with unique error handlers *)
  List.iteri
    (fun i rs ->
      let name = Wire.C.struct_name rs.struct_ in
      (* Include EverParse.h and parser *)
      if i = 0 then pr "#include \"%s/EverParse.h\"\n" schema_dir;
      pr "#include \"%s/%s.h\"\n" schema_dir name;
      pr "#include \"%s/%s.c\"\n" schema_dir name;
      (* Inline wrapper with renamed error handler *)
      pr
        "void %sEverParseError(const char *s, const char *f, const char *r) { \
         (void)s; (void)f; (void)r; }\n"
        name;
      pr "static void %s_ErrorHandler(\n" name;
      pr "  const char *t, const char *f, const char *r,\n";
      pr "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {\n";
      pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
      pr "}\n";
      pr "BOOLEAN %sCheck%s(uint8_t *base, uint32_t len) {\n" name name;
      pr
        "  uint64_t result = %sValidate%s(NULL, %s_ErrorHandler, base, len, 0);\n"
        name name name;
      pr "  return EverParseIsSuccess(result);\n";
      pr "}\n\n")
    schemas;
  (* Generate OCaml stubs *)
  List.iter
    (fun rs ->
      let name = Wire.C.struct_name rs.struct_ in
      pr "CAMLprim value caml_%s_check(value v_bytes) {\n"
        (String.lowercase_ascii name);
      pr "  CAMLparam1(v_bytes);\n";
      pr "  uint8_t *data = (uint8_t *)Bytes_val(v_bytes);\n";
      pr "  uint32_t len = caml_string_length(v_bytes);\n";
      pr "  BOOLEAN result = %sCheck%s(data, len);\n" name name;
      pr "  CAMLreturn(Val_bool(result));\n";
      pr "}\n\n")
    schemas;
  close_out oc

let generate_ml_stubs outdir schemas =
  let oc = open_out (Filename.concat outdir "stubs.ml") in
  let pr fmt = Printf.fprintf oc fmt in
  List.iter
    (fun rs ->
      let name = Wire.C.struct_name rs.struct_ in
      let lower = String.lowercase_ascii name in
      pr "external %s_check : bytes -> bool = \"caml_%s_check\"\n" lower lower)
    schemas;
  close_out oc

let emit_constraint_check oc rf offset k =
  let pr fmt = Printf.fprintf oc fmt in
  let endian = if rf.big_endian then "be" else "le" in
  match rf.ft.wire_size with
  | 1 ->
      pr "  let %s = Bytes.get_uint8 buf %d in\n" rf.name offset;
      pr "  if %s > %d then false else\n" rf.name k
  | 2 ->
      pr "  let %s = Bytes.get_uint16_%s buf %d in\n" rf.name endian offset;
      pr "  if %s > %d then false else\n" rf.name k
  | 4 ->
      pr "  let %s = Bytes.get_int32_%s buf %d in\n" rf.name endian offset;
      Printf.fprintf oc
        "  if Int32.unsigned_compare %s (%ldl) > 0 then false else\n" rf.name
        (Int32.of_int k)
  | 8 ->
      pr "  let %s = Bytes.get_int64_%s buf %d in\n" rf.name endian offset;
      Printf.fprintf oc
        "  if Int64.unsigned_compare %s (%LdL) > 0 then false else\n" rf.name
        (Int64.of_int k)
  | _ ->
      pr "  let %s = Bytes.get_uint8 buf %d in\n" rf.name offset;
      pr "  if %s > %d then false else\n" rf.name k

let fields_with_offsets fields =
  let rec aux offset = function
    | [] -> []
    | rf :: rest -> (rf, offset) :: aux (offset + rf.ft.wire_size) rest
  in
  aux 0 fields

let emit_wire_checker oc rs =
  let pr fmt = Printf.fprintf oc fmt in
  let name = Wire.C.struct_name rs.struct_ in
  let lower = String.lowercase_ascii name in
  pr "(* %s: wire_size=%d *)\n" name rs.total_wire_size;
  pr "let %s_wire_check (buf : bytes) : bool =\n" lower;
  pr "  if Bytes.length buf < %d then false else\n" rs.total_wire_size;
  let constrained_fields =
    List.filter_map
      (fun (rf, offset) ->
        Option.map (fun k -> (rf, offset, k)) rf.constraint_val)
      (fields_with_offsets rs.fields)
  in
  List.iter
    (fun (rf, offset, k) -> emit_constraint_check oc rf offset k)
    constrained_fields;
  pr "  true\n\n"

let generate_test_runner outdir schemas =
  let oc = open_out (Filename.concat outdir "diff_test.ml") in
  let pr fmt = Printf.fprintf oc fmt in
  pr "(* Auto-generated differential test runner *)\n\n";
  pr "let num_values = 100\n\n";
  pr "type schema = {\n";
  pr "  name : string;\n";
  pr "  wire_size : int;\n";
  pr "  wire_check : bytes -> bool;\n";
  pr "  c_check : bytes -> bool;\n";
  pr "}\n\n";
  List.iter (emit_wire_checker oc) schemas;
  (* Generate schema list *)
  pr "let schemas = [\n";
  List.iter
    (fun rs ->
      let name = Wire.C.struct_name rs.struct_ in
      let lower = String.lowercase_ascii name in
      pr
        "  { name = %S; wire_size = %d; wire_check = %s_wire_check; c_check = \
         Stubs.%s_check };\n"
        name rs.total_wire_size lower lower)
    schemas;
  pr "]\n\n";
  (* Test runner *)
  pr "let () =\n";
  pr "  let seed = 42 in\n";
  pr "  let rng = Random.State.make [| seed |] in\n";
  pr "  let total_tests = ref 0 in\n";
  pr "  let mismatches = ref 0 in\n";
  pr "  List.iter (fun schema ->\n";
  pr "    for _ = 1 to num_values do\n";
  pr "      let buf = Bytes.create schema.wire_size in\n";
  pr "      for i = 0 to schema.wire_size - 1 do\n";
  pr "        Bytes.set buf i (Char.chr (Random.State.int rng 256))\n";
  pr "      done;\n";
  pr "      let wire_ok = schema.wire_check buf in\n";
  pr "      let c_ok = schema.c_check buf in\n";
  pr "      incr total_tests;\n";
  pr "      if wire_ok <> c_ok then begin\n";
  pr "        incr mismatches;\n";
  pr
    "        Printf.printf \"MISMATCH %%s: wire=%%b c=%%b\\n\" schema.name \
     wire_ok c_ok\n";
  pr "      end\n";
  pr "    done\n";
  pr "  ) schemas;\n";
  pr
    "  Printf.printf \"Tested %%d values across %%d schemas, %%d mismatches\\n\"\n";
  pr "    !total_tests (List.length schemas) !mismatches;\n";
  pr "  if !mismatches > 0 then exit 1\n";
  close_out oc

(* ---- Main ---- *)

let () =
  let schema_dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "schemas"
  in
  let num_random =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 20
  in
  let schemas = List.init num_random (fun i -> random_struct i) in
  let wire_c_schemas =
    List.map
      (fun rs ->
        Wire_c.schema
          ~name:(Wire.C.struct_name rs.struct_)
          ~module_:(Wire.module_ [ Wire.typedef ~entrypoint:true rs.struct_ ])
          ~wire_size:rs.total_wire_size)
      schemas
  in
  Wire_c.generate_3d ~outdir:schema_dir wire_c_schemas;
  Wire_c.run_everparse ~outdir:schema_dir wire_c_schemas;
  (* Generate FFI stubs and test runner in current dir *)
  generate_c_stubs ~schema_dir "." schemas;
  generate_ml_stubs "." schemas;
  generate_test_runner "." schemas
