(** OCaml FFI stub generation for EverParse-produced C validators. *)

let ml_type_of = Wire.Private.ml_type_of
let everparse_name = Wire_3d.everparse_name

let pp_c_stub_error_handler ppf lower =
  Fmt.pf ppf
    "static void %s_err(const char *t, const char *f, const char *r,@\n" lower;
  Fmt.pf ppf
    "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {@\n";
  Fmt.pf ppf
    "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;@\n";
  Fmt.pf ppf "}@\n"

let c_stub_validate ppf ~lower ~ep =
  Fmt.pf ppf "  %sFields fields = {0};@\n" ep;
  Fmt.pf ppf
    "  uint64_t r = %sValidate%s((WIRECTX *) &fields, NULL, %s_err, data, len, \
     0);@\n"
    ep ep lower;
  Fmt.pf ppf
    "  if (!EverParseIsSuccess(r)) caml_failwith(\"%s: validation failed\");@\n"
    lower

let pp_field_value ppf (fname, kind) =
  match kind with
  | Wire.Everparse.Raw.Int64 ->
      Fmt.pf ppf "caml_copy_int64((int64_t) fields.%s)" fname
  | Float32 | Float64 ->
      (* The 3D plug stores floats with their typed C type ([float] /
         [double]) and bit-reinterprets in the setter, so we hand the
         value straight to [caml_copy_double]. *)
      Fmt.pf ppf "caml_copy_double((double) fields.%s)" fname
  | _ -> Fmt.pf ppf "Val_long(fields.%s)" fname

let pp_c_stub_output ppf ~lower ~ep (s : Wire.Everparse.Raw.struct_) =
  let kinds = Wire.Everparse.Raw.field_kinds s in
  let n_fields = List.length kinds in
  (* Single C entry point per schema: [caml_wire_<name>_parse_k] takes a
     continuation and applies it to the parsed field values via
     [caml_callbackN]. The OCaml side derives the record-returning
     [<name>_parse] from [<name>_parse_k] with a constructor continuation
     (see [to_ml_stubs] below); both flavours share this one C function. *)
  if n_fields > 0 then begin
    Fmt.pf ppf
      "CAMLprim value caml_wire_%s_parse_k(value v_k, value v_buf, value \
       v_off) {@\n"
      lower;
    Fmt.pf ppf "  CAMLparam3(v_k, v_buf, v_off);@\n";
    Fmt.pf ppf "  CAMLlocal1(v_result);@\n";
    Fmt.pf ppf
      "  uint8_t *data = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);@\n";
    Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf) - Int_val(v_off);@\n";
    c_stub_validate ppf ~lower ~ep;
    Fmt.pf ppf "  value args[%d];@\n" n_fields;
    List.iteri
      (fun i kind -> Fmt.pf ppf "  args[%d] = %a;@\n" i pp_field_value kind)
      kinds;
    Fmt.pf ppf "  v_result = caml_callbackN(v_k, %d, args);@\n" n_fields;
    Fmt.pf ppf "  CAMLreturn(v_result);@\n";
    Fmt.pf ppf "}@\n@\n"
  end
  else begin
    (* Zero-field schema: no callback needed, just validate and return unit. *)
    Fmt.pf ppf
      "CAMLprim value caml_wire_%s_parse(value v_buf, value v_off) {@\n" lower;
    Fmt.pf ppf "  CAMLparam2(v_buf, v_off);@\n";
    Fmt.pf ppf
      "  uint8_t *data = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);@\n";
    Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf) - Int_val(v_off);@\n";
    c_stub_validate ppf ~lower ~ep;
    Fmt.pf ppf "  CAMLreturn(Val_unit);@\n";
    Fmt.pf ppf "}@\n@\n"
  end

let pp_c_stub ppf (s : Wire.Everparse.Raw.struct_) =
  let name = Wire.Everparse.Raw.struct_name s in
  let ep = everparse_name name in
  let lower = String.lowercase_ascii name in
  pp_c_stub_error_handler ppf lower;
  pp_c_stub_output ppf ~lower ~ep s

let to_c_stubs (structs : Wire.Everparse.Raw.struct_ list) =
  let buf = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf
    "/* wire_stubs.c - OCaml FFI stubs for EverParse-generated C */@\n@\n";
  Fmt.pf ppf "#include <caml/mlvalues.h>@\n";
  Fmt.pf ppf "#include <caml/memory.h>@\n";
  Fmt.pf ppf "#include <caml/alloc.h>@\n";
  Fmt.pf ppf "#include <caml/fail.h>@\n";
  Fmt.pf ppf "#include <caml/callback.h>@\n";
  Fmt.pf ppf "#include <stdint.h>@\n";
  Fmt.pf ppf "#include <string.h>@\n";
  Fmt.pf ppf "@\n";
  Fmt.pf ppf "/* EverParse headers + default <Name>_Fields plug */@\n";
  List.iteri
    (fun i (s : Wire.Everparse.Raw.struct_) ->
      let name = Wire.Everparse.Raw.struct_name s in
      if i = 0 then Fmt.pf ppf "#include \"EverParse.h\"@\n";
      Fmt.pf ppf "#include \"%s_Fields.h\"@\n" name;
      Fmt.pf ppf "#include \"%s.h\"@\n" name;
      Fmt.pf ppf "#include \"%s.c\"@\n" name;
      Fmt.pf ppf "#include \"%s_Fields.c\"@\n" name)
    structs;
  Fmt.pf ppf "@\n/* Stubs */@\n";
  List.iter (fun s -> pp_c_stub ppf s) structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let ml_field_name name =
  let lower = String.lowercase_ascii name in
  match lower with
  | "and" | "as" | "assert" | "begin" | "class" | "constraint" | "do" | "done"
  | "downto" | "else" | "end" | "exception" | "external" | "false" | "for"
  | "fun" | "function" | "functor" | "if" | "in" | "include" | "inherit"
  | "initializer" | "lazy" | "let" | "match" | "method" | "module" | "mutable"
  | "new" | "nonrec" | "object" | "of" | "open" | "or" | "private" | "rec"
  | "sig" | "struct" | "then" | "to" | "true" | "try" | "type" | "val"
  | "virtual" | "when" | "while" | "with" ->
      lower ^ "_"
  | _ -> lower

let ml_kind_string = function
  | Wire.Everparse.Raw.Int -> "int"
  | Int64 -> "int64"
  | Float32 | Float64 -> "float"
  | Bool -> "int"
  | String -> "string"
  | Unit -> "unit"

let gen_ml_record ppf ~type_name kinds =
  Fmt.pf ppf "type %s = {" type_name;
  List.iteri
    (fun i (name, kind) ->
      if i > 0 then Fmt.pf ppf ";";
      Fmt.pf ppf " %s : %s" (ml_field_name name) (ml_kind_string kind))
    kinds;
  Fmt.pf ppf " }@\n@\n"

let pp_ml_k_type ppf kinds =
  Fmt.pf ppf "(";
  List.iter (fun (_, kind) -> Fmt.pf ppf "%s -> " (ml_kind_string kind)) kinds;
  Fmt.pf ppf "'r)"

(* Emit [<name>_parse] as an OCaml-side wrapper over the single C
   [<name>_parse_k]. The continuation is a record constructor; the OCaml
   compiler eliminates it when the caller only wants the record (the
   record is built directly from the field values). Single C codepath
   per schema; both record-returning and CPS ergonomics preserved. *)
let pp_ml_record_constructor ppf kinds =
  Fmt.pf ppf "(fun ";
  List.iteri (fun i _ -> Fmt.pf ppf "v%d " i) kinds;
  Fmt.pf ppf "-> { ";
  List.iteri
    (fun i (name, _) ->
      if i > 0 then Fmt.pf ppf "; ";
      Fmt.pf ppf "%s = v%d" (ml_field_name name) i)
    kinds;
  Fmt.pf ppf " })"

let to_ml_stubs (structs : Wire.Everparse.Raw.struct_ list) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  List.iter
    (fun (s : Wire.Everparse.Raw.struct_) ->
      let lower = String.lowercase_ascii (Wire.Everparse.Raw.struct_name s) in
      let kinds = Wire.Everparse.Raw.field_kinds s in
      if kinds <> [] then begin
        gen_ml_record ppf ~type_name:lower kinds;
        (* Single external: the CPS variant. *)
        Fmt.pf ppf "external %s_parse_k : %a -> bytes -> int -> 'r@\n" lower
          (fun ppf () -> pp_ml_k_type ppf kinds)
          ();
        Fmt.pf ppf "  = \"caml_wire_%s_parse_k\"@\n@\n" lower;
        (* OCaml-side record-returning wrapper. *)
        Fmt.pf ppf "let %s_parse buf off =@\n" lower;
        Fmt.pf ppf "  %s_parse_k@\n" lower;
        Fmt.pf ppf "    %a@\n" pp_ml_record_constructor kinds;
        Fmt.pf ppf "    buf off@\n@\n"
      end
      else begin
        Fmt.pf ppf "external %s_parse : bytes -> int -> unit@\n" lower;
        Fmt.pf ppf "  = \"caml_wire_%s_parse\"@\n@\n" lower
      end)
    structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let to_ml_stub_name (s : Wire.Everparse.Raw.struct_) =
  let name = Wire.Everparse.Raw.struct_name s in
  let buf = Buffer.create (String.length name + 4) in
  String.iteri
    (fun i c ->
      if i > 0 && Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c then
        Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    name;
  Buffer.contents buf

let to_ml_stub (s : Wire.Everparse.Raw.struct_) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let lower = String.lowercase_ascii (Wire.Everparse.Raw.struct_name s) in
  let kinds = Wire.Everparse.Raw.field_kinds s in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  if kinds <> [] then begin
    gen_ml_record ppf ~type_name:"t" kinds;
    Fmt.pf ppf "external parse : bytes -> int -> t@\n";
    Fmt.pf ppf "  = \"caml_wire_%s_parse\"@\n@\n" lower;
    Fmt.pf ppf "external parse_k : %a -> bytes -> int -> 'r@\n"
      (fun ppf () -> pp_ml_k_type ppf kinds)
      ();
    Fmt.pf ppf "  = \"caml_wire_%s_parse_k\"@\n" lower
  end
  else begin
    Fmt.pf ppf "external parse : bytes -> int -> unit@\n";
    Fmt.pf ppf "  = \"caml_wire_%s_parse\"@\n" lower
  end;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

type packed_codec = C : _ Wire.Codec.t -> packed_codec

let of_structs ~schema_dir ~outdir structs =
  let schemas = List.map Wire.Everparse.schema_of_struct structs in
  Wire_3d.write_external_typedefs ~outdir:schema_dir schemas;
  Wire_3d.write_fields ~outdir:schema_dir schemas;
  write_file (Filename.concat outdir "wire_ffi.c") (to_c_stubs structs);
  write_file (Filename.concat outdir "stubs.ml") (to_ml_stubs structs)

let generate ~schema_dir ~outdir codecs =
  let structs =
    List.map (fun (C c) -> Wire.Everparse.struct_of_codec c) codecs
  in
  of_structs ~schema_dir ~outdir structs
