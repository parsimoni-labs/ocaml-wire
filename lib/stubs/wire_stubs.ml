(** OCaml FFI stub generation for EverParse-produced C validators. *)

let ml_type_of = Wire.Private.ml_type_of
let everparse_name = Wire_3d.everparse_name

let c_stub_error_handler ppf lower =
  Fmt.pf ppf
    "static void %s_err(const char *t, const char *f, const char *r,@\n" lower;
  Fmt.pf ppf
    "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {@\n";
  Fmt.pf ppf
    "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;@\n";
  Fmt.pf ppf "}@\n"

let c_stub_validate ppf ~lower ~ep n_fields =
  Fmt.pf ppf "  int64_t fields[%d];@\n" (max 1 n_fields);
  Fmt.pf ppf "  WIRECTX ctx = { %s };@\n"
    (if n_fields > 0 then "fields" else "NULL");
  Fmt.pf ppf "  uint64_t r = %sValidate%s(&ctx, NULL, %s_err, data, len, 0);@\n"
    ep ep lower;
  Fmt.pf ppf
    "  if (!EverParseIsSuccess(r)) caml_failwith(\"%s: validation failed\");@\n"
    lower

let c_stub_output ppf ~lower ~ep (s : Wire.Everparse.Raw.struct_) =
  let kinds = Wire.Everparse.Raw.field_kinds s in
  let n_fields = List.length kinds in
  (* _parse: validate at offset, allocate record directly in C *)
  Fmt.pf ppf "CAMLprim value caml_wire_%s_parse(value v_buf, value v_off) {@\n"
    lower;
  Fmt.pf ppf "  CAMLparam2(v_buf, v_off);@\n";
  Fmt.pf ppf "  CAMLlocal1(v_result);@\n";
  Fmt.pf ppf
    "  uint8_t *data = (uint8_t *)Bytes_val(v_buf) + Int_val(v_off);@\n";
  Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf) - Int_val(v_off);@\n";
  c_stub_validate ppf ~lower ~ep n_fields;
  if n_fields > 0 then begin
    Fmt.pf ppf "  v_result = caml_alloc(%d, 0);@\n" n_fields;
    List.iteri
      (fun i (_, kind) ->
        match kind with
        | Wire.Private.Types.K_int64 ->
            Fmt.pf ppf
              "  Store_field(v_result, %d, caml_copy_int64(fields[%d]));@\n" i i
        | _ ->
            Fmt.pf ppf "  Store_field(v_result, %d, Val_long(fields[%d]));@\n" i
              i)
      kinds
  end
  else Fmt.pf ppf "  v_result = Val_unit;@\n";
  Fmt.pf ppf "  CAMLreturn(v_result);@\n";
  Fmt.pf ppf "}@\n@\n";
  (* _parse_k: apply constructor via caml_callbackN *)
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
    c_stub_validate ppf ~lower ~ep n_fields;
    Fmt.pf ppf "  value args[%d];@\n" n_fields;
    List.iteri
      (fun i (_, kind) ->
        match kind with
        | Wire.Private.Types.K_int64 ->
            Fmt.pf ppf "  args[%d] = caml_copy_int64(fields[%d]);@\n" i i
        | _ -> Fmt.pf ppf "  args[%d] = Val_long(fields[%d]);@\n" i i)
      kinds;
    Fmt.pf ppf "  v_result = caml_callbackN(v_k, %d, args);@\n" n_fields;
    Fmt.pf ppf "  CAMLreturn(v_result);@\n";
    Fmt.pf ppf "}@\n@\n"
  end

let c_stub ppf (s : Wire.Everparse.Raw.struct_) =
  let name = Wire.Everparse.Raw.struct_name s in
  let ep = everparse_name name in
  let lower = String.lowercase_ascii name in
  c_stub_error_handler ppf lower;
  c_stub_output ppf ~lower ~ep s

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
  Fmt.pf ppf "/* EverParse headers and sources */@\n";
  List.iteri
    (fun i (s : Wire.Everparse.Raw.struct_) ->
      let name = Wire.Everparse.Raw.struct_name s in
      if i = 0 then Fmt.pf ppf "#include \"EverParse.h\"@\n";
      Fmt.pf ppf "#include \"%s.h\"@\n" name;
      Fmt.pf ppf "#include \"%s.c\"@\n" name)
    structs;
  Fmt.pf ppf "@\n/* Stubs */@\n";
  List.iter (fun s -> c_stub ppf s) structs;
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
  | Wire.Private.Types.K_int -> "int"
  | K_int64 -> "int64"
  | K_bool -> "int"
  | K_string -> "string"
  | K_unit -> "unit"

let gen_ml_record ppf ~type_name kinds =
  Fmt.pf ppf "type %s = {" type_name;
  List.iteri
    (fun i (name, kind) ->
      if i > 0 then Fmt.pf ppf ";";
      Fmt.pf ppf " %s : %s" (ml_field_name name) (ml_kind_string kind))
    kinds;
  Fmt.pf ppf " }@\n@\n"

let gen_ml_k_type ppf kinds =
  Fmt.pf ppf "(";
  List.iter (fun (_, kind) -> Fmt.pf ppf "%s -> " (ml_kind_string kind)) kinds;
  Fmt.pf ppf "'r)"

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
        Fmt.pf ppf "external %s_parse : bytes -> int -> %s@\n" lower lower;
        Fmt.pf ppf "  = \"caml_wire_%s_parse\" [@@@@warning \"-61\"]@\n@\n"
          lower;
        Fmt.pf ppf "external %s_parse_k : %a -> bytes -> int -> 'r@\n" lower
          (fun ppf () -> gen_ml_k_type ppf kinds)
          ();
        Fmt.pf ppf "  = \"caml_wire_%s_parse_k\"@\n@\n" lower
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
      (fun ppf () -> gen_ml_k_type ppf kinds)
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

(* Generate the _ExternalTypedefs.h for EverParse output schemas.
   WIRECTX holds a pointer to a C int64_t array. WireSet callbacks store
   extracted field values by index. NULL means validation-only (no extraction). *)
let to_external_typedefs name =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "/* Generated by wire — defines WireCtx for %s */@\n" name;
  Fmt.pf ppf "#ifndef WIRECTX_DEFINED@\n";
  Fmt.pf ppf "#define WIRECTX_DEFINED@\n@\n";
  Fmt.pf ppf "#include <stdint.h>@\n@\n";
  Fmt.pf ppf "typedef struct { int64_t *fields; } WIRECTX;@\n@\n";
  Fmt.pf ppf "#endif@\n";
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Generate WireSet* C function implementations.
   Each callback stores the extracted field value into the C int64_t array
   at the field's index. When fields is NULL (validation-only), no-op. *)
let to_wire_setters () =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "/* Generated by wire — WireSet* field extraction */@\n@\n";
  Fmt.pf ppf "#include <stdint.h>@\n@\n";
  Fmt.pf ppf "#ifndef WIRECTX_DEFINED@\n";
  Fmt.pf ppf "#define WIRECTX_DEFINED@\n";
  Fmt.pf ppf "typedef struct { int64_t *fields; } WIRECTX;@\n";
  Fmt.pf ppf "#endif@\n@\n";
  List.iter
    (fun (c_type, fn_name) ->
      Fmt.pf ppf "void %s(WIRECTX *ctx, uint32_t idx, %s v) {@\n" fn_name c_type;
      Fmt.pf ppf "  if (ctx->fields) ctx->fields[idx] = (int64_t)v;@\n";
      Fmt.pf ppf "}@\n@\n")
    [
      ("uint8_t", "WireSetU8");
      ("uint16_t", "WireSetU16");
      ("uint16_t", "WireSetU16be");
      ("uint32_t", "WireSetU32");
      ("uint32_t", "WireSetU32be");
    ];
  Fmt.pf ppf "void WireSetU64(WIRECTX *ctx, uint32_t idx, uint64_t v) {@\n";
  Fmt.pf ppf "  if (ctx->fields) ctx->fields[idx] = (int64_t)v;@\n";
  Fmt.pf ppf "}@\n@\n";
  Fmt.pf ppf "void WireSetU64be(WIRECTX *ctx, uint32_t idx, uint64_t v) {@\n";
  Fmt.pf ppf "  if (ctx->fields) ctx->fields[idx] = (int64_t)v;@\n";
  Fmt.pf ppf "}@\n@\n";
  Format.pp_print_flush ppf ();
  Buffer.contents buf

type packed_codec = C : _ Wire.Codec.t -> packed_codec

let of_structs ~schema_dir ~outdir structs =
  List.iter
    (fun s ->
      let name = Wire.Everparse.Raw.struct_name s in
      write_file
        (Filename.concat schema_dir (name ^ "_ExternalTypedefs.h"))
        (to_external_typedefs name))
    structs;
  write_file
    (Filename.concat outdir "wire_ffi.c")
    (to_wire_setters () ^ to_c_stubs structs);
  write_file (Filename.concat outdir "stubs.ml") (to_ml_stubs structs)

let generate ~schema_dir ~outdir codecs =
  let structs =
    List.map (fun (C c) -> Wire.Everparse.struct_of_codec c) codecs
  in
  of_structs ~schema_dir ~outdir structs
