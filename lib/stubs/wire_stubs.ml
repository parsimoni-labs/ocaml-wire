(** OCaml FFI stub generation for EverParse-produced C validators. *)

let ml_type_of = Wire.Private.ml_type_of
let param_name = Wire.Private.param_name
let param_is_mutable = Wire.Private.param_is_mutable
let param_c_type = Wire.Private.param_c_type
let everparse_name = Wire_3d.everparse_name

let c_stub_error_handler ppf lower =
  Fmt.pf ppf
    "static void %s_err(const char *t, const char *f, const char *r,@\n" lower;
  Fmt.pf ppf
    "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {@\n";
  Fmt.pf ppf
    "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;@\n";
  Fmt.pf ppf "}@\n"

let field_names = Wire.C.Raw.field_names

let c_stub_output ppf ~lower ~ep s =
  let fields = field_names s in
  let n_fields = List.length fields in
  Fmt.pf ppf "CAMLprim value caml_wire_%s_parse(value v_buf) {@\n" lower;
  Fmt.pf ppf "  CAMLparam1(v_buf);@\n";
  Fmt.pf ppf "  CAMLlocal1(v_result);@\n";
  Fmt.pf ppf "  uint8_t *data = (uint8_t *)Bytes_val(v_buf);@\n";
  Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf);@\n";
  Fmt.pf ppf "  v_result = caml_alloc(%d, 0);@\n" n_fields;
  Fmt.pf ppf "  WIRECTX ctx = { v_result };@\n";
  Fmt.pf ppf "  uint64_t r = %sValidate%s(&ctx, NULL, %s_err, data, len, 0);@\n"
    ep ep lower;
  Fmt.pf ppf "  if (!EverParseIsSuccess(r)) { CAMLreturn(Atom(0)); }@\n";
  Fmt.pf ppf "  CAMLreturn(v_result);@\n";
  Fmt.pf ppf "}@\n@\n"

let c_stub_no_params ppf ~lower ~ep =
  Fmt.pf ppf "CAMLprim value caml_wire_%s_check(value v_buf) {@\n" lower;
  Fmt.pf ppf "  uint8_t *data = (uint8_t *)Bytes_val(v_buf);@\n";
  Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf);@\n";
  Fmt.pf ppf "  uint64_t r = %sValidate%s(NULL, %s_err, data, len, 0);@\n" ep ep
    lower;
  Fmt.pf ppf "  return Val_bool(EverParseIsSuccess(r));@\n";
  Fmt.pf ppf "}@\n@\n"

let c_stub_with_params ppf ~lower ~ep ~input_params ~output_params =
  let n_args = 1 + List.length input_params + List.length output_params in
  let arg_names =
    "v_buf"
    :: List.map (fun (p : Wire.param) -> "v_" ^ param_name p) input_params
    @ List.map (fun (p : Wire.param) -> "v_" ^ param_name p) output_params
  in
  if n_args <= 5 then begin
    Fmt.pf ppf "CAMLprim value caml_wire_%s_check(%s) {@\n" lower
      (String.concat ", " (List.map (fun a -> "value " ^ a) arg_names))
  end
  else begin
    Fmt.pf ppf
      "CAMLprim value caml_wire_%s_check_bytecode(value *argv, int argn) {@\n"
      lower;
    Fmt.pf ppf "  (void)argn;@\n";
    List.iteri (fun i a -> Fmt.pf ppf "  value %s = argv[%d];@\n" a i) arg_names;
    Fmt.pf ppf "@\n"
  end;
  Fmt.pf ppf "  uint8_t *data = (uint8_t *)Bytes_val(v_buf);@\n";
  Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf);@\n";
  List.iter
    (fun p ->
      let n = param_name p in
      Fmt.pf ppf "  %s %s_val = Int_val(v_%s);@\n" (param_c_type p) n n)
    input_params;
  List.iter
    (fun p ->
      let n = param_name p in
      Fmt.pf ppf "  %s %s_val = 0;@\n" (param_c_type p) n)
    output_params;
  let param_args =
    List.map (fun p -> param_name p ^ "_val") input_params
    @ List.map (fun p -> "&" ^ param_name p ^ "_val") output_params
  in
  let all_args = param_args @ [ "NULL"; lower ^ "_err"; "data"; "len"; "0" ] in
  Fmt.pf ppf "  uint64_t r = %sValidate%s(%s);@\n" ep ep
    (String.concat ", " all_args);
  List.iter
    (fun p ->
      let n = param_name p in
      Fmt.pf ppf "  Store_field(v_%s, 0, Val_int(%s_val));@\n" n n)
    output_params;
  Fmt.pf ppf "  return Val_bool(EverParseIsSuccess(r));@\n";
  Fmt.pf ppf "}@\n@\n"

let c_stub_check ?(output = false) ppf (s : Wire.C.Raw.struct_) =
  let name = Wire.C.Raw.struct_name s in
  let params = Wire.C.Raw.struct_params s in
  let ep = everparse_name name in
  let lower = String.lowercase_ascii name in
  c_stub_error_handler ppf lower;
  let input_params =
    List.filter (fun (p : Wire.param) -> not (param_is_mutable p)) params
  in
  let output_params =
    List.filter (fun (p : Wire.param) -> param_is_mutable p) params
  in
  if output then c_stub_output ppf ~lower ~ep s
  else if params = [] then c_stub_no_params ppf ~lower ~ep
  else c_stub_with_params ppf ~lower ~ep ~input_params ~output_params

let to_c_stubs ?(output = false) (structs : Wire.C.Raw.struct_ list) =
  let buf = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf
    "/* wire_stubs.c - OCaml FFI stubs for EverParse-generated C */@\n@\n";
  Fmt.pf ppf "#include <caml/mlvalues.h>@\n";
  Fmt.pf ppf "#include <caml/memory.h>@\n";
  Fmt.pf ppf "#include <caml/alloc.h>@\n";
  Fmt.pf ppf "#include <stdint.h>@\n";
  if output then Fmt.pf ppf "#include <string.h>@\n";
  Fmt.pf ppf "@\n";
  Fmt.pf ppf "/* EverParse headers and sources */@\n";
  List.iteri
    (fun i (s : Wire.C.Raw.struct_) ->
      let name = Wire.C.Raw.struct_name s in
      if i = 0 then Fmt.pf ppf "#include \"EverParse.h\"@\n";
      Fmt.pf ppf "#include \"%s.h\"@\n" name;
      Fmt.pf ppf "#include \"%s.c\"@\n" name)
    structs;
  Fmt.pf ppf "@\n/* Stubs */@\n";
  List.iter (fun s -> c_stub_check ~output ppf s) structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let to_ml_stubs ?(output = false) (structs : Wire.C.Raw.struct_ list) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  List.iter
    (fun (s : Wire.C.Raw.struct_) ->
      let lower = String.lowercase_ascii (Wire.C.Raw.struct_name s) in
      let params = Wire.C.Raw.struct_params s in
      let input_params =
        List.filter (fun (p : Wire.param) -> not (param_is_mutable p)) params
      in
      let output_params =
        List.filter (fun (p : Wire.param) -> param_is_mutable p) params
      in
      if output then begin
        Fmt.pf ppf "external %s_parse : bytes -> 'a@\n" lower;
        Fmt.pf ppf "  = \"caml_wire_%s_parse\"@\n@\n" lower
      end
      else if params = [] then begin
        Fmt.pf ppf "external %s_check : bytes -> bool@\n" lower;
        Fmt.pf ppf "  = \"caml_wire_%s_check\" [@@@@noalloc]@\n@\n" lower
      end
      else begin
        let input_types = List.map (fun _ -> "int") input_params in
        let output_types = List.map (fun _ -> "int array") output_params in
        let all_types = ("bytes" :: input_types) @ output_types @ [ "bool" ] in
        let n_args = List.length all_types - 1 in
        Fmt.pf ppf "external %s_check : %s@\n" lower
          (String.concat " -> " all_types);
        if n_args > 5 then
          Fmt.pf ppf
            "  = \"caml_wire_%s_check_bytecode\" \"caml_wire_%s_check\"@\n@\n"
            lower lower
        else Fmt.pf ppf "  = \"caml_wire_%s_check\"@\n@\n" lower
      end)
    structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let to_ml_stub_name (s : Wire.C.Raw.struct_) =
  let name = Wire.C.Raw.struct_name s in
  let buf = Buffer.create (String.length name + 4) in
  String.iteri
    (fun i c ->
      if i > 0 && Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c then
        Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    name;
  Buffer.contents buf

let to_ml_stub ?(output = false) (s : Wire.C.Raw.struct_) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let lower = String.lowercase_ascii (Wire.C.Raw.struct_name s) in
  let params = Wire.C.Raw.struct_params s in
  let input_params =
    List.filter (fun (p : Wire.param) -> not (param_is_mutable p)) params
  in
  let output_params =
    List.filter (fun (p : Wire.param) -> param_is_mutable p) params
  in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  if output then begin
    Fmt.pf ppf "external parse : bytes -> 'a@\n";
    Fmt.pf ppf "  = \"caml_wire_%s_parse\"@\n" lower
  end
  else if params = [] then begin
    Fmt.pf ppf "external check : bytes -> bool@\n";
    Fmt.pf ppf "  = \"caml_wire_%s_check\" [@@@@noalloc]@\n" lower
  end
  else begin
    let input_types = List.map (fun _ -> "int") input_params in
    let output_types = List.map (fun _ -> "int array") output_params in
    let all_types = ("bytes" :: input_types) @ output_types @ [ "bool" ] in
    let n_args = List.length all_types - 1 in
    Fmt.pf ppf "external check : %s@\n" (String.concat " -> " all_types);
    if n_args > 5 then
      Fmt.pf ppf "  = \"caml_wire_%s_check_bytecode\" \"caml_wire_%s_check\"@\n"
        lower lower
    else Fmt.pf ppf "  = \"caml_wire_%s_check\"@\n" lower
  end;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Generate the _ExternalTypedefs.h for EverParse output schemas.
   Defines WIRECTX as a struct holding an OCaml value. *)
let to_external_typedefs name =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "/* Generated by wire — defines WireCtx for %s */@\n" name;
  Fmt.pf ppf "#ifndef WIRECTX_DEFINED@\n";
  Fmt.pf ppf "#define WIRECTX_DEFINED@\n@\n";
  Fmt.pf ppf "#include <caml/mlvalues.h>@\n@\n";
  Fmt.pf ppf "typedef struct { value v_record; } WIRECTX;@\n@\n";
  Fmt.pf ppf "#endif@\n";
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Generate WireSet* C function implementations.
   Each setter stores a field value into the OCaml record via WireCtx. *)
let to_wire_setters () =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "/* Generated by wire — WireSet* implementations */@\n@\n";
  Fmt.pf ppf "#include <stdint.h>@\n";
  Fmt.pf ppf "#include <caml/mlvalues.h>@\n";
  Fmt.pf ppf "#include <caml/memory.h>@\n";
  Fmt.pf ppf "#include <caml/alloc.h>@\n@\n";
  Fmt.pf ppf "#ifndef WIRECTX_DEFINED@\n";
  Fmt.pf ppf "#define WIRECTX_DEFINED@\n";
  Fmt.pf ppf "typedef struct { value v_record; } WIRECTX;@\n";
  Fmt.pf ppf "#endif@\n@\n";
  List.iter
    (fun (c_type, fn_name) ->
      Fmt.pf ppf "void %s(WIRECTX *ctx, uint32_t idx, %s v) {@\n" fn_name c_type;
      Fmt.pf ppf "  Store_field(ctx->v_record, idx, Val_long((long)v));@\n";
      Fmt.pf ppf "}@\n@\n")
    [
      ("uint8_t", "WireSetU8");
      ("uint16_t", "WireSetU16");
      ("uint16_t", "WireSetU16be");
      ("uint32_t", "WireSetU32");
      ("uint32_t", "WireSetU32be");
    ];
  Fmt.pf ppf "void WireSetU64(WIRECTX *ctx, uint32_t idx, uint64_t v) {@\n";
  Fmt.pf ppf "  Store_field(ctx->v_record, idx, caml_copy_int64(v));@\n";
  Fmt.pf ppf "}@\n@\n";
  Fmt.pf ppf "void WireSetU64be(WIRECTX *ctx, uint32_t idx, uint64_t v) {@\n";
  Fmt.pf ppf "  Store_field(ctx->v_record, idx, caml_copy_int64(v));@\n";
  Fmt.pf ppf "}@\n@\n";
  Format.pp_print_flush ppf ();
  Buffer.contents buf
