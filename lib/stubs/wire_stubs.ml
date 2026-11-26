(** OCaml FFI stub generation for EverParse-produced C validators. *)

let ml_type_of = Wire.Private.ml_type_of
let everparse_name = Wire_3d.everparse_name

let validator_name name =
  Wire_3d.pascal_case
    (String.capitalize_ascii name
    ^ "_validate_Wire"
    ^ Wire.Private.Types.escape_3d name)

let pp_c_stub_error_handler ppf lower =
  Fmt.pf ppf
    "static void %s_err(const char *t, const char *f, const char *r,@\n" lower;
  Fmt.pf ppf
    "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {@\n";
  Fmt.pf ppf
    "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;@\n";
  Fmt.pf ppf "}@\n"

(* The validator decides whether it accepts the record, and only that: the
   OCaml side computes the error of a record it refused ([pp_ml_make]). *)
let c_stub_validate ppf ~lower ~ep ~validator =
  Fmt.pf ppf "  %sFields fields = {0};@\n" ep;
  Fmt.pf ppf
    "  uint64_t r = %s((WIRECTX *) &fields, NULL, %s_err, data, len, 0);@\n"
    validator lower

(* [off] is caller-supplied and may carry attacker-controlled data (a length or
   offset parsed from the input). A negative one is misuse and raises
   [Invalid_argument]; one past the end of the buffer is input, and the
   validator reads it as no bytes, so it refuses the record rather than
   underflowing [len] into a span past the buffer. *)
let pp_c_stub_bounds ppf ~lower =
  Fmt.pf ppf "  intnat off = Int_val(v_off);@\n";
  Fmt.pf ppf "  mlsize_t buf_len = caml_string_length(v_buf);@\n";
  Fmt.pf ppf "  if (off < 0) caml_invalid_argument(\"%s: negative offset\");@\n"
    lower;
  Fmt.pf ppf
    "  mlsize_t start = (mlsize_t) off > buf_len ? buf_len : (mlsize_t) off;@\n";
  Fmt.pf ppf "  uint8_t *data = (uint8_t *)Bytes_val(v_buf) + start;@\n";
  Fmt.pf ppf "  uint32_t len = (uint32_t)(buf_len - start);@\n"

(* The C side reads each field from the default visitor, whose member escapes C's
   keywords ([Wire.Everparse.visitor_member]). *)
let pp_field_value ppf (fname, kind) =
  let fname = Wire.Everparse.visitor_member fname in
  match kind with
  | Wire.Everparse.Raw.Int64 ->
      Fmt.pf ppf "caml_copy_int64((int64_t) fields.%s)" fname
  | Float32 | Float64 ->
      (* The 3D visitor stores floats with their typed C type ([float] /
         [double]) and bit-reinterprets in the setter, so we hand the
         value straight to [caml_copy_double]. *)
      Fmt.pf ppf "caml_copy_double((double) fields.%s)" fname
  | _ -> Fmt.pf ppf "Val_long(fields.%s)" fname

let pp_c_stub_output ppf ~lower ~ep ~validator (s : Wire.Everparse.Raw.struct_)
    =
  let kinds = Wire.Everparse.Raw.field_kinds s in
  let n_fields = List.length kinds in
  (* Single C entry point per schema: [caml_wire_<name>_parse_k] takes a
     continuation and applies it to the parsed field values via
     [caml_callbackN]. The OCaml side derives the record-returning [parse]
     from [parse_k] with a constructor continuation (see [pp_ml_body] below);
     both share this one C function. *)
  if n_fields > 0 then begin
    Fmt.pf ppf
      "CAMLprim value caml_wire_%s_parse_k(value v_k, value v_buf, value \
       v_off) {@\n"
      lower;
    Fmt.pf ppf "  CAMLparam3(v_k, v_buf, v_off);@\n";
    Fmt.pf ppf "  CAMLlocal2(v_result, v_ok);@\n";
    (* [args] collects boxed values ([caml_copy_int64], [caml_copy_double]).
       Each of those allocations can trigger a minor collection that moves or
       reclaims the boxes already stored, and a bare [value args[N]] is
       invisible to the GC, so the earlier slots go stale and the continuation
       receives garbage field values. [CAMLlocalN] registers the array as a
       root block and pre-fills it with [Val_unit], so every slot is a valid
       root from the declaration on. It comes after the [CAMLparam3] /
       [CAMLlocal2] declarations and before the first allocation, and the
       single [CAMLreturn] unwinds all three root blocks. *)
    Fmt.pf ppf "  CAMLlocalN(args, %d);@\n" n_fields;
    pp_c_stub_bounds ppf ~lower;
    c_stub_validate ppf ~lower ~ep ~validator;
    (* [Some v] on a record the validator accepted, [None] otherwise. *)
    Fmt.pf ppf "  if (!EverParseIsSuccess(r)) CAMLreturn(Val_none);@\n";
    List.iteri
      (fun i kind -> Fmt.pf ppf "  args[%d] = %a;@\n" i pp_field_value kind)
      kinds;
    Fmt.pf ppf "  v_ok = caml_callbackN(v_k, %d, args);@\n" n_fields;
    Fmt.pf ppf "  v_result = caml_alloc_small(1, 0);@\n";
    Fmt.pf ppf "  Field(v_result, 0) = v_ok;@\n";
    Fmt.pf ppf "  CAMLreturn(v_result);@\n";
    Fmt.pf ppf "}@\n@\n"
  end
  else begin
    (* Zero-field schema: no callback, the validator's verdict. *)
    Fmt.pf ppf
      "CAMLprim value caml_wire_%s_parse(value v_buf, value v_off) {@\n" lower;
    Fmt.pf ppf "  CAMLparam2(v_buf, v_off);@\n";
    pp_c_stub_bounds ppf ~lower;
    c_stub_validate ppf ~lower ~ep ~validator;
    Fmt.pf ppf "  CAMLreturn(Val_bool(EverParseIsSuccess(r)));@\n";
    Fmt.pf ppf "}@\n@\n"
  end

(* A codec's name, capitalised, is the OCaml module of its stubs, and,
   lower-cased, their C symbol: [foo] and [FOO] would define both twice. *)
let module_name s =
  let name = Wire.Everparse.Raw.struct_name s in
  let letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false in
  if Wire.Private.Names.is_identifier name && letter name.[0] then
    String.capitalize_ascii name
  else
    Fmt.invalid_arg
      "Wire_stubs: codec %S names no OCaml module; start its name with a letter"
      name

let check_codec_names structs =
  List.iter (fun s -> ignore (module_name s : string)) structs;
  match
    Wire.Private.Names.clash
      (List.map
         (fun s ->
           let name = Wire.Everparse.Raw.struct_name s in
           (name, String.lowercase_ascii name))
         structs)
  with
  | None -> ()
  | Some (a, b, name) ->
      Fmt.invalid_arg
        "Wire_stubs: codecs %S and %S both become the stub name %S; rename one \
         of them"
        a b name

let pp_c_stub ppf (s : Wire.Everparse.Raw.struct_) =
  let name = Wire.Everparse.Raw.struct_name s in
  let ep = everparse_name name in
  let validator = validator_name name in
  let lower = String.lowercase_ascii name in
  pp_c_stub_error_handler ppf lower;
  pp_c_stub_output ppf ~lower ~ep ~validator s

let to_c_stubs (structs : Wire.Everparse.Raw.struct_ list) =
  check_codec_names structs;
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
  (* Headers only. Each codec's validator ([<Name>.c]) and [<Name>_Fields.c]
     visitor is compiled as its own translation unit and linked in (see
     [build_codec_archive]), so the per-codec shared types (an [enum], a
     synthesised [_RefByte_*] or [Se_*] element struct) stay confined to that
     unit and any set of codecs links without colliding on a duplicate
     definition. *)
  Fmt.pf ppf "/* EverParse headers + default <Name>_Fields visitor */@\n";
  List.iteri
    (fun i (s : Wire.Everparse.Raw.struct_) ->
      let base = String.capitalize_ascii (Wire.Everparse.Raw.struct_name s) in
      if i = 0 then Fmt.pf ppf "#include \"EverParse.h\"@\n";
      Fmt.pf ppf "#include \"%s_Fields.h\"@\n" base;
      Fmt.pf ppf "#include \"%s.h\"@\n" base)
    structs;
  Fmt.pf ppf "@\n/* Stubs */@\n";
  List.iter (fun s -> pp_c_stub ppf s) structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Compile each generated validator and [_Fields] visitor in [schema_dir] as its
   own translation unit and archive them into [archive] (a [lib<name>.a], so a
   dune [(foreign_archives <name>)] links it). [wire_ffi.c] (see [to_c_stubs])
   includes only the headers and calls the validators across this link, so the
   per-codec shared types stay translation-unit-local and any set of codecs
   links. This runs at generation time, alongside the EverParse invocation that
   produced the [.c]; the [cc]/[ar] calls mirror how [Wire_3d.run_everparse]
   shells out to [3d.exe], keeping the build rule free of inline shell.
   [test.c] (it has a [main]) and [<Name>Wrapper.c] (it needs a user-supplied
   [<Name>EverParseError] the FFI path never provides) are skipped. *)
let build_codec_archive ?(cflags = []) ~schema_dir ~archive () =
  let is_codec_unit f =
    Filename.check_suffix f ".c"
    && f <> "test.c"
    && not (Filename.check_suffix f "Wrapper.c")
  in
  let units =
    Sys.readdir schema_dir |> Array.to_list |> List.filter is_codec_unit
    |> List.sort compare
  in
  let object_of c =
    let obj = Filename.remove_extension c ^ ".codec.o" in
    let cmd =
      Fmt.str "cc -c -D_DEFAULT_SOURCE%s -I %s %s -o %s"
        (String.concat "" (List.map (fun f -> " " ^ Filename.quote f) cflags))
        (Filename.quote schema_dir)
        (Filename.quote (Filename.concat schema_dir c))
        (Filename.quote obj)
    in
    if Sys.command cmd <> 0 then Fmt.failwith "wire_stubs: cc failed on %s" c;
    obj
  in
  let objects = List.map object_of units in
  let cmd =
    Fmt.str "ar rcs %s %s" (Filename.quote archive)
      (String.concat " " (List.map Filename.quote objects))
  in
  if Sys.command cmd <> 0 then failwith "wire_stubs: ar failed";
  List.iter (fun o -> try Sys.remove o with Sys_error _ -> ()) objects

(* A record label escapes the keywords of OCaml and of OxCaml, so the stubs
   compile on either compiler. *)
let ml_field_name name = Wire.Private.Names.ocaml_label name

(* Lower-casing is many-to-one: [X] and [x] are one label; escaping is not,
   [done] and [done_] are [done_] and [done__]. Refuse naming both fields
   rather than emit a record that declares a label twice. *)
let check_labels (s : Wire.Everparse.Raw.struct_) kinds =
  match
    Wire.Private.Names.clash
      (List.map (fun (name, _) -> (name, ml_field_name name)) kinds)
  with
  | None -> ()
  | Some (a, b, label) ->
      Fmt.invalid_arg
        "Wire_stubs: in %S, fields %S and %S both become the record label %S; \
         rename one of them"
        (Wire.Everparse.Raw.struct_name s)
        a b label

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

(* The continuation [parse] passes to [parse_k]: the record constructor, so
   the record is built directly from the field values. *)
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

(* The type of the functor's parameter, the codec. The signature and the
   structure both name it, so that the [codec] it declares is used. *)
let pp_ml_codec_type ppf =
  Fmt.pf ppf
    "module type CODEC = sig@\n\
    \  type r@\n\
     @\n\
    \  val codec : r Wire.Codec.t@\n\
     end@\n\
     @\n"

(* The interface of the stubs of a codec: its record type [t], when it has
   fields, the type [CODEC] of the codec value and the functor over it. *)
let pp_ml_sig ppf kinds =
  if kinds <> [] then gen_ml_record ppf ~type_name:"t" kinds;
  pp_ml_codec_type ppf;
  Fmt.pf ppf "module Make (_ : CODEC) : sig@\n";
  if kinds <> [] then begin
    Fmt.pf ppf
      "  val parse_k : %a -> bytes -> int -> ('r, Wire.Error.t) result@\n"
      pp_ml_k_type kinds;
    Fmt.pf ppf
      "  (** [parse_k k buf off] applies [k] to the fields of the record at \
       [off]@\n\
      \      in [buf], or answers the error [Wire.Codec.validate] answers for \
       the@\n\
      \      codec, [buf] and [off]. Raises [Invalid_argument] if [off] is@\n\
      \      negative. *)@\n\
       @\n";
    Fmt.pf ppf "  val parse : bytes -> int -> (t, Wire.Error.t) result@\n";
    Fmt.pf ppf "  (** [parse] is [parse_k] building the record. *)@\n"
  end
  else begin
    Fmt.pf ppf "  val parse : bytes -> int -> (unit, Wire.Error.t) result@\n";
    Fmt.pf ppf
      "  (** [parse buf off] checks the record at [off] in [buf], or answers \
       the@\n\
      \      error [Wire.Codec.validate] answers for the codec, [buf] and \
       [off].@\n\
      \      Raises [Invalid_argument] if [off] is negative. *)@\n"
  end;
  Fmt.pf ppf "end@\n"

(* The stubs of a codec. The C entry point only accepts or refuses, which is
   the verified fast path; the error of a refused record is the one
   [Wire.Codec.validate] answers for the codec the functor takes, computed out
   of line. [digest], when the codec is known, checks once, when the functor is
   applied, that the codec is the one the stubs come from. *)
let pp_ml_body ppf ~digest s kinds =
  let symbol = String.lowercase_ascii (Wire.Everparse.Raw.struct_name s) in
  if kinds <> [] then begin
    gen_ml_record ppf ~type_name:"t" kinds;
    Fmt.pf ppf
      "external validate_k : %a -> bytes -> int -> 'r option@\n\
      \  = \"caml_wire_%s_parse_k\"@\n\
       @\n"
      pp_ml_k_type kinds symbol
  end
  else
    Fmt.pf ppf
      "external validate : bytes -> int -> bool = \"caml_wire_%s_parse\"@\n@\n"
      symbol;
  pp_ml_codec_type ppf;
  Fmt.pf ppf "module Make (C : CODEC) = struct@\n";
  Option.iter
    (fun d -> Fmt.pf ppf "  let () = Wire.Private.Gen.check C.codec %S@\n@\n" d)
    digest;
  (* The validator refused the record. Where the codec accepts it, the two
     disagree, a defect of one of them; the record is then refused as a whole,
     as a [~where] that fails. *)
  Fmt.pf ppf
    "  let[@@inline never] error buf off =@\n\
    \    match Wire.Codec.validate C.codec buf off with@\n\
    \    | Error e -> e@\n\
    \    | Ok () ->@\n\
    \        {@\n\
    \          Wire.Error.at = off;@\n\
    \          field = [];@\n\
    \          kind = Constraint_failed { which = Where; value = None };@\n\
    \        }@\n\
     @\n";
  if kinds <> [] then begin
    Fmt.pf ppf
      "  let parse_k k buf off =@\n\
      \    match validate_k k buf off with@\n\
      \    | Some v -> Ok v@\n\
      \    | None -> Error (error buf off)@\n\
       @\n";
    Fmt.pf ppf "  let parse buf off = parse_k %a buf off@\n"
      pp_ml_record_constructor kinds
  end
  else
    Fmt.pf ppf
      "  let parse buf off =@\n\
      \    if validate buf off then Ok () else Error (error buf off)@\n";
  Fmt.pf ppf "end@\n"

(* [text] without its final newline, each line indented. *)
let indent text =
  let text =
    if String.ends_with ~suffix:"\n" text then
      String.sub text 0 (String.length text - 1)
    else text
  in
  String.split_on_char '\n' text
  |> List.map (fun l -> if l = "" then l else "  " ^ l)
  |> String.concat "\n"

let to_string pp =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let ml_stubs entries =
  check_codec_names (List.map fst entries);
  to_string (fun ppf ->
      Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n";
      List.iter
        (fun (s, digest) ->
          let kinds = Wire.Everparse.Raw.field_kinds s in
          check_labels s kinds;
          Fmt.pf ppf "@\nmodule %s : sig@\n%s@\nend = struct@\n%s@\nend@\n"
            (module_name s)
            (indent (to_string (fun ppf -> pp_ml_sig ppf kinds)))
            (indent (to_string (fun ppf -> pp_ml_body ppf ~digest s kinds))))
        entries)

let to_ml_stubs structs = ml_stubs (List.map (fun s -> (s, None)) structs)

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
  let kinds = Wire.Everparse.Raw.field_kinds s in
  check_labels s kinds;
  to_string (fun ppf ->
      Fmt.pf ppf
        "(* Generated by wire (do not edit) *)@\n\
         @\n\
         include (@\n\
        \  struct@\n\
         %s@\n\
        \  end :@\n\
        \  sig@\n\
         %s@\n\
        \  end)@\n"
        (indent
           (indent (to_string (fun ppf -> pp_ml_body ppf ~digest:None s kinds))))
        (indent (indent (to_string (fun ppf -> pp_ml_sig ppf kinds)))))

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

type packed_codec = C : _ Wire.Codec.t -> packed_codec

let write ~schema_dir ~outdir entries =
  let structs = List.map fst entries in
  let schemas =
    List.map (Wire.Everparse.Raw.project_struct ~mode:`Ffi) structs
  in
  Wire_3d.write_external_typedefs ~outdir:schema_dir schemas;
  Wire_3d.write_fields ~outdir:schema_dir schemas;
  write_file (Filename.concat outdir "wire_ffi.c") (to_c_stubs structs);
  write_file (Filename.concat outdir "stubs.ml") (ml_stubs entries)

let of_structs ~schema_dir ~outdir structs =
  write ~schema_dir ~outdir (List.map (fun s -> (s, None)) structs)

let check_names s =
  let schema = Wire.Everparse.Raw.project_struct ~mode:`Ffi s in
  ignore (Wire.Everparse.Raw.to_3d schema.module_ : string);
  Wire_3d.check_visitor_names schema;
  check_labels s (Wire.Everparse.Raw.field_kinds s)

let generate ~schema_dir ~outdir codecs =
  write ~schema_dir ~outdir
    (List.map
       (fun (C c) ->
         (Wire.Everparse.Raw.struct_of_codec c, Some (Wire.Private.Gen.digest c)))
       codecs)
