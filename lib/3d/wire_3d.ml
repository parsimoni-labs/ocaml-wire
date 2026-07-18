(** Generate verified C libraries from Wire codecs via EverParse. *)

open Wire.Everparse

let is_upper c = Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c

(* Apply EverParse's normalization to a single identifier segment (no
   underscores): every run of two or more consecutive uppercase letters keeps
   its first letter and lowercases the rest, wherever it occurs in the segment
   ([SSID -> Ssid], [TMFrame -> Tmframe], [SpaceOSFrame -> SpaceOsframe]); a lone
   uppercase letter (a camelCase word boundary) is left alone. *)
let normalize_segment seg =
  let len = String.length seg in
  let b = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if is_upper seg.[!i] then begin
      let j = ref !i in
      while !j < len && is_upper seg.[!j] do
        incr j
      done;
      Buffer.add_char b seg.[!i];
      if !j - !i >= 2 then
        for k = !i + 1 to !j - 1 do
          Buffer.add_char b (Char.lowercase_ascii seg.[k])
        done;
      i := !j
    end
    else begin
      Buffer.add_char b seg.[!i];
      incr i
    end
  done;
  Buffer.contents b

(* EverParse strips underscores when generating a C identifier from a name and
   joins the [_]-separated segments into CamelCase: each segment is capitalized
   and any internal run of two or more uppercase letters collapses to one.
   [Grpc_message -> GrpcMessage], [EP_Header -> EpHeader],
   [MC_Status_Reply -> McStatusReply], [SSID -> Ssid]. wire writes the .3d file
   and type with a capitalized name, so the leading segment is normally already
   capitalized; capitalizing it here keeps the identifier matching EverParse even
   for an otherwise lower-case name. *)
let everparse_name name =
  String.split_on_char '_' name
  |> List.map (fun seg -> String.capitalize_ascii (normalize_segment seg))
  |> String.concat ""

(* EverParse's own identifier mangling, transcribed from [pascal_case] in
   [src/3d/Target.fst]. It drops underscores and CamelCases, but unlike
   {!everparse_name} it lowercases every character that follows an uppercase
   letter *or a digit* until the next lower-case letter (a digit counts as
   uppercase, since [uppercase '2' = '2']). So [TPM2B -> Tpm2b] and
   [Foo2Bar -> Foo2bar], not [Tpm2B] / [Foo2Bar]. EverParse builds the wrapper
   symbol as [pascal_case (module ^ "_check_" ^ codec)], so this must match it
   exactly or the differential [agree.c] links against a symbol that does not
   exist. *)
let pascal_case name =
  if not (String.contains name '_') then String.capitalize_ascii name
  else begin
    let keep = 0 and up = 1 and low = 2 in
    let what_next = ref up in
    let b = Buffer.create (String.length name) in
    String.iter
      (fun c ->
        if c = '_' then what_next := up
        else begin
          if !what_next = keep then Buffer.add_char b c
          else if !what_next = up then
            Buffer.add_char b (Char.uppercase_ascii c)
          else Buffer.add_char b (Char.lowercase_ascii c);
          if Char.uppercase_ascii c = c then what_next := low
          else if Char.lowercase_ascii c = c then what_next := keep
        end)
      name;
    Buffer.contents b
  end

(* EverParse derives the C output filename from the [.3d] filename, which
   [Wire.Everparse.filename] already writes with [String.capitalize_ascii].
   All filenames wire.3d emits or references must go through the same
   capitalization so dune targets match the files EverParse actually
   produces. Identifiers inside C code -- the [<Name>Set*] setters and the
   typed struct name -- use [everparse_name], which also strips underscores
   and CamelCases segments ([rpmsg_endpoint_info] -> [RpmsgEndpointInfo]).
   Keep the two concerns separate: [file_base] for filenames, [c_ident] for
   C identifiers. *)
let file_base (s : t) = String.capitalize_ascii s.name
let c_ident (s : t) = everparse_name s.name

(* EverParse normalizes extern callback names in ways that are awkward to
   mirror exactly (runs of uppercase after a digit get lowercased, trailing
   uppercase runs get lowercased, ...). Rather than re-implement EverParse's
   rule and drift from it over time, we read the normalized names straight
   out of the [_ExternalAPI.h] file EverParse has just generated. *)
let read_extern_names ~outdir s =
  let path = Filename.concat outdir (file_base s ^ "_ExternalAPI.h") in
  let ic = open_in path in
  let names = ref [] in
  (try
     while true do
       let line = input_line ic in
       match
         ( String.index_opt line '(',
           String.index_opt line ' ',
           String.length line )
       with
       | Some lp, _, _ when String.length line >= 11 ->
           let prefix = "extern void " in
           let plen = String.length prefix in
           if
             String.length line > plen
             && String.sub line 0 plen = prefix
             && lp > plen
           then
             let name = String.sub line plen (lp - plen) in
             names := name :: !names
       | _ -> ()
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !names

(* EverParse's top-level validator function follows its own normalization
   rule (different from extern callbacks: it preserves underscores when the
   name doesn't start with 2+ uppercase, strips them when it does). Rather
   than duplicate EverParse's logic, extract the actual name from the
   generated [<Name>.h]: [uint64_t <Name>Validate<Name>(...)]. *)
let read_validate_name ~outdir s =
  let path = Filename.concat outdir (file_base s ^ ".h") in
  let ic = open_in path in
  let found = ref None in
  let needle = "Validate" in
  let nlen = String.length needle in
  let is_ident c =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  (* EverParse declares the validator as [<Name>Validate<Name>(...)]. Find the
     [Validate] keyword and return the C identifier immediately before it -- the
     base [<Name>]. Scanning every position (not just the first 'V') lets the
     name itself contain a 'V' (e.g. "VeritySuperblock"); anchoring on the
     identifier before [Validate] also drops a leading return type should
     EverParse ever emit it on the same line. *)
  let base_before_validate line =
    let len = String.length line in
    let rec scan i =
      if i + nlen > len then None
      else if i > 0 && is_ident line.[i - 1] && String.sub line i nlen = needle
      then begin
        let j = ref i in
        while !j > 0 && is_ident line.[!j - 1] do
          decr j
        done;
        Some (String.sub line !j (i - !j))
      end
      else scan (i + 1)
    in
    scan 0
  in
  (try
     while !found = None do
       let line = String.trim (input_line ic) in
       found := base_before_validate line
     done
   with End_of_file -> ());
  close_in ic;
  match !found with
  | Some n -> n
  | None -> Fmt.failwith "could not find Validate function name in %s" path

let write_3d ~outdir schemas = Wire.Everparse.write ~mode:`Ffi ~outdir schemas

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let oc = open_out_bin dst in
  output_bytes oc buf;
  close_out oc

let locate_3d_exe () =
  let ic = Unix.open_process_in "command -v 3d.exe 2>/dev/null" in
  let path = try Some (input_line ic) with End_of_file -> None in
  ignore (Unix.close_process_in ic);
  match path with
  | Some p -> Some p
  | None ->
      let local =
        Filename.concat (Sys.getenv "HOME") ".local/everparse/bin/3d.exe"
      in
      if Sys.file_exists local then Some local else None

let everparse_dir () =
  match locate_3d_exe () with
  | Some exe -> Filename.dirname exe |> Filename.dirname
  | None -> failwith "3d.exe not found"

let copy_everparse_endianness ~outdir =
  let dst = Filename.concat outdir "EverParseEndianness.h" in
  if not (Sys.file_exists dst) then begin
    let ep_dir = everparse_dir () in
    let src = Filename.concat ep_dir "src/3d/EverParseEndianness.h" in
    if Sys.file_exists src then copy_file ~src ~dst
    else Fmt.failwith "Cannot find EverParseEndianness.h at %s" src
  end

let has_3d_exe () = locate_3d_exe () <> None

(* The [_ExternalTypedefs.h] header seen by the EverParse-generated validator
   and wrapper. The default shipped by wire.3d is a forward declaration that
   ties WIRECTX to the matching [<Name>_Fields] plug struct. Users who want
   their own plug (e.g. {!Wire_stubs} for OCaml FFI) overwrite this file with
   a different WIRECTX definition; they must then also omit the default
   [<Name>_Fields.c] from their link. *)
let write_external_typedefs ~outdir schemas =
  List.iter
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then begin
        let path =
          Filename.concat outdir (file_base s ^ "_ExternalTypedefs.h")
        in
        let oc = open_out path in
        Fmt.pf
          (Format.formatter_of_out_channel oc)
          "#ifndef WIRECTX_DEFINED@\n\
           #define WIRECTX_DEFINED@\n\
           typedef struct %sFields WIRECTX;@\n\
           #endif@\n"
          (c_ident s);
        close_out oc
      end)
    schemas

(* Typed-struct plug: one C struct per schema with one member per named field,
   plus a [WireSet*] implementation that switches on idx to populate it. *)
let write_fields_header ~outdir s =
  let fields = Wire.Everparse.plug_fields s in
  let base = file_base s in
  let ident = c_ident s in
  let path = Filename.concat outdir (base ^ "_Fields.h") in
  let oc = open_out path in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  let guard =
    String.uppercase_ascii ident ^ "_FIELDS_H" |> fun g ->
    String.map (fun c -> if c = '-' then '_' else c) g
  in
  let prefix =
    String.uppercase_ascii ident |> fun p ->
    String.map (fun c -> if c = '-' then '_' else c) p
  in
  pr "#ifndef %s@\n" guard;
  pr "#define %s@\n" guard;
  pr "#include <stdint.h>@\n@\n";
  pr "/* Field indices -- use with the schema's WireSet* callbacks in a@\n";
  pr "   custom [WIRECTX] if you only want to capture a subset. */@\n";
  List.iter
    (fun f ->
      pr "#define %s_IDX_%s %d@\n" prefix
        (String.uppercase_ascii f.Wire.Everparse.name)
        f.idx)
    fields;
  if fields <> [] then pr "@\n";
  pr "/* Default plug: one typed member per named field. Pass a pointer to@\n";
  pr "   [%sFields] as [WIRECTX *] when you want every field populated. */@\n"
    ident;
  pr "typedef struct %sFields {@\n" ident;
  List.iter (fun f -> pr "  %s %s;@\n" f.Wire.Everparse.c_type f.name) fields;
  if fields = [] then pr "  int _unused;@\n";
  pr "} %sFields;@\n" ident;
  pr "#endif@\n";
  Format.pp_print_flush ppf ();
  close_out oc

(* Emit one [case N:] body inside a [WireSet*] setter. [float] / [double]
   fields get a [memcpy] bit-reinterpret because the parser hands us the
   underlying [UINT32] / [UINT64] but the plug struct stores the typed
   float; everyone else takes a value cast. *)
let emit_setter_case ppf logical f =
  if String.equal f.Wire.Everparse.setter logical then
    match f.c_type with
    | "float" | "double" ->
        Fmt.pf ppf
          "    case %d: { %s _x; memcpy(&_x, &v, sizeof _x); f->%s = _x; \
           break; }@\n"
          f.idx f.c_type f.name
    | _ ->
        Fmt.pf ppf "    case %d: f->%s = (%s) v; break;@\n" f.idx f.name
          f.c_type

let write_fields_impl ~outdir s =
  let fields = Wire.Everparse.plug_fields s in
  let setters = Wire.Everparse.plug_setters s in
  let base = file_base s in
  let ident = c_ident s in
  (* EverParse renames some setter symbols when emitting [.c] (for example,
     uppercase runs after a digit are lowercased). Read the actual symbol
     names from the just-generated [_ExternalAPI.h] rather than re-deriving
     them. Order matches the declaration order of the extern functions,
     which matches [plug_setters]. *)
  let physical_names = read_extern_names ~outdir s in
  let path = Filename.concat outdir (base ^ "_Fields.c") in
  let oc = open_out path in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "#include <stdint.h>@\n";
  pr "#include <string.h>@\n";
  pr "#include \"%s_Fields.h\"@\n" base;
  pr "#include \"%s_ExternalTypedefs.h\"@\n" base;
  pr "#include \"%s_ExternalAPI.h\"@\n@\n" base;
  (* Cast [WIRECTX *] to the schema's concrete struct type. In a translation
     unit that includes multiple schemas' [_Fields.c] files, only the first
     [_ExternalTypedefs.h] defines [WIRECTX]; subsequent headers are skipped
     by the include guard. The explicit cast to [<Name>Fields *] makes each
     setter work regardless of which schema's typedef won. *)
  List.iter2
    (fun (logical, val_c_type) physical ->
      pr "void %s(WIRECTX *ctx, uint32_t idx, %s v) {@\n" physical val_c_type;
      pr "  %sFields *f = (%sFields *) ctx;@\n" ident ident;
      pr "  switch (idx) {@\n";
      List.iter (fun f -> emit_setter_case ppf logical f) fields;
      pr "    default: (void) f; (void) v; break;@\n";
      pr "  }@\n";
      pr "}@\n@\n")
    setters physical_names;
  Format.pp_print_flush ppf ();
  close_out oc

let write_fields ~outdir schemas =
  List.iter
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then begin
        write_fields_header ~outdir s;
        write_fields_impl ~outdir s
      end)
    schemas

(* Files shipped with a schema whose validator depends on the WireCtx contract:
   the forward-decl header, the EverParse-emitted API / wrapper, and the
   default plug pair. Every file in this list is installed and accounted for
   in the dune rules; wrapper artefacts are only needed at install time,
   while [_Fields.{c,h}] also link into the runtest. *)
let wire_ctx_files schemas =
  List.concat_map
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then
        let base = file_base s in
        [
          base ^ "_ExternalTypedefs.h";
          base ^ "_ExternalAPI.h";
          base ^ "Wrapper.c";
          base ^ "Wrapper.h";
          base ^ "_Fields.h";
          base ^ "_Fields.c";
        ]
      else [])
    schemas

let fields_c_files schemas =
  List.filter_map
    (fun s ->
      if Wire.Everparse.uses_wire_ctx s then Some (file_base s ^ "_Fields.c")
      else None)
    schemas

(* EverParse's [<Base>Check<Codec>] wrapper returns TRUE on any successful
   validator result, but a success result is the consumed position: a valid
   record followed by trailing bytes still returns TRUE, making the wrapper a
   prefix recognizer rather than a validator. Wire's contract is whole-buffer
   validation, so rewrite each success tail to also require full consumption.
   Textual on EverParse's wrapper shape (the tail is identical for
   parameterized and plain entrypoints, [print_c_entry] in the upstream
   [src/3d/Target.fst]): if a future EverParse emits neither the known tail
   nor its own consumption check, fail loudly rather than silently shipping a
   prefix recognizer. The behavioural backstop is the differential runtest,
   whose corpus includes over-length inputs the oracle rejects. *)
let wrapper_success_tail = "\t\treturn FALSE;\n\t}\n\treturn TRUE;\n}"
let wrapper_consumption_check = "result != (uint64_t) len"

let wrapper_hardened_tail =
  "\t\treturn FALSE;\n\
   \t}\n\
   \tif (result != (uint64_t) len)\n\
   \t{\n\
   \t\treturn FALSE;\n\
   \t}\n\
   \treturn TRUE;\n\
   }"

let harden_wrapper ~outdir base =
  let path = Filename.concat outdir (base ^ "Wrapper.c") in
  if Sys.file_exists path then begin
    let src = In_channel.with_open_text path In_channel.input_all in
    let tail = Re.compile (Re.str wrapper_success_tail) in
    if Re.execp tail src then
      Out_channel.with_open_text path (fun oc ->
          Out_channel.output_string oc
            (Re.replace_string tail ~by:wrapper_hardened_tail src))
    else if not (Re.execp (Re.compile (Re.str wrapper_consumption_check)) src)
    then
      Fmt.failwith
        "%s: unrecognized EverParse wrapper shape; cannot insert the \
         full-consumption check"
        path
  end

let run_everparse_files ?(quiet = true) ~outdir files =
  let exe =
    match locate_3d_exe () with
    | Some e -> e
    | None -> failwith "3d.exe not found in PATH or ~/.local/everparse/bin/"
  in
  List.iter
    (fun f ->
      let redirect = if quiet then " > /dev/null 2>&1" else "" in
      let cmd = Fmt.str "cd %s && %s --batch %s%s" outdir exe f redirect in
      let ret = Sys.command cmd in
      if ret <> 0 then Fmt.failwith "EverParse failed on %s with code %d" f ret;
      harden_wrapper ~outdir (Filename.remove_extension (Filename.basename f)))
    files;
  copy_everparse_endianness ~outdir

let run_everparse ?(quiet = true) ~outdir schemas =
  run_everparse_files ~quiet ~outdir (List.map Wire.Everparse.filename schemas)

let parse_3d ?(batch = false) ~outdir file =
  let exe =
    match locate_3d_exe () with
    | Some e -> e
    | None -> failwith "3d.exe not found in PATH or ~/.local/everparse/bin/"
  in
  let log_path = Filename.temp_file "wire_parse_3d" ".log" in
  (* 3d.exe emits diagnostics to stdout, so capture both streams. *)
  let flag = if batch then "--batch " else "" in
  let cmd =
    Fmt.str "cd %s && %s %s%s > %s 2>&1" outdir exe flag file log_path
  in
  let ret = Sys.command cmd in
  let captured =
    try In_channel.with_open_text log_path In_channel.input_all
    with Sys_error _ -> ""
  in
  (try Sys.remove log_path with Sys_error _ -> ());
  if ret = 0 then Ok ()
  else
    let msg =
      String.split_on_char '\n' captured
      |> List.filter (fun l ->
          let l = String.trim l in
          l <> ""
          && not (String.length l >= 11 && String.sub l 0 11 = "Processing "))
      |> String.concat "\n"
    in
    Error (if msg = "" then Fmt.str "exit %d" ret else msg)

let emit_sanity_check ppf ~name ~ep ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  (* Sanity: the OCaml codec's wire_size must match what the EverParse
     validator consumes. A mismatch means the .3d projection of the codec
     packs to a different size than the codec declares -- almost always a
     bug in the codec's bitfield declarations. Later checks are meaningless
     if this fails, so abort the whole test binary with a clear message. *)
  pr "    r = %sValidate%s(%sNULL, counting_error_handler, buf, %d, 0);\n" ep ep
    ctx_arg wire_size;
  pr "    if (!EverParseIsSuccess(r) || r != %d) {\n" wire_size;
  pr "      fprintf(stderr,\n";
  pr "        \"FATAL: %s wire_size mismatch -- codec declared %d bytes, \"\n"
    name wire_size;
  pr "        \"EverParse validator returned %%llu. Fix the OCaml codec's \"\n";
  pr "        \"wire_size or the .3d projection.\\n\",\n";
  pr "        (unsigned long long) r);\n";
  pr "      return 2;\n";
  pr "    }\n"

let emit_truncation_checks ppf ~ep ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  pr "    r = %sValidate%s(%sNULL, counting_error_handler, buf, %d, 0);\n" ep ep
    ctx_arg (wire_size * 2);
  pr "    CHECK(\"larger buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position is %d not %d\", r == %d);\n" wire_size
    (wire_size * 2) wire_size;
  pr "\n";
  pr "    for (uint64_t len = 0; len < %d; len++) {\n" wire_size;
  pr "      error_count = 0;\n";
  pr "      r = %sValidate%s(%sNULL, counting_error_handler, buf, len, 0);\n" ep
    ep ctx_arg;
  pr "      CHECK(\"truncated to len fails\", EverParseIsError(r));\n";
  pr "    }\n";
  pr "\n";
  pr "    r = %sValidate%s(%sNULL, counting_error_handler, buf, 0, 0);\n" ep ep
    ctx_arg;
  pr "    CHECK(\"empty input fails\", EverParseIsError(r));\n"

let emit_random_checks ppf ~ep ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  pr "    srand(42);\n";
  pr "    for (int i = 0; i < 1000; i++) {\n";
  pr "      for (int j = 0; j < %d; j++)\n" wire_size;
  pr "        buf[j] = (uint8_t)(rand() & 0xff);\n";
  pr "      r = %sValidate%s(%sNULL, counting_error_handler, buf, %d, 0);\n" ep
    ep ctx_arg wire_size;
  pr "      CHECK(\"random buffer validates\", EverParseIsSuccess(r));\n";
  pr "      CHECK(\"random position correct\", r == %d);\n" wire_size;
  pr "    }\n"

let emit_schema_test ?outdir ppf s wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  (* Read the validator name straight out of EverParse's generated [.h]
     -- the one authoritative source. EverParse applies its own naming
     rules (different for the top-level Validate function vs. the extern
     callbacks); any attempt to re-implement them here has drifted before. *)
  let ep =
    match outdir with
    | Some dir -> read_validate_name ~outdir:dir s
    | None -> file_base s
  in
  let lower = String.lowercase_ascii s.name in
  let uses_ctx = Wire.Everparse.uses_wire_ctx s in
  let ctx_arg = if uses_ctx then "(WIRECTX *) &ctx, " else "" in
  pr "\n  /* %s (%d bytes) */\n" s.name wire_size;
  pr "  {\n";
  pr "    int pass = 0, fail = 0;\n";
  pr "    uint8_t buf[%d];\n" wire_size;
  pr "    uint64_t r;\n";
  if uses_ctx then pr "    %sFields ctx = {0};\n" (c_ident s);
  pr "\n";
  pr "    memset(buf, 0, %d);\n" wire_size;
  emit_sanity_check ppf ~name:s.name ~ep ~ctx_arg wire_size;
  pr "    CHECK(\"zero buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position advanced to %d\", r == %d);\n" wire_size wire_size;
  pr "\n";
  emit_truncation_checks ppf ~ep ~ctx_arg wire_size;
  pr "\n";
  emit_random_checks ppf ~ep ~ctx_arg wire_size;
  pr "\n";
  if uses_ctx then pr "    (void) ctx;\n";
  pr "    printf(\"%s: %%d passed, %%d failed\\n\", pass, fail);\n" lower;
  pr "    failures += fail;\n";
  pr "  }\n"

let generate_test ~outdir schemas =
  let oc = open_out (Filename.concat outdir "test.c") in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "#include <stdio.h>\n";
  pr "#include <stdlib.h>\n";
  pr "#include <stdint.h>\n";
  pr "#include <string.h>\n";
  pr "#include \"EverParse.h\"\n";
  let fixed_schemas =
    List.filter_map
      (fun s -> Option.map (fun ws -> (s, ws)) s.wire_size)
      schemas
  in
  List.iter
    (fun (s, _) ->
      let base = file_base s in
      pr "#include \"%s.h\"\n" base;
      if Wire.Everparse.uses_wire_ctx s then
        pr "#include \"%s_Fields.h\"\n" base)
    fixed_schemas;
  (* counting_error_handler is only referenced from the per-schema test
     blocks emit_schema_test emits, which run only for fixed-size schemas.
     Skip it entirely when there are none, otherwise -Wunused-function
     under strict flags rejects the file. *)
  if fixed_schemas <> [] then begin
    pr "\nstatic int error_count;\n\n";
    pr "static void counting_error_handler(\n";
    pr "  EVERPARSE_STRING t, EVERPARSE_STRING f, EVERPARSE_STRING r,\n";
    pr "  uint64_t c, uint8_t *ctx, uint8_t *i, uint64_t p) {\n";
    pr "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;\n";
    pr "  error_count++;\n";
    pr "}\n\n"
  end;
  pr "#define CHECK(msg, cond) do { \\\n";
  pr "  if (cond) { pass++; } \\\n";
  pr "  else { fail++; fprintf(stderr, \"  FAIL: %%s\\n\", msg); } \\\n";
  pr "} while(0)\n\n";
  pr "int main(void) {\n";
  pr "  int failures = 0;\n";
  List.iter (fun (s, ws) -> emit_schema_test ~outdir ppf s ws) fixed_schemas;
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
  write_3d ~outdir schemas

let rm_rf dir =
  (try Sys.readdir dir with Sys_error _ -> [||])
  |> Array.iter (fun f ->
      try Sys.remove (Filename.concat dir f) with Sys_error _ -> ());
  try Sys.rmdir dir with Sys_error _ -> ()

let default_job_count () = max 1 (min 4 (Domain.recommended_domain_count ()))

(* A bounded fork pool: each job runs in its own process, so blocking EverParse
   runs ([Sys.command]) overlap across cores with full isolation. Returns each
   job's success (exit 0 and no exception) in input order. EverParse runs must
   be isolated because they race on shared intermediate files in a shared
   directory, so jobs that invoke [3d.exe] should each use a private cwd. *)
let fork_pool ~max_jobs jobs =
  let n = Array.length jobs in
  let ok = Array.make n false in
  let pid_idx = Hashtbl.create 64 in
  let next = ref 0 and running = ref 0 in
  let reap () =
    let pid, status = Unix.wait () in
    match Hashtbl.find_opt pid_idx pid with
    | Some i ->
        Hashtbl.remove pid_idx pid;
        decr running;
        ok.(i) <- (match status with Unix.WEXITED 0 -> true | _ -> false)
    | None -> ()
  in
  (* Drain any buffered output before forking, so a child does not inherit and
     re-flush the parent's pending bytes. *)
  Format.pp_print_flush Fmt.stderr ();
  Format.pp_print_flush Fmt.stdout ();
  while !next < n || !running > 0 do
    if !next < n && !running < max_jobs then begin
      let i = !next in
      incr next;
      match Unix.fork () with
      | 0 -> (
          try
            jobs.(i) ();
            Unix._exit 0
          with e ->
            Fmt.epr "%s\n%!" (Printexc.to_string e);
            Unix._exit 1)
      | pid ->
          Hashtbl.add pid_idx pid i;
          incr running
    end
    else reap ()
  done;
  ok

(* Verify every schema through EverParse, the way its own corpus is tested (one
   schema per .3d module, each accepted iff F* verifies it). The cost is
   dominated by per-module F* verification (CPU-bound, several seconds each);
   per-invocation startup is negligible, so a single [3d.exe --batch] over
   everything just verifies serially on one core. The schemas are instead
   verified concurrently in a {!fork_pool} (at most [max_jobs] at once), each
   [3d.exe] run in its own directory (concurrent runs race on EverParse's shared
   intermediate files). The pool overlaps the per-module work across cores and
   load-balances as runs finish, the only lever for this cost. [Ok ()] iff
   EverParse accepts every schema, else [Error] naming the offending schema(s)
   with their captured diagnostics. The caller provides schemas with distinct
   names (each becomes its own .3d module). *)
let batch_check ?max_jobs ~outdir schemas =
  match (locate_3d_exe (), schemas) with
  | None, _ -> Error "3d.exe not found in PATH or ~/.local/everparse/bin/"
  | Some _, [] -> Ok ()
  | Some exe, _ -> (
      ensure_dir outdir;
      let arr : t array = Array.of_list schemas in
      let log_of i = Filename.concat outdir (arr.(i).name ^ ".batchlog") in
      let jobs =
        Array.mapi
          (fun i schema () ->
            let work = Filename.temp_dir "wire_batchchk" "" in
            Fun.protect
              ~finally:(fun () -> rm_rf work)
              (fun () ->
                generate_3d ~outdir:work [ schema ];
                let cmd =
                  Fmt.str
                    "cd %s && %s --batch --no_copy_everparse_h %s > %s 2>&1"
                    work exe
                    (Wire.Everparse.filename schema)
                    (Filename.quote (log_of i))
                in
                if Sys.command cmd <> 0 then failwith "EverParse rejected"))
          arr
      in
      let max_jobs = Option.value max_jobs ~default:(default_job_count ()) in
      let ok = fork_pool ~max_jobs jobs in
      let errors =
        Array.to_list ok
        |> List.mapi (fun i passed ->
            if passed then None
            else
              let msg =
                try In_channel.with_open_text (log_of i) In_channel.input_all
                with Sys_error _ -> ""
              in
              Fmt.kstr (fun s -> Some s) "%s:\n%s" arr.(i).name msg)
        |> List.filter_map Fun.id
      in
      match errors with [] -> Ok () | _ -> Error (String.concat "\n" errors))

let generate_c ?(quiet = true) ~outdir schemas =
  ensure_dir outdir;
  if has_3d_exe () then begin
    run_everparse ~quiet ~outdir schemas;
    write_external_typedefs ~outdir schemas;
    write_fields ~outdir schemas;
    generate_test ~outdir schemas
  end
  else
    failwith
      "3d.exe not found in PATH. Install EverParse to regenerate C files."

let run ?(quiet = true) ~outdir schemas =
  generate_3d ~outdir schemas;
  generate_c ~quiet ~outdir schemas

(* Strict C11 with warnings-as-errors. [_DEFAULT_SOURCE] declares the BSD endian
   helpers (be16toh, ...) the generated C uses from <endian.h> on Linux glibc.
   [-Wextra] is deliberately omitted: its [-Wtype-limits] flags the always-true
   [0U <= unsigned] bound check EverParse emits for an optional's absent 0-byte
   case (an empty case is a 3D syntax error, so the zero-byte field is forced),
   which is not a strict-C11 violation. The generated validators compile clean
   under this set. *)
let strict_cc_flags =
  "-std=c11 -D_DEFAULT_SOURCE -Wall -Werror -Wpedantic -Wstrict-prototypes \
   -Wmissing-prototypes -Wshadow -Wcast-qual"

(* EverParse's generated wrapper types a parameterized validator's parameters
   with the 3D type name (e.g. [UINT16BE max_len]) but defines none of those
   names (the validator itself uses [uint16_t]). Map each 3D integer type to its
   host-order C type so the wrapper compiles. Harmless for non-parameterized
   wrappers, which never reference them. *)
let everparse_type_defines =
  "-DUINT8=uint8_t -DUINT16=uint16_t -DUINT16BE=uint16_t -DUINT32=uint32_t \
   -DUINT32BE=uint32_t -DUINT64=uint64_t -DUINT64BE=uint64_t"

let emit_gen_rules ppf three_d_files c_files ctx_files =
  Fmt.pf ppf
    "(rule\n\
    \ (alias 3d)\n\
    \ (mode promote)\n\
    \ (targets %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} 3d)))\n\n\
     (rule\n\
    \ (alias 3d)\n\
    \ (enabled_if\n\
    \  (= %%{env:BUILD_EVERPARSE=} \"1\"))\n\
    \ (mode promote)\n\
    \ (targets EverParse.h EverParseEndianness.h %s test.c)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} c)))\n\n"
    (String.concat " " three_d_files)
    (String.concat " " (c_files @ ctx_files))
    (String.concat " " three_d_files)

let emit_runtest_rule ppf ~test_bin ~all_deps ~c_srcs =
  Fmt.pf ppf
    "(rule\n\
    \ (targets %s)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (run cc %s -o %s test.c %s)))\n\n\
     (rule\n\
    \ (alias runtest)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (run %%{dep:%s})))\n\n"
    test_bin
    (String.concat " " all_deps)
    strict_cc_flags test_bin (String.concat " " c_srcs) test_bin test_bin

let emit_install_stanza ppf ~package ~three_d_files ~c_files ~ctx_files =
  let pr fmt = Fmt.pf ppf fmt in
  pr "(install\n (package %s)\n (section lib)\n (files\n" package;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) three_d_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) c_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) ctx_files;
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n"

let generate_dune ~outdir ~package schemas =
  let oc = open_out (Filename.concat outdir "dune.inc") in
  let ppf = Format.formatter_of_out_channel oc in
  let names = List.map file_base schemas in
  let c_files = List.concat_map (fun n -> [ n ^ ".h"; n ^ ".c" ]) names in
  let ctx_files = wire_ctx_files schemas in
  let fields_srcs = fields_c_files schemas in
  let three_d_files = List.map (fun n -> n ^ ".3d") names in
  let test_bin =
    "test_" ^ String.map (fun c -> if c = '-' then '_' else c) package
  in
  let all_deps =
    [ "test.c"; "EverParse.h"; "EverParseEndianness.h" ] @ c_files @ ctx_files
  in
  let c_srcs = List.map (fun n -> n ^ ".c") names @ fields_srcs in
  emit_gen_rules ppf three_d_files c_files ctx_files;
  emit_runtest_rule ppf ~test_bin ~all_deps ~c_srcs;
  emit_install_stanza ppf ~package ~three_d_files ~c_files ~ctx_files;
  Format.pp_print_flush ppf ();
  close_out oc

(* A codec awaiting projection. [main] and the standalone helpers take these
   rather than an already-projected [Wire.Everparse.t] so the caller never has
   to choose the projection mode: it is picked here from [main]'s [~mode], not at
   the call site, which removes any [~mode] + projection-mode mismatch. *)
type packed = Pack : 'a Wire.Codec.t -> packed

let pack c = Pack c

(* -- Documentation / single-file pipeline. [main ~mode:`Standalone] drives these:
   project each codec with [Wire.Everparse.project] (FFI-free), merge the family
   into one [<Package>.3d] with [write], and compile that single spec to a
   validator-only [<Package>.c] -- no per-codec files, no [_Fields] plug, no FFI
   stubs. The package name becomes the 3D module name, so turn it into a valid
   EverParse identifier (CamelCase, no hyphens): "my-pkg" -> "MyPkg". -- *)
let doc_module_name package =
  let alnum c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  in
  package
  |> String.map (fun c -> if alnum c then c else '_')
  |> String.split_on_char '_'
  |> List.filter (fun s -> s <> "")
  |> List.map String.capitalize_ascii
  |> String.concat ""

(* The 3D module / file base for the doc pipeline: [?name] when given, else the
   opam [~package]. Lets the emitted [<Base>.3d] / [.c] be named independently of
   the install package (whose name [~package] keeps for the install stanza). *)
let standalone_base ?name ~package () =
  doc_module_name (match name with Some n -> n | None -> package)

(* -- Differential self-check for the doc pipeline. The doc projection emits a
   validator-only C parser with no FFI, so nothing otherwise confirms that the
   generated validator accepts exactly the inputs the OCaml codec accepts: a
   bug in the projection (wrong bit order, a constraint that means something
   different over the wire type) would pass unnoticed. [generate_corpus] prints
   fuzzed inputs tagged with the OCaml verdict, and [generate_agree] emits a C
   program that runs the validator on each and fails on any disagreement. -- *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  let ppf = Fmt.with_buffer buf in
  Bytes.iter (fun c -> Fmt.pf ppf "%02x" (Char.code c)) b;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The accept/reject decision the validator must reproduce: the OCaml codec
   decodes (structure plus constraints), validates (constraints and
   where-clauses), and the record spans the whole buffer -- the hardened
   [<Base>Check<Codec>] wrapper rejects trailing bytes (see [harden_wrapper]),
   so the oracle must too. *)
let codec_accepts ?env c buf =
  match Wire.Codec.decode ?env c buf 0 with
  | Ok _ -> (
      try
        Wire.Codec.validate ?env c buf 0;
        Wire.Codec.wire_size_at c buf 0 = Bytes.length buf
      with Wire.Parse_error _ -> false)
  | Error _ -> false

(* A small param value, biased around the codec's footprint so a parameter that
   bounds a field size (e.g. a max length) straddles the accept/reject boundary
   rather than always accepting or always rejecting. *)
let fuzz_param_value rng center =
  match Random.State.int rng 6 with
  | 0 -> 0
  | 1 -> center
  | 2 -> center + 1
  | 3 -> Random.State.int rng (max 1 ((2 * center) + 4))
  | _ -> Random.State.int rng (max 1 (center + 1))

(* Structurally-biased lengths: cluster around the codec's minimum size so the
   corpus straddles the accept/reject boundary. The exact size and a little
   over exercise the constraint path; under-size exercises truncation; a few
   far-out lengths add breadth. *)
let fuzz_length rng center =
  match Random.State.int rng 10 with
  | 0 -> 0
  | 1 | 2 -> Random.State.int rng (max 1 (center + 1))
  | 3 -> (2 * center) + Random.State.int rng 4
  | 4 -> center + 1
  | _ -> center

let generate_corpus ?(count = 256) ppf codecs =
  let rng = Random.State.make [| 0x5eed51 |] in
  List.iter
    (fun (Pack c) ->
      let s = project ~mode:`Standalone c in
      let name = s.name in
      let pnames =
        match s.source with Some st -> Raw.input_param_names st | None -> []
      in
      let center = Wire.Codec.min_wire_size c in
      for _ = 1 to count do
        let len = fuzz_length rng center in
        let b = Bytes.init len (fun _ -> Char.chr (Random.State.int rng 256)) in
        let pvals = List.map (fun _ -> fuzz_param_value rng center) pnames in
        (* Seed the validator env with the same param values the C wrapper will
           be given, so the two agree on what they validated against. *)
        let env =
          match pnames with
          | [] -> None
          | _ ->
              Some
                (List.fold_left2
                   (fun e n v -> Wire.Param.bind_by_name n v e)
                   (Wire.Codec.env c) pnames pvals)
        in
        let pfield =
          match pvals with
          | [] -> "-"
          | _ -> String.concat "," (List.map string_of_int pvals)
        in
        let hex = if len = 0 then "-" else hex_of_bytes b in
        Fmt.pf ppf "%s %s %s %d@\n" name pfield hex
          (if codec_accepts ?env c b then 1 else 0)
      done)
    codecs;
  Format.pp_print_flush ppf ()

let emit_agree_preamble ppf base ~has_params =
  let pr fmt = Fmt.pf ppf (fmt ^^ "@\n") in
  pr "/* Differential check: the EverParse validator must accept exactly the";
  pr "   inputs the OCaml codec accepts. Reads `<codec> <params> <hex>";
  pr "   <verdict>` lines from gen.exe's corpus, passing each codec's";
  pr "   parameters to its validator, and exits nonzero on any disagreement. */";
  pr "#include <stdio.h>";
  pr "#include <stdlib.h>";
  pr "#include <string.h>";
  pr "#include <stdint.h>";
  pr "#include \"%s.h\"" base;
  pr "#include \"%sWrapper.h\"" base;
  pr "";
  (* The wrapper declares this error sink only in its .c, so declare it here too
     to satisfy -Wmissing-prototypes before defining it as a no-op. *)
  pr "void %sEverParseError(const char *s, const char *f, const char *r);" base;
  pr "void %sEverParseError(const char *s, const char *f, const char *r)" base;
  pr "{ (void) s; (void) f; (void) r; }";
  (* Only emit the parameter parser when a codec actually has parameters: a
     package of plain (non-parameterized) codecs never calls it, and an unused
     static function is a -Werror under the strict flags. *)
  if has_params then begin
    pr "";
    pr "/* Parse the corpus's comma-separated parameter values. */";
    pr "static void parse_params(const char *s, unsigned long *out, int n) {";
    pr "  const char *p = s;";
    pr "  for (int i = 0; i < n; i++) {";
    pr "    out[i] = strtoul(p, NULL, 10);";
    pr "    const char *c = strchr(p, ',');";
    pr "    if (c == NULL) break;";
    pr "    p = c + 1;";
    pr "  }";
    pr "}"
  end

let emit_agree_run ppf triples =
  let pr fmt = Fmt.pf ppf (fmt ^^ "@\n") in
  pr "";
  pr
    "static int run(const char *name, const char *params, uint8_t *base, \
     uint32_t len) {";
  pr "  (void) params;";
  List.iter
    (fun (cname, check, ptypes) ->
      let n = List.length ptypes in
      if n = 0 then
        pr "  if (strcmp(name, \"%s\") == 0) return %s(base, len) ? 1 : 0;"
          cname check
      else begin
        let args =
          ptypes
          |> List.mapi (fun i t -> Fmt.str "(%s) p[%d]" t i)
          |> String.concat ", "
        in
        pr "  if (strcmp(name, \"%s\") == 0) {" cname;
        pr "    unsigned long p[%d];" n;
        pr "    parse_params(params, p, %d);" n;
        pr "    return %s(%s, base, len) ? 1 : 0;" check args;
        pr "  }"
      end)
    triples;
  pr "  fprintf(stderr, \"agree: unknown codec '%%s'\\n\", name);";
  pr "  exit(3);";
  pr "}"

let emit_agree_main ppf =
  let pr fmt = Fmt.pf ppf (fmt ^^ "@\n") in
  pr "";
  pr "int main(int argc, char **argv) {";
  pr
    "  if (argc < 2) { fprintf(stderr, \"usage: %%s <corpus>\\n\", argv[0]); \
     return 2; }";
  pr "  FILE *fp = fopen(argv[1], \"r\");";
  pr "  if (!fp) { perror(\"fopen\"); return 2; }";
  pr "  char name[256];";
  pr "  char params[4096];";
  (* [hex] needs two chars per [buf] byte plus the NUL; one char short truncates
     the corpus line and misparses the verdict as a false mismatch. *)
  pr "  uint8_t buf[65536];";
  pr "  char hex[2 * sizeof(buf) + 1];";
  pr "  long verdict, total = 0, mismatch = 0;";
  pr
    "  while (fscanf(fp, \"%%255s %%4095s %%131072s %%ld\", name, params, hex, \
     &verdict) == 4) {";
  pr "    uint32_t len = 0;";
  pr "    if (strcmp(hex, \"-\") != 0) {";
  pr "      size_t hl = strlen(hex);";
  pr "      len = (uint32_t) (hl / 2);";
  pr
    "      if (len > sizeof(buf)) { fprintf(stderr, \"input too long\\n\"); \
     fclose(fp); return 2; }";
  pr "      for (uint32_t i = 0; i < len; i++) {";
  pr "        unsigned b;";
  pr
    "        if (sscanf(hex + 2 * i, \"%%2x\", &b) != 1) { fprintf(stderr, \
     \"bad hex\\n\"); fclose(fp); return 2; }";
  pr "        buf[i] = (uint8_t) b;";
  pr "      }";
  pr "    }";
  pr "    int accept = run(name, params, buf, len);";
  pr "    total++;";
  pr "    if (accept != (int) verdict) {";
  pr "      mismatch++;";
  pr "      if (mismatch <= 20)";
  pr
    "        fprintf(stderr, \"MISMATCH codec=%%s len=%%u validator=%%d \
     oracle=%%ld\\n\", name, len, accept, verdict);";
  pr "    }";
  pr "  }";
  pr "  fclose(fp);";
  pr
    "  fprintf(stdout, \"agree: %%ld inputs, %%ld mismatches\\n\", total, \
     mismatch);";
  pr "  return mismatch == 0 ? 0 : 1;";
  pr "}"

(* EverParse names each entrypoint wrapper [<Base>Check<Codec>] and gives it the
   input parameters before the trailing [base, len], so both the helper name and
   its parameter C types follow from the codec alone. Deriving them here keeps
   [agree.c] pure OCaml (no reading of the generated [<Base>Wrapper.h]); a wrong
   name would surface as a link error when the differential test compiles
   [agree.c] against the real wrapper. *)
let generate_agree ?name ~outdir ~package codecs =
  let base = standalone_base ?name ~package () in
  let triples =
    List.map
      (fun (Pack c) ->
        let s = project ~mode:`Standalone c in
        let ptypes =
          match s.source with
          | Some st -> Raw.input_param_c_types st
          | None -> []
        in
        (* EverParse builds the wrapper symbol as
           [pascal_case (module ^ "_check_" ^ codec)], so compute it the same way
           rather than gluing pre-normalized parts: only the whole-string
           [pascal_case] gets the casing right for names like [TPM2B -> Tpm2b]. *)
        (s.name, pascal_case (base ^ "_check_" ^ s.name), ptypes))
      codecs
  in
  let has_params = List.exists (fun (_, _, ptypes) -> ptypes <> []) triples in
  let oc = open_out (Filename.concat outdir "agree.c") in
  let ppf = Format.formatter_of_out_channel oc in
  emit_agree_preamble ppf base ~has_params;
  emit_agree_run ppf triples;
  emit_agree_main ppf;
  Format.pp_print_flush ppf ();
  close_out oc

let generate_3d_standalone ?name ~outdir ~package codecs =
  ensure_dir outdir;
  write ~mode:`Standalone ~outdir
    ~name:(standalone_base ?name ~package ())
    (List.map (fun (Pack c) -> project ~mode:`Standalone c) codecs)

let generate_c_standalone ?(quiet = true) ?name ~outdir ~package () =
  ensure_dir outdir;
  if has_3d_exe () then
    run_everparse_files ~quiet ~outdir
      [ standalone_base ?name ~package () ^ ".3d" ]
  else
    failwith
      "3d.exe not found in PATH. Install EverParse to regenerate C files."

let generate_standalone ?(quiet = true) ?name ~outdir ~package codecs =
  generate_3d_standalone ?name ~outdir ~package codecs;
  generate_c_standalone ~quiet ?name ~outdir ~package ();
  generate_agree ?name ~outdir ~package codecs

(* The [.3d] and [agree.c] are pure OCaml ([gen.exe 3d] / [gen.exe agree]), so
   they regenerate on demand and never go stale. The C needs [3d.exe], so its
   rule exists only under [BUILD_EVERPARSE=1] ([mode promote] writes it back into
   the tree); a plain [dune build] uses the committed C and never invokes
   [3d.exe], and fails loudly if the C was never committed rather than silently
   shelling out. A codec change refreshes the [.3d] and [agree.c] while the
   committed C goes stale -- the runtest differential below catches that, since
   it regenerates the corpus and [agree.c] from the current codec and runs them
   against the committed validator. *)
let emit_standalone_gen_rules ppf ~three_d ~c_files =
  Fmt.pf ppf
    "(rule\n\
    \ (alias 3d)\n\
    \ (mode promote)\n\
    \ (targets %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} 3d)))\n\n\
     (rule\n\
    \ (targets agree.c)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} agree)))\n\n\
     (rule\n\
    \ (alias 3d)\n\
    \ (enabled_if\n\
    \  (= %%{env:BUILD_EVERPARSE=} \"1\"))\n\
    \ (mode promote)\n\
    \ (targets EverParse.h EverParseEndianness.h %s)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} c)))\n\n"
    three_d
    (String.concat " " c_files)
    three_d

(* The C symbols a standalone archive exports: the [<Base>Check<Codec>] wrapper
   for each codec, named exactly as EverParse names it (and as [agree.c] links
   it, see [generate_agree]), so the export allowlist matches the real symbol. *)
let wrapper_symbols base codecs =
  List.map
    (fun (Pack c) ->
      pascal_case (base ^ "_check_" ^ (project ~mode:`Standalone c).name))
    codecs

(* Post-compile steps that fold the compiled objects into one archive member
   exporting only [wrappers], localizing the raw [<Base>Validate*] entry points
   so their unguarded [StartPosition] (see [emit_standalone_install]) is not
   reachable through the installed archive. macOS localizes during the partial
   link ([ld -r -exported_symbol]); GNU [ld] cannot, so [objcopy] does it after.
   Shared by the emitted dune rule and the symbol-hiding test so they cannot
   drift. *)
let archive_link_steps ~macos ~archive ~base ~wrappers =
  let libo = base ^ "_lib.o" in
  let objs = Fmt.str "%s.o %sWrapper.o" base base in
  if macos then
    [
      Fmt.str "ld -r -o %s %s%s" libo objs
        (List.fold_left (fun a w -> a ^ " -exported_symbol _" ^ w) "" wrappers);
      Fmt.str "ar rcs %s %s" archive libo;
    ]
  else
    [
      Fmt.str "ld -r -o %s %s" libo objs;
      Fmt.str "objcopy%s %s"
        (List.fold_left
           (fun a w -> a ^ " --keep-global-symbol " ^ w)
           "" wrappers)
        libo;
      Fmt.str "ar rcs %s %s" archive libo;
    ]

(* Differential self-check: the OCaml codec's accept/reject verdict over a
   fuzzed corpus must match the committed C validator's, or the doc projection
   is unsound (or the committed C is stale). The corpus and the [agree] driver
   are built as targets and run directly, so no shell is involved: the generator
   is referenced through [%{exe:gen.exe}], which records the dependency and
   resolves the sandbox path (a bare [./gen.exe] is not reliably present in the
   action's cwd). Uses only gen.exe and cc, no 3d.exe. *)
let emit_standalone_check_rules ppf ~base ~archive =
  Fmt.pf ppf
    "(rule\n\
    \ (targets corpus)\n\
    \ (action\n\
    \  (with-stdout-to corpus (run %%{exe:gen.exe} corpus))))\n\n\
     (rule\n\
    \ (targets agree)\n\
    \ (deps agree.c %s EverParse.h EverParseEndianness.h %s.h %sWrapper.h)\n\
    \ (action\n\
    \  (run cc %s %s agree.c %s -o agree)))\n\n\
     (rule\n\
    \ (alias runtest)\n\
    \ (deps corpus agree)\n\
    \ (action\n\
    \  (run %%{dep:agree} corpus)))\n\n"
    archive base base strict_cc_flags everparse_type_defines archive

let emit_standalone_build_rules ppf ~base ~archive ~c_files ~wrappers =
  (* Build the validator into an archive, installed with the package, so
     consumers get a ready-to-link library and a downstream build fails loudly
     if the spec stops projecting to compilable C. The archive exports only the
     checked [<Base>Check<Codec>] wrappers and localizes the raw validators; the
     localizing step is platform-specific, so there is one rule per
     [ocaml-config:system]. *)
  let compile =
    Fmt.str "cc %s %s -c %s.c %sWrapper.c" strict_cc_flags
      everparse_type_defines base base
  in
  let emit_rule ~cond ~macos =
    let steps = compile :: archive_link_steps ~macos ~archive ~base ~wrappers in
    Fmt.pf ppf
      "(rule\n\
      \ (targets %s)\n\
      \ (enabled_if %s)\n\
      \ (deps EverParse.h EverParseEndianness.h %s)\n\
      \ (action\n\
      \  (progn\n"
      archive cond
      (String.concat " " c_files);
    List.iter (fun s -> Fmt.pf ppf "   (run %s)\n" s) steps;
    Fmt.pf ppf "   )))\n\n"
  in
  emit_rule ~cond:"(= %{ocaml-config:system} macosx)" ~macos:true;
  emit_rule ~cond:"(<> %{ocaml-config:system} macosx)" ~macos:false;
  emit_standalone_check_rules ppf ~base ~archive

(* Install only the checked wrapper header, not the raw validator header. The
   [<Base>Validate*] entrypoints in [<Base>.h] take a [StartPosition] and their
   EverParse-emitted preamble bounds a read as [N <= InputLength - StartPosition]
   with no prior [StartPosition <= InputLength] check, so a direct C caller
   passing [StartPosition > InputLength] underflows the span (unsigned) and reads
   out of bounds. The generated wrapper [<Base>Check<Codec>(base, len)] always
   validates from position 0 and is the safe public C API; the raw validators
   stay build-internal, linked into the archive but not shipped as a header.
   [<Base>Wrapper.h] does not include [<Base>.h], so this compiles standalone. *)
let emit_standalone_install ppf ~package ~three_d ~archive ~public_header =
  let pr fmt = Fmt.pf ppf fmt in
  pr "(install\n (package %s)\n (section lib)\n (files\n" package;
  List.iter
    (fun f -> pr "  (%s as c/%s)\n" f f)
    [ three_d; archive; public_header ];
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n"

let generate_dune_standalone ?name ~outdir ~package codecs =
  let base = standalone_base ?name ~package () in
  let three_d = base ^ ".3d" in
  let c_files =
    [ base ^ ".c"; base ^ ".h"; base ^ "Wrapper.c"; base ^ "Wrapper.h" ]
  in
  let archive = "lib" ^ String.lowercase_ascii base ^ ".a" in
  let wrappers = wrapper_symbols base codecs in
  let oc = open_out (Filename.concat outdir "dune.inc") in
  let ppf = Format.formatter_of_out_channel oc in
  emit_standalone_gen_rules ppf ~three_d ~c_files;
  emit_standalone_build_rules ppf ~base ~archive ~c_files ~wrappers;
  emit_standalone_install ppf ~package ~three_d ~archive
    ~public_header:(base ^ "Wrapper.h");
  Format.pp_print_flush ppf ();
  close_out oc

let main ?name ~mode ~package codecs =
  let argv = Array.to_list Sys.argv in
  match mode with
  | `Ffi -> (
      let schemas = List.map (fun (Pack c) -> project ~mode:`Ffi c) codecs in
      match argv with
      | [ _; "3d" ] -> generate_3d ~outdir:"." schemas
      | [ _; "c" ] -> generate_c ~outdir:"." schemas
      | [ _; "dune" ] -> generate_dune ~outdir:"." ~package schemas
      | _ -> run ~outdir:"." schemas)
  | `Standalone -> (
      match argv with
      | [ _; "3d" ] -> generate_3d_standalone ?name ~outdir:"." ~package codecs
      | [ _; "c" ] -> generate_c_standalone ?name ~outdir:"." ~package ()
      | [ _; "agree" ] -> generate_agree ?name ~outdir:"." ~package codecs
      | [ _; "dune" ] ->
          generate_dune_standalone ?name ~outdir:"." ~package codecs
      | [ _; "corpus" ] -> generate_corpus Format.std_formatter codecs
      | _ -> generate_standalone ?name ~outdir:"." ~package codecs)
