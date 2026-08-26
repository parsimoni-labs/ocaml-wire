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

(* Both mangles are many-to-one, so two codecs with different names can land
   on one generated artifact and overwrite each other in silence. [file_base]
   collides whenever the names differ only in the leading capital ([header]
   and [Header] both write [Header.3d], and the second write wins: the
   verified C, the plug and the provenance stamp then describe one codec while
   the OCaml side still parses with both, so the shadowed codec's FFI stubs
   validate against the other codec's spec). [c_ident] collides more widely,
   since [everparse_name] also strips underscores and collapses uppercase runs
   ([TMFrame] and [Tmframe] write two [_Fields.h] files that share the
   [TMFRAME_FIELDS_H] include guard and the [TmframeFields] struct tag, so a
   translation unit including both takes one schema's plug layout for the
   other's). The standalone pipeline already rejects a name collision in
   [Wire.Everparse.merge]; do the same for the per-schema pipeline, up front
   and naming both codecs. Its remaining collapse, two codecs sharing one
   [pascal_case] wrapper symbol ([TPM2B] and [Tpm2b]), is left to the C
   compiler: the merged module puts both definitions in one translation unit,
   so it fails loudly at build time rather than substituting one for the
   other. *)
let check_name_collisions schemas =
  let check what key =
    let seen = Hashtbl.create 16 in
    List.iter
      (fun (s : t) ->
        let k = key s in
        match Hashtbl.find_opt seen k with
        | Some first ->
            Fmt.invalid_arg
              "Wire_3d: codecs %S and %S both generate %s %S; rename one of \
               them"
              first s.name what k
        | None -> Hashtbl.add seen k s.name)
      schemas
  in
  check "the file" (fun s -> file_base s ^ ".3d");
  check "the C identifier" c_ident

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

(* EverParse's top-level validator function follows its own normalization rule
   and includes the 3D struct tag after [Validate]. Rather than duplicate that
   logic, extract the complete function name from the generated [<Name>.h]. *)
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
  (* Find [Validate] inside the declaration and return its complete surrounding
     C identifier. Scanning every position lets the module name itself contain a
     [V]; the identifier boundaries also discard the return type and arguments. *)
  let identifier_containing_validate line =
    let len = String.length line in
    let rec scan i =
      if i + nlen > len then None
      else if i > 0 && is_ident line.[i - 1] && String.sub line i nlen = needle
      then begin
        let j = ref i in
        while !j > 0 && is_ident line.[!j - 1] do
          decr j
        done;
        let k = ref (i + nlen) in
        while !k < len && is_ident line.[!k] do
          incr k
        done;
        Some (String.sub line !j (!k - !j))
      end
      else scan (i + 1)
    in
    scan 0
  in
  (try
     while !found = None do
       let line = String.trim (input_line ic) in
       found := identifier_containing_validate line
     done
   with End_of_file -> ());
  close_in ic;
  match !found with
  | Some n -> n
  | None -> Fmt.failwith "could not find Validate function name in %s" path

let write_3d ~outdir schemas =
  check_name_collisions schemas;
  Wire.Everparse.write ~mode:`Ffi ~outdir schemas

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let executable path =
  try
    Unix.access path [ Unix.X_OK ];
    not (Sys.is_directory path)
  with Unix.Unix_error _ | Sys_error _ -> false

let locate_3d_exe () =
  let path =
    Sys.getenv_opt "PATH" |> Option.to_list
    |> List.concat_map (String.split_on_char ':')
    |> List.find_map (fun dir ->
        let dir = if dir = "" then "." else dir in
        let candidate = absolute_path (Filename.concat dir "3d.exe") in
        if executable candidate then Some candidate else None)
  in
  match path with
  | Some p -> Some p
  | None ->
      let local =
        Filename.concat (Sys.getenv "HOME") ".local/everparse/bin/3d.exe"
      in
      if executable local then Some local else None

type process_output = Inherit | Dev_null | File of string

let process_status_code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

(* Run [exe] without a shell. Redirection is opened in the parent, then the
   child changes directory and receives each argument literally through
   [execv]. This keeps a process-local cwd: changing it in the parent would race
   with other domains and fibers. *)
let run_process ?(output = Inherit) ~cwd exe args =
  let output_fd =
    match output with
    | Inherit -> None
    | Dev_null -> Some (Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o600)
    | File path ->
        Some
          (Unix.openfile path
             [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
             0o600)
  in
  match Unix.fork () with
  | 0 -> (
      try
        Unix.chdir cwd;
        Option.iter
          (fun fd ->
            Unix.dup2 fd Unix.stdout;
            Unix.dup2 fd Unix.stderr;
            Unix.close fd)
          output_fd;
        Unix.execv exe (Array.of_list (exe :: args))
      with Unix.Unix_error _ -> Unix._exit 127)
  | pid ->
      Option.iter Unix.close output_fd;
      snd (Unix.waitpid [] pid)

let everparse_version exe =
  let ic = Unix.open_process_args_in exe [| exe; "--version" |] in
  let output = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> (
      match String.split_on_char '\n' output with
      | version :: _ when version <> "" -> version
      | _ -> Fmt.failwith "%s --version returned no version" exe)
  | Unix.WEXITED n -> Fmt.failwith "%s --version exited with code %d" exe n
  | Unix.WSIGNALED n ->
      Fmt.failwith "%s --version was killed by signal %d" exe n
  | Unix.WSTOPPED n ->
      Fmt.failwith "%s --version was stopped by signal %d" exe n

let provenance_file three_d =
  Filename.remove_extension (Filename.basename three_d) ^ ".provenance"

let schema_digest ~outdir three_d =
  Digest.BLAKE256.(to_hex (file (Filename.concat outdir three_d)))

let write_provenance ~outdir ~version three_d =
  let digest = schema_digest ~outdir three_d in
  let path = Filename.concat outdir (provenance_file three_d) in
  Out_channel.with_open_bin path (fun oc ->
      Fmt.pf
        (Format.formatter_of_out_channel oc)
        "schema-blake2b-256: %s\neverparse: %s\n%!" digest version)

let recorded_digest path =
  let prefix = "schema-blake2b-256: " in
  In_channel.with_open_bin path In_channel.input_lines
  |> List.find_map (fun line ->
      if String.starts_with ~prefix line then
        Some
          (String.sub line (String.length prefix)
             (String.length line - String.length prefix))
      else None)
  |> function
  | Some digest -> digest
  | None -> Fmt.failwith "%s: missing schema-blake2b-256" path

(* A stamp is only ever written beside the C that EverParse produced from that
   exact [.3d], so a recorded hash that no longer matches means the committed C
   is stale. Fail instead of emitting a promotable diff: promoting a recomputed
   stamp would relabel the stale C as current and hide the drift for good. *)
let check_provenance ~outdir three_d_files =
  List.iter
    (fun three_d ->
      let stamp = Filename.concat outdir (provenance_file three_d) in
      let recorded = recorded_digest stamp in
      let actual = schema_digest ~outdir three_d in
      if recorded <> actual then
        Fmt.failwith
          "stale generated C: %s records schema-blake2b-256 %s but %s hashes \
           to %s; regenerate with BUILD_EVERPARSE=1 dune build @3d"
          stamp recorded three_d actual)
    three_d_files

let everparse_dir () =
  match locate_3d_exe () with
  | Some exe -> Filename.dirname exe |> Filename.dirname
  | None -> failwith "3d.exe not found"

(* A freestanding endianness branch for EverParseEndianness.h. The shipped
   header keys byte order off an OS (<endian.h> on Linux, OSSwap on macOS, ...)
   and [#error]s when none matches, so it does not compile for a no-OS target
   like a unikernel (which defines neither [__linux__] nor [__APPLE__]). Any GCC
   or Clang exposes byte order as [__BYTE_ORDER__] and byte-swaps with
   [__builtin_bswap*] without an OS, so this branch, spliced in before the
   [#error], makes the header portable to a cross target. *)
let endianness_freestanding_branch =
  {|#elif (defined(__GNUC__) || defined(__clang__)) && defined(__BYTE_ORDER__)
/* Freestanding target with no OS <endian.h> (e.g. a unikernel): take byte
   order from the compiler and byte-swap with its builtins. */
#  if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#    define htobe16(x) __builtin_bswap16(x)
#    define htole16(x) (x)
#    define be16toh(x) __builtin_bswap16(x)
#    define le16toh(x) (x)
#    define htobe32(x) __builtin_bswap32(x)
#    define htole32(x) (x)
#    define be32toh(x) __builtin_bswap32(x)
#    define le32toh(x) (x)
#    define htobe64(x) __builtin_bswap64(x)
#    define htole64(x) (x)
#    define be64toh(x) __builtin_bswap64(x)
#    define le64toh(x) (x)
#  else
#    define htobe16(x) (x)
#    define htole16(x) __builtin_bswap16(x)
#    define be16toh(x) (x)
#    define le16toh(x) __builtin_bswap16(x)
#    define htobe32(x) (x)
#    define htole32(x) __builtin_bswap32(x)
#    define be32toh(x) (x)
#    define le32toh(x) __builtin_bswap32(x)
#    define htobe64(x) (x)
#    define htole64(x) __builtin_bswap64(x)
#    define be64toh(x) (x)
#    define le64toh(x) __builtin_bswap64(x)
#  endif

|}

(* The EverParse anchor the freestanding branch is spliced before: the
   fallthrough that rejects an unrecognized platform. EverParse ships this
   header with CRLF line endings, so the blank line between the two directives
   is matched as a run of newline characters rather than pinned to [\n]. *)
let endianness_unsupported_anchor =
  Re.compile
    (Re.seq
       [
         Re.str "#else";
         Re.rep1 (Re.set "\r\n");
         Re.str "#error \"Unsupported platform\"";
       ])

(* Says the freestanding branch is already spliced in, so a second pass over
   the same file does not add it twice. *)
let endianness_freestanding_marker =
  Re.compile (Re.str "defined(__BYTE_ORDER__)")

let with_freestanding_endianness content =
  if Re.execp endianness_freestanding_marker content then content
  else if not (Re.execp endianness_unsupported_anchor content) then
    failwith
      "EverParseEndianness.h: no \"Unsupported platform\" fallthrough to \
       splice the freestanding byte-order branch before; EverParse's header \
       layout changed"
  else
    Re.replace ~all:false endianness_unsupported_anchor
      ~f:(fun g -> endianness_freestanding_branch ^ Re.Group.get g 0)
      content

(* Patch the [EverParseEndianness.h] in [outdir] so it also compiles for a
   target with no OS <endian.h>. 3d.exe writes its own copy of this header on
   every run, overwriting whatever is there, so the branch has to be spliced
   into that copy after 3d.exe has run: patching a file that is not there yet,
   or skipping the patch because the file already exists, both leave the
   shipped header [#error]ing on the cross target the standalone C archive is
   meant to serve. Falls back to EverParse's source copy when there is nothing
   in [outdir] to patch. *)
let patch_everparse_endianness ~outdir =
  let dst = Filename.concat outdir "EverParseEndianness.h" in
  let src =
    if Sys.file_exists dst then dst
    else begin
      let src =
        Filename.concat (everparse_dir ()) "src/3d/EverParseEndianness.h"
      in
      if not (Sys.file_exists src) then
        Fmt.failwith "Cannot find EverParseEndianness.h at %s" src;
      src
    end
  in
  let content = In_channel.with_open_bin src In_channel.input_all in
  let patched = with_freestanding_endianness content in
  Out_channel.with_open_bin dst (fun oc -> Out_channel.output_string oc patched)

let has_3d_exe () = locate_3d_exe () <> None

(* The [_ExternalTypedefs.h] header seen by the EverParse-generated validator
   and wrapper. The default shipped by wire.3d is a forward declaration that
   ties WIRECTX to the matching [<Name>_Fields] plug struct. Users who want
   their own plug (e.g. {!Wire_stubs} for OCaml FFI) overwrite this file with
   a different WIRECTX definition; they must then also omit the default
   [<Name>_Fields.c] from their link. *)
let write_external_typedefs ~outdir schemas =
  check_name_collisions schemas;
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
  check_name_collisions schemas;
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

(* EverParse's [<Base>CheckWire<Codec>] wrapper returns TRUE on any successful
   validator result, but a success result is the consumed position: a valid
   record followed by trailing bytes still returns TRUE, making the wrapper a
   prefix recognizer rather than a validator. Wire's contract is whole-buffer
   validation, so rewrite each success tail to also require full consumption.
   Textual on EverParse's wrapper shape (the tail is identical for
   parameterized and plain entrypoints, [print_c_entry] in the upstream
   [src/3d/Target.fst]): if a future EverParse emits neither the known tail
   nor its own consumption check, fail loudly rather than silently shipping a
   prefix recognizer. Tracked upstream as
   https://github.com/project-everest/everparse/issues/312. The behavioural
   backstop is the differential runtest, whose corpus includes over-length
   inputs the oracle rejects. *)
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
  let version = everparse_version exe in
  List.iter
    (fun f ->
      let output = if quiet then Dev_null else Inherit in
      let ret =
        run_process ~output ~cwd:outdir exe [ "--batch"; f ]
        |> process_status_code
      in
      if ret <> 0 then Fmt.failwith "EverParse failed on %s with code %d" f ret;
      harden_wrapper ~outdir (Filename.remove_extension (Filename.basename f));
      write_provenance ~outdir ~version f)
    files;
  (* Last, because every 3d.exe run above rewrites this header. *)
  patch_everparse_endianness ~outdir

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
  let args = if batch then [ "--batch"; file ] else [ file ] in
  let ret =
    run_process ~output:(File log_path) ~cwd:outdir exe args
    |> process_status_code
  in
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

let emit_sanity_check ppf ~name ~validator ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  (* Sanity: the OCaml codec's wire_size must match what the EverParse
     validator consumes. A mismatch means the .3d projection of the codec
     packs to a different size than the codec declares -- almost always a
     bug in the codec's bitfield declarations. Later checks are meaningless
     if this fails, so abort the whole test binary with a clear message. *)
  pr "    r = %s(%sNULL, counting_error_handler, buf, %d, 0);\n" validator
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

let emit_truncation_checks ppf ~validator ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  pr "    r = %s(%sNULL, counting_error_handler, buf, %d, 0);\n" validator
    ctx_arg (wire_size * 2);
  pr "    CHECK(\"larger buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position is %d not %d\", r == %d);\n" wire_size
    (wire_size * 2) wire_size;
  pr "\n";
  pr "    for (uint64_t len = 0; len < %d; len++) {\n" wire_size;
  pr "      error_count = 0;\n";
  pr "      r = %s(%sNULL, counting_error_handler, buf, len, 0);\n" validator
    ctx_arg;
  pr "      CHECK(\"truncated to len fails\", EverParseIsError(r));\n";
  pr "    }\n";
  pr "\n";
  pr "    r = %s(%sNULL, counting_error_handler, buf, 0, 0);\n" validator
    ctx_arg;
  pr "    CHECK(\"empty input fails\", EverParseIsError(r));\n"

let emit_random_checks ppf ~validator ~ctx_arg wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  pr "    srand(42);\n";
  pr "    for (int i = 0; i < 1000; i++) {\n";
  pr "      for (int j = 0; j < %d; j++)\n" wire_size;
  pr "        buf[j] = (uint8_t)(rand() & 0xff);\n";
  pr "      r = %s(%sNULL, counting_error_handler, buf, %d, 0);\n" validator
    ctx_arg wire_size;
  pr "      CHECK(\"random buffer validates\", EverParseIsSuccess(r));\n";
  pr "      CHECK(\"random position correct\", r == %d);\n" wire_size;
  pr "    }\n"

let emit_schema_test ~outdir ppf s wire_size =
  let pr fmt = Fmt.pf ppf fmt in
  (* Read the validator name straight out of EverParse's generated [.h]
     -- the one authoritative source. EverParse applies its own naming
     rules (different for the top-level Validate function vs. the extern
     callbacks); any attempt to re-implement them here has drifted before. *)
  let validator = read_validate_name ~outdir s in
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
  emit_sanity_check ppf ~name:s.name ~validator ~ctx_arg wire_size;
  pr "    CHECK(\"zero buffer validates\", EverParseIsSuccess(r));\n";
  pr "    CHECK(\"position advanced to %d\", r == %d);\n" wire_size wire_size;
  pr "\n";
  emit_truncation_checks ppf ~validator ~ctx_arg wire_size;
  pr "\n";
  emit_random_checks ppf ~validator ~ctx_arg wire_size;
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

let copy_file ~src ~dst =
  let contents = In_channel.with_open_bin src In_channel.input_all in
  Out_channel.with_open_bin dst (fun oc ->
      Out_channel.output_string oc contents)

let rm_rf dir =
  (try Sys.readdir dir with Sys_error _ -> [||])
  |> Array.iter (fun f ->
      try Sys.remove (Filename.concat dir f) with Sys_error _ -> ());
  try Sys.rmdir dir with Sys_error _ -> ()

(* Regenerate into a scratch directory through the same writer the committed
   [.3d] came from, so the drift check compares against the real generator
   rather than against a second copy of it that could itself drift. *)
let generate_3d_check ~outdir schemas =
  ensure_dir outdir;
  let tmpdir = Filename.temp_dir "wire_3d_check" "" in
  Fun.protect
    ~finally:(fun () -> rm_rf tmpdir)
    (fun () ->
      write_3d ~outdir:tmpdir schemas;
      List.iter
        (fun s ->
          let file = Wire.Everparse.filename s in
          copy_file
            ~src:(Filename.concat tmpdir file)
            ~dst:(Filename.concat outdir (file ^ ".gen")))
        schemas)

let default_job_count () = max 1 (min 4 (Domain.recommended_domain_count ()))

(* A bounded fork pool: each job runs in its own process, so blocking EverParse
   runs overlap across cores with full isolation. Returns each
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
                let status =
                  run_process
                    ~output:(File (log_of i))
                    ~cwd:work exe
                    [
                      "--batch";
                      "--no_copy_everparse_h";
                      Wire.Everparse.filename schema;
                    ]
                in
                if process_status_code status <> 0 then
                  failwith "EverParse rejected"))
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
  check_name_collisions schemas;
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

let emit_gen_rules ppf three_d_files c_files ctx_files provenance_files =
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
    \ (targets EverParse.h EverParseEndianness.h %s test.c %s)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} c)))\n\n"
    (String.concat " " three_d_files)
    (String.concat " " (c_files @ ctx_files))
    (String.concat " " provenance_files)
    (String.concat " " three_d_files)

(* One [runtest] rule per file rather than one [progn] over all of them: a
   [progn] stops at the first mismatch, so a single pass would report and offer
   to promote only the first drifted spec. *)
let emit_drift_check_rules ppf three_d_files =
  let generated = List.map (fun f -> f ^ ".gen") three_d_files in
  let pr fmt = Fmt.pf ppf fmt in
  pr "(rule\n (targets %s)\n (action\n  (run %%{exe:gen.exe} 3d-gen)))\n\n"
    (String.concat " " generated);
  List.iter
    (fun f ->
      pr "(rule\n (alias runtest)\n (action\n  (diff %s %s.gen)))\n\n" f f)
    three_d_files;
  pr
    "(rule\n\
    \ (targets dune.inc.gen)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} dune-gen)))\n\n\
     (rule\n\
    \ (alias runtest)\n\
    \ (action\n\
    \  (diff dune.inc dune.inc.gen)))\n\n"

let emit_provenance_check_rules ppf three_d_files =
  let stamps = List.map provenance_file three_d_files in
  Fmt.pf ppf
    "(rule\n\
    \ (alias runtest)\n\
    \ (deps %s %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} provenance-check)))\n\n"
    (String.concat " " three_d_files)
    (String.concat " " stamps)

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

let emit_install_stanza ppf ~package ~three_d_files ~c_files ~ctx_files
    ~provenance_files =
  let pr fmt = Fmt.pf ppf fmt in
  pr "(install\n (package %s)\n (section lib)\n (files\n" package;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) three_d_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) c_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) ctx_files;
  List.iter (fun f -> pr "  (%s as c/%s)\n" f f) provenance_files;
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n"

let generate_dune_file ~filename ~outdir ~package schemas =
  check_name_collisions schemas;
  let oc = open_out (Filename.concat outdir filename) in
  let ppf = Format.formatter_of_out_channel oc in
  let names = List.map file_base schemas in
  let c_files = List.concat_map (fun n -> [ n ^ ".h"; n ^ ".c" ]) names in
  let ctx_files = wire_ctx_files schemas in
  let fields_srcs = fields_c_files schemas in
  let three_d_files = List.map (fun n -> n ^ ".3d") names in
  let provenance_files = List.map provenance_file three_d_files in
  let test_bin =
    "test_" ^ String.map (fun c -> if c = '-' then '_' else c) package
  in
  let all_deps =
    [ "test.c"; "EverParse.h"; "EverParseEndianness.h" ] @ c_files @ ctx_files
  in
  let c_srcs = List.map (fun n -> n ^ ".c") names @ fields_srcs in
  emit_gen_rules ppf three_d_files c_files ctx_files provenance_files;
  emit_drift_check_rules ppf three_d_files;
  emit_provenance_check_rules ppf three_d_files;
  emit_runtest_rule ppf ~test_bin ~all_deps ~c_srcs;
  emit_install_stanza ppf ~package ~three_d_files ~c_files ~ctx_files
    ~provenance_files;
  Format.pp_print_flush ppf ();
  close_out oc

let generate_dune ~outdir ~package schemas =
  generate_dune_file ~filename:"dune.inc" ~outdir ~package schemas

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

(* The accept/reject decision the validator must reproduce, and -- on rejection
   -- what the codec objected to, which makes an input repairable. The record
   must span the whole buffer because the hardened [Check] wrapper rejects
   trailing bytes. [`Resize n] means that the parsed record occupies [n] bytes. *)
let codec_verdict ?env c buf =
  match Wire.Codec.decode ?env c buf 0 with
  | Error e -> `Reject e
  | Ok _ -> (
      match
        Wire.Codec.validate ?env c buf 0;
        Wire.Codec.wire_size_at c buf 0
      with
      | n -> if n = Bytes.length buf then `Accept else `Resize n
      | exception Wire.Parse_error e -> `Reject e)

let codec_accepts ?env c buf =
  match codec_verdict ?env c buf with `Accept -> true | _ -> false

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

(* [agree.c] reads a line into a fixed 64 KiB buffer. *)
let max_corpus_input = 65536

let random_bytes rng n =
  Bytes.init n (fun _ -> Char.chr (Random.State.int rng 256))

(* Keep bytes already repaired when the codec supplies the record's true size. *)
let resize rng buf n =
  let out = random_bytes rng n in
  Bytes.blit buf 0 out 0 (min n (Bytes.length buf));
  out

(* Write the low [width] bytes of [v] without native-int shifts: field seeds may
   describe an eight-byte slot even on wasm_of_ocaml or js_of_ocaml. Negative
   signed values naturally produce their two's-complement wire bytes. *)
let write_slot buf off (slot : Raw.int_slot) v =
  let width = slot.width in
  if off >= 0 && off + width <= Bytes.length buf then
    for i = 0 to width - 1 do
      let shift =
        8
        * match slot.endian with Wire.Big -> width - 1 - i | Wire.Little -> i
      in
      let byte = Int64.(to_int (logand (shift_right_logical v shift) 0xffL)) in
      Bytes.set buf (off + i) (Char.chr byte)
    done

let seed_for seeds (e : Wire.parse_error) =
  match List.rev e.field with
  | leaf :: _ ->
      List.find_opt
        (fun (seed : Raw.field_seed) -> String.equal seed.field leaf)
        seeds
  | [] -> None

(* An EOF can be repaired by growing the input. Other failures can be repaired
   when the failing field's declaration names candidate values. *)
let repair_for ~seeds ~len (e : Wire.parse_error) =
  match e.kind with
  | Wire.Unexpected_eof { expected; got } when expected > got ->
      `Grow (len + expected - got)
  | _ -> (
      match seed_for seeds e with
      | Some seed -> `Place (e.at, seed)
      | None -> `Stuck)

(* Construct one accepting input by repeatedly asking the codec how to repair
   the earliest failure. The field offsets come from parse errors rather than a
   static layout, so the same loop works after variable-width prefixes. *)
let seed_input ?env rng ~seeds ~center c =
  let fuel = ref ((2 * List.length seeds) + 6) in
  let buf = ref (random_bytes rng (max 1 center)) in
  let placed = ref [] in
  let out = ref None in
  let grow n =
    if n >= 0 && n <> Bytes.length !buf && n <= max_corpus_input then
      buf := resize rng !buf n
    else fuel := 0
  in
  let place off (seed : Raw.field_seed) =
    let values = Array.of_list seed.values in
    write_slot !buf off seed.slot
      values.(Random.State.int rng (Array.length values));
    if not (List.mem_assoc off !placed) then placed := (off, seed) :: !placed
  in
  while !out = None && !fuel > 0 do
    decr fuel;
    match codec_verdict ?env c !buf with
    | `Accept -> out := Some !buf
    | `Resize n -> grow n
    | `Reject e -> (
        match repair_for ~seeds ~len:(Bytes.length !buf) e with
        | `Grow n -> grow n
        | `Place (off, seed) -> place off seed
        | `Stuck -> fuel := 0)
  done;
  match !out with Some b -> Some (b, List.rev !placed) | None -> None

let neighbours v =
  let before = if Int64.equal v Int64.min_int then [] else [ Int64.pred v ] in
  let after = if Int64.equal v Int64.max_int then [] else [ Int64.succ v ] in
  before @ (v :: after)

(* Put each named value, and its immediate neighbours, directly into every
   constrained field settled while constructing the seed. These cases expose a
   stale validator whose boundary differs by one without relying on chance. *)
let boundary_inputs seed placed =
  List.concat_map
    (fun (off, (field_seed : Raw.field_seed)) ->
      field_seed.values |> List.concat_map neighbours
      |> List.sort_uniq Int64.compare
      |> List.map (fun value ->
          let b = Bytes.copy seed in
          write_slot b off field_seed.slot value;
          b))
    placed

(* Mutants cross field constraints and the checked wrapper's whole-buffer
   boundary while retaining most of a known-accepted record. *)
let mutate rng seed =
  let len = Bytes.length seed in
  if len = 0 then seed
  else
    match Random.State.int rng 8 with
    | 0 -> Bytes.sub seed 0 (len - 1)
    | 1 when len < max_corpus_input ->
        let b = Bytes.create (len + 1) in
        Bytes.blit seed 0 b 0 len;
        Bytes.set b len (Char.chr (Random.State.int rng 256));
        b
    | kind ->
        let b = Bytes.copy seed in
        let offset = Random.State.int rng len in
        let value = Char.code (Bytes.get b offset) in
        let value' =
          if kind land 1 = 0 then value lxor (1 lsl Random.State.int rng 8)
          else Random.State.int rng 256
        in
        Bytes.set b offset (Char.chr (value' land 0xff));
        b

(* Try a bounded number of fresh byte/parameter combinations for each desired
   accepting seed. An unsolved constraint is reported by the vacuity check
   rather than spinning. *)
let corpus_seeds ?env_of rng ~seeds ~center ~draw_params ~want c =
  let env_of = match env_of with Some f -> f | None -> fun _ -> None in
  let found = ref [] and attempts = ref (4 * want) in
  while List.length !found < want && !attempts > 0 do
    decr attempts;
    let pvals = draw_params () in
    match seed_input ?env:(env_of pvals) rng ~seeds ~center c with
    | Some (b, placed) -> found := (pvals, b, placed) :: !found
    | None -> ()
  done;
  Array.of_list !found

let vacuous_corpus name ~accepts ~rejects =
  Fmt.failwith
    "corpus for %s is vacuous: %d accepted, %d rejected. A differential over \
     it cannot tell the validator from one that answers the same way to every \
     input.@\n\
     %s"
    name accepts rejects
    (if accepts = 0 then
       "Nothing reached the accepting side of the codec's constraints. \
        Wire.Everparse.Raw.field_seeds names values only for equality, \
        inequality, ordering and closed-enum constraints on whole-byte integer \
        fields; any other constraint needs a hand-written corpus."
     else "Every input was accepted; the corpus needs a rejected one.")

(* Seeds, boundary cases, seed mutants, then uniform bytes for breadth. Keep the
   boundary cases ahead of random mutants and preserve exactly [count] lines. *)
let emit_streams ~count ~emit ~rng ~center ~draw_params seeded =
  let emitted = ref 0 in
  Array.iter
    (fun (pvals, b, _) ->
      if !emitted < count then begin
        emit (pvals, b);
        incr emitted
      end)
    seeded;
  Array.iter
    (fun (pvals, b, placed) ->
      List.iter
        (fun boundary ->
          if !emitted < count then begin
            emit (pvals, boundary);
            incr emitted
          end)
        (boundary_inputs b placed))
    seeded;
  let mutant_count =
    if Array.length seeded = 0 then 0
    else min (count / 2) (max 0 (count - !emitted))
  in
  for _ = 1 to mutant_count do
    let pvals, b, _ = seeded.(Random.State.int rng (Array.length seeded)) in
    emit (pvals, mutate rng b);
    incr emitted
  done;
  for _ = 1 to max 0 (count - !emitted) do
    let len = min max_corpus_input (fuzz_length rng center) in
    emit (draw_params (), random_bytes rng len)
  done

let corpus_of_codec ~count ppf (Pack c) =
  let rng = Random.State.make [| 0x5eed51 |] in
  let schema = project ~mode:`Standalone c in
  let name = schema.name in
  let pnames =
    match schema.source with
    | Some source -> Raw.input_param_names source
    | None -> []
  in
  let seeds =
    match schema.source with
    | Some source -> Raw.field_seeds source
    | None -> []
  in
  let center = Wire.Codec.min_wire_size c in
  let draw_params () = List.map (fun _ -> fuzz_param_value rng center) pnames in
  let env_of pvals =
    match pnames with
    | [] -> None
    | _ ->
        Some
          (List.fold_left2
             (fun env name value -> Wire.Param.bind_by_name name value env)
             (Wire.Codec.env c) pnames pvals)
  in
  let accepts = ref 0 and rejects = ref 0 in
  let emit (pvals, b) =
    let pfield =
      match pvals with
      | [] -> "-"
      | _ -> String.concat "," (List.map string_of_int pvals)
    in
    let hex = if Bytes.length b = 0 then "-" else hex_of_bytes b in
    let accepted = codec_accepts ?env:(env_of pvals) c b in
    if accepted then incr accepts else incr rejects;
    Fmt.pf ppf "%s %s %s %d@\n" name pfield hex (if accepted then 1 else 0)
  in
  emit_streams ~count ~emit ~rng ~center ~draw_params
    (corpus_seeds ~env_of rng ~seeds ~center ~draw_params
       ~want:(max 1 (count / 8))
       c);
  if !accepts = 0 || !rejects = 0 then
    vacuous_corpus name ~accepts:!accepts ~rejects:!rejects

let generate_corpus ?(count = 256) ppf codecs =
  if count < 2 then
    Fmt.invalid_arg "Wire_3d.generate_corpus: count must be at least 2";
  List.iter (corpus_of_codec ~count ppf) codecs;
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

(* The 3D struct tag [pp_struct] emits, which is what EverParse mangles into the
   entrypoint name. A raw-module schema has no source struct to read it from, so
   say so rather than guess a symbol that would only fail at link time. *)
let schema_entrypoint_name s =
  match s.source with
  | Some source -> "Wire" ^ Raw.struct_name source
  | None ->
      Fmt.failwith "%s: raw-module schema has no entrypoint struct tag" s.name

(* EverParse names each entrypoint wrapper [<Base>CheckWire<Codec>] and gives
   it the input parameters before the trailing [base, len], so both the helper
   name and its parameter C types follow from the codec alone. Deriving them
   here keeps
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
        ( s.name,
          pascal_case (base ^ "_check_" ^ schema_entrypoint_name s),
          ptypes ))
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

let generate_3d_standalone_check ?name ~outdir ~package codecs =
  let tmpdir = Filename.temp_dir "wire_3d_check" "" in
  Fun.protect
    ~finally:(fun () -> rm_rf tmpdir)
    (fun () ->
      generate_3d_standalone ?name ~outdir:tmpdir ~package codecs;
      let file = standalone_base ?name ~package () ^ ".3d" in
      copy_file
        ~src:(Filename.concat tmpdir file)
        ~dst:(Filename.concat outdir (file ^ ".gen")))

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

(* The standalone [c/] archive is a per-target artefact: a consumer links it
   into a program for the target it was built for, so a cross build must produce
   a target archive and install it into the cross toolchain, exactly as it does
   the host archive for the host toolchain. [emit_standalone_build_rules] gets
   this by building through the context's own toolchain -- [%{cc}], the
   [ocaml-config] partial linker, and the binutils that compiler resolves -- so
   the tools follow the build context rather than a fixed host.

   Two things stay host-only, gated on [%{context_name}] (a cross context is
   named [<host-context>.<target>], so the host is the one still called
   [default]): regenerating the committed [.3d]/C from [3d.exe] (a host
   developer action; the committed C is what a cross build compiles), and the
   [agree] differential test (it runs the built validator, which a cross build
   produces for the target, not the host). *)
let host_context = "(= %{context_name} default)"

(* [(and)] of the host-context gate and [cond], laid out as [dune fmt] prints
   it (the one-line form runs past the margin). *)
let host_context_and cond = Fmt.str "(and\n   %s\n   %s)" host_context cond

(* The committed [.3d] is a plain source that no rule names as a target. The
   install stanza depends on it, so a rule producing it would run on any build
   that installs anything and promote over the file before the drift check in
   [emit_drift_check_rules] compares it. Drift is reported there instead, by
   diffing the committed schema against a scratch [<Base>.3d.gen], and [dune
   promote] writes the new bytes after an intended codec change, reviewed as a
   source diff like anything else.

   [agree.c] is pure OCaml and a scratch target, never promoted, so it
   regenerates on demand. The C needs [3d.exe], so its rule exists only under
   [BUILD_EVERPARSE=1] ([mode promote] writes it back into the tree); a plain
   [dune build] uses the committed C and never invokes [3d.exe], and fails
   loudly if the C was never committed rather than silently shelling out. A
   codec change refreshes the [.3d] and [agree.c] while the committed C goes
   stale -- the runtest differential below catches that, since it regenerates
   the corpus and [agree.c] from the current codec and runs them against the
   committed validator. *)
let emit_standalone_gen_rules ppf ~three_d ~c_files ~provenance =
  Fmt.pf ppf
    "(rule\n\
    \ (enabled_if\n\
    \  %s)\n\
    \ (targets agree.c)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} agree)))\n\n\
     (rule\n\
    \ (alias 3d)\n\
    \ (enabled_if\n\
    \  %s)\n\
    \ (mode promote)\n\
    \ (targets EverParse.h EverParseEndianness.h %s %s)\n\
    \ (deps %s)\n\
    \ (action\n\
    \  (run %%{exe:gen.exe} c)))\n\n"
    host_context
    (host_context_and "(= %{env:BUILD_EVERPARSE=} \"1\")")
    (String.concat " " c_files)
    provenance three_d

(* The C symbols a standalone archive exports: the [<Base>CheckWire<Codec>]
   wrapper for each codec, named exactly as EverParse names it (and as
   [agree.c] links it, see [generate_agree]), so the export allowlist matches
   the real symbol. *)
let wrapper_symbols base codecs =
  List.map
    (fun (Pack c) ->
      let s = project ~mode:`Standalone c in
      pascal_case (base ^ "_check_" ^ schema_entrypoint_name s))
    codecs

(* Post-compile steps that fold the compiled objects into one archive member
   exporting only [wrappers], localizing the raw [<Base>Validate*] entry points
   so their unguarded [StartPosition] (see [emit_standalone_install]) is not
   reachable through the installed archive. macOS localizes during the partial
   link ([ld -r -exported_symbol]); GNU [ld] cannot, so [objcopy] does it after.
   Shared by the emitted dune rule and the symbol-hiding test so they cannot
   drift. *)
let archive_link_steps ~macos ~pack_linker ~objcopy ~ar ~archive ~base ~wrappers
    =
  let libo = base ^ "_lib.o" in
  let objs = Fmt.str "%s.o %sWrapper.o" base base in
  if macos then
    [
      Fmt.str "%s %s %s%s" pack_linker libo objs
        (List.fold_left (fun a w -> a ^ " -exported_symbol _" ^ w) "" wrappers);
      Fmt.str "%s rcs %s %s" ar archive libo;
    ]
  else
    [
      Fmt.str "%s %s %s" pack_linker libo objs;
      Fmt.str "%s%s %s" objcopy
        (List.fold_left
           (fun a w -> a ^ " --keep-global-symbol " ^ w)
           "" wrappers)
        libo;
      Fmt.str "%s rcs %s %s" ar archive libo;
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
    \ (enabled_if\n\
    \  %s)\n\
    \ (targets corpus)\n\
    \ (action\n\
    \  (with-stdout-to corpus (run %%{exe:gen.exe} corpus))))\n\n\
     (rule\n\
    \ (enabled_if\n\
    \  %s)\n\
    \ (targets agree)\n\
    \ (deps agree.c %s EverParse.h EverParseEndianness.h %s.h %sWrapper.h)\n\
    \ (action\n\
    \  (run cc %s %s agree.c %s -o agree)))\n\n\
     (rule\n\
    \ (alias runtest)\n\
    \ (enabled_if\n\
    \  %s)\n\
    \ (deps corpus agree)\n\
    \ (action\n\
    \  (run %%{dep:agree} corpus)))\n\n"
    host_context host_context archive base base strict_cc_flags
    everparse_type_defines archive host_context

(* Build the validator into an archive, installed with the package, so consumers
   get a ready-to-link library and a downstream build fails loudly if the spec
   stops projecting to compilable C. The archive exports only the checked
   [<Base>CheckWire<Codec>] wrappers and localizes the raw validators.

   The build runs in every context through that context's own toolchain, so a
   cross build produces a target archive: [CC] is the context C compiler
   ([ocaml-config:c_compiler]), the partial link is the [ocaml-config] pack
   linker, and the symbol-hiding [objcopy]/[ar] are the ones that compiler
   resolves ([-print-prog-name]) -- a cross [cc] finds the target's binutils. A
   shell runs the whole thing so that command substitution can resolve the
   binutils; there is no dune variable for [objcopy] or [ar]. The localizing step
   is still platform-specific ([macos] localizes during the partial link,
   elsewhere [objcopy] does it after), keyed on [ocaml-config:system]: for the
   host context it names the host, for a cross context the target, which is what
   selects the right localize path either way. *)
let emit_standalone_build_rules ppf ~base ~archive ~c_files ~wrappers =
  let compile cc =
    Fmt.str "%s %s %s -c %s.c %sWrapper.c" cc strict_cc_flags
      everparse_type_defines base base
  in
  let emit_rule ~cond ~macos =
    let steps =
      compile "\"$CC\""
      :: archive_link_steps ~macos
           ~pack_linker:"%{ocaml-config:native_pack_linker}"
           ~objcopy:"\"$(\"$CC\" -print-prog-name=objcopy)\""
           ~ar:"\"$(\"$CC\" -print-prog-name=ar)\"" ~archive ~base ~wrappers
    in
    let script =
      "set -e; CC=%{ocaml-config:c_compiler}; " ^ String.concat "; " steps
    in
    (* The script is one dune-quoted string argument to [sh -c]; its own double
       quotes (around [$CC] and the [$(...)] tool lookups) are escaped so they
       do not close the dune string. *)
    let quoted = String.concat "\\\"" (String.split_on_char '"' script) in
    Fmt.pf ppf
      "(rule\n\
      \ (targets %s)\n\
      \ (enabled_if\n\
      \  %s)\n\
      \ (deps EverParse.h EverParseEndianness.h %s)\n\
      \ (action\n\
      \  (run sh -c \"%s\")))\n\n"
      archive cond
      (String.concat " " c_files)
      quoted
  in
  emit_rule ~cond:"(= %{ocaml-config:system} macosx)" ~macos:true;
  emit_rule ~cond:"(<> %{ocaml-config:system} macosx)" ~macos:false;
  emit_standalone_check_rules ppf ~base ~archive

(* Install only the checked wrapper header, not the raw validator header. The
   [<Base>Validate*] entrypoints in [<Base>.h] take a [StartPosition] and their
   EverParse-emitted preamble bounds a read as [N <= InputLength - StartPosition]
   with no prior [StartPosition <= InputLength] check, so a direct C caller
   passing [StartPosition > InputLength] underflows the span (unsigned) and reads
   out of bounds. The generated wrapper [<Base>CheckWire<Codec>(base, len)]
   always validates from position 0 and is the safe public C API; the raw
   validators stay build-internal, linked into the archive but not shipped as a
   header. [<Base>Wrapper.h] does not include [<Base>.h], so this compiles
   standalone. *)
let emit_standalone_install ppf ~package ~three_d ~archive ~public_header
    ~provenance =
  (* Dune currently adds files from a disabled install stanza to the directory's
     [all] alias. Keep the stanza unconditional until
     https://github.com/ocaml/dune/issues/15825 is fixed; the archive itself is
     intentionally built for every context above. *)
  let pr fmt = Fmt.pf ppf fmt in
  pr "(install\n (package %s)\n (section lib)\n (files\n" package;
  List.iter
    (fun f -> pr "  (%s as c/%s)\n" f f)
    [ three_d; archive; public_header; provenance ];
  pr "  (EverParse.h as c/EverParse.h)\n";
  pr "  (EverParseEndianness.h as c/EverParseEndianness.h)))\n"

let generate_dune_standalone_file ~filename ?name ~outdir ~package codecs =
  let base = standalone_base ?name ~package () in
  let three_d = base ^ ".3d" in
  let c_files =
    [ base ^ ".c"; base ^ ".h"; base ^ "Wrapper.c"; base ^ "Wrapper.h" ]
  in
  let archive = "lib" ^ String.lowercase_ascii base ^ ".a" in
  let wrappers = wrapper_symbols base codecs in
  let provenance = provenance_file three_d in
  let oc = open_out (Filename.concat outdir filename) in
  let ppf = Format.formatter_of_out_channel oc in
  emit_standalone_gen_rules ppf ~three_d ~c_files ~provenance;
  emit_drift_check_rules ppf [ three_d ];
  emit_provenance_check_rules ppf [ three_d ];
  emit_standalone_build_rules ppf ~base ~archive ~c_files ~wrappers;
  emit_standalone_install ppf ~package ~three_d ~archive
    ~public_header:(base ^ "Wrapper.h") ~provenance;
  Format.pp_print_flush ppf ();
  close_out oc

let generate_dune_standalone ?name ~outdir ~package codecs =
  generate_dune_standalone_file ~filename:"dune.inc" ?name ~outdir ~package
    codecs

let main ?name ~mode ~package codecs =
  let argv = Array.to_list Sys.argv in
  match mode with
  | `Ffi -> (
      let schemas = List.map (fun (Pack c) -> project ~mode:`Ffi c) codecs in
      match argv with
      | [ _; "3d" ] -> generate_3d ~outdir:"." schemas
      | [ _; "3d-gen" ] -> generate_3d_check ~outdir:"." schemas
      | [ _; "c" ] -> generate_c ~outdir:"." schemas
      | [ _; "dune" ] -> generate_dune ~outdir:"." ~package schemas
      | [ _; "dune-gen" ] ->
          generate_dune_file ~filename:"dune.inc.gen" ~outdir:"." ~package
            schemas
      | [ _; "provenance-check" ] ->
          check_provenance ~outdir:"."
            (List.map Wire.Everparse.filename schemas)
      | _ -> run ~outdir:"." schemas)
  | `Standalone -> (
      match argv with
      | [ _; "3d" ] -> generate_3d_standalone ?name ~outdir:"." ~package codecs
      | [ _; "3d-gen" ] ->
          generate_3d_standalone_check ?name ~outdir:"." ~package codecs
      | [ _; "c" ] -> generate_c_standalone ?name ~outdir:"." ~package ()
      | [ _; "agree" ] -> generate_agree ?name ~outdir:"." ~package codecs
      | [ _; "dune" ] ->
          generate_dune_standalone ?name ~outdir:"." ~package codecs
      | [ _; "dune-gen" ] ->
          generate_dune_standalone_file ~filename:"dune.inc.gen" ?name
            ~outdir:"." ~package codecs
      | [ _; "provenance-check" ] ->
          check_provenance ~outdir:"."
            [ standalone_base ?name ~package () ^ ".3d" ]
      | [ _; "corpus" ] -> generate_corpus Format.std_formatter codecs
      | _ -> generate_standalone ?name ~outdir:"." ~package codecs)
