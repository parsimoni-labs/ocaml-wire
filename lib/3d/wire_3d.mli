(** Tooling over {!Wire.Everparse.t}.

    [Wire_3d] does not define another schema language. It takes exported schemas
    from {!Wire.Everparse}, writes their [.3d] files, invokes EverParse, and
    generates C parser artifacts around the result.

    The output directory contains a self-contained C library: [EverParse.h],
    [<Name>.h], [<Name>.c], and a [test.c] that exercises the validators.

    {b Typical usage} ([gen.ml]):
    {[
    open Wire

    type header = { version : int; length : int }

    let codec =
      Codec.v "Header"
        (fun version length -> { version; length })
        Codec.
          [
            (Field.v "Version" (bits ~width:4 U8) $ fun h -> h.version);
            (Field.v "Length" uint16be $ fun h -> h.length);
          ]

    let main () = Wire_3d.main ~mode:`Ffi ~package:"hdr" [ Wire_3d.pack codec ]
    ]}

    [mode] is mandatory: [`Ffi] emits the per-codec FFI artifacts above, [`Doc]
    a single FFI-free [<Package>.3d] spec and validator for the whole package.
    The codecs are the same either way.

    With a minimal [dune] that includes the generated rules:
    {v
      (executable (name gen) (modules gen) (libraries clcw wire.3d))
      (rule (mode promote) (alias 3d)
       (targets dune.inc) (deps gen.exe) (action (run ./gen.exe dune)))
      (include dune.inc)
    v}

    If you want OCaml to call the generated C validators, use {!Wire_stubs} on
    the resulting {!type:Wire.Everparse.Raw.struct_} values. *)

val everparse_name : string -> string
(** [everparse_name name] returns the EverParse-normalized identifier for a
    struct name. EverParse 3D normalizes names that start with two or more
    consecutive uppercase letters by lowercasing the whole name and capitalizing
    only the first letter (e.g., [CLCW] becomes [Clcw], [TMFrame] becomes
    [Tmframe]). Names with standard camelCase are preserved. *)

val generate_3d : outdir:string -> Wire.Everparse.t list -> unit
(** [generate_3d ~outdir schemas] generates [.3d] files from Wire modules. *)

val generate_dune :
  outdir:string -> package:string -> Wire.Everparse.t list -> unit
(** [generate_dune ~outdir ~package schemas] writes [dune.inc] listing the
    build, runtest, and install rules for the generated C artifacts. *)

val run_everparse :
  ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
(** [run_everparse ?quiet ~outdir schemas] invokes EverParse on [.3d] files in
    [outdir].

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

    Requires [3d.exe] in PATH. *)

val parse_3d : ?batch:bool -> outdir:string -> string -> (unit, string) result
(** [parse_3d ~outdir file] runs [3d.exe] on a single [.3d] file in [outdir].
    Returns [Ok ()] when 3D accepts the file, or [Error stderr] with the
    captured error message otherwise.

    With [~batch:false] (the default) it stops after the 3D type-checker, before
    F* and KaRaMeL: fast, but it does not catch kind errors that only fail F*
    verification (e.g. a list over a possibly-empty element). With [~batch:true]
    it runs the full [--batch] verification (F* and C extraction), catching
    those, and discarding the generated C. Unlike {!run_everparse} it does no
    endianness-header copy, so it works on a bare output directory. Intended for
    projection coverage tests.

    Requires [3d.exe] in PATH. *)

val batch_check :
  outdir:string -> Wire.Everparse.t list -> (unit, string) result
(** [batch_check ~outdir schemas] generates one [.3d] per schema in [outdir] and
    validates them all in a single [3d.exe --batch] invocation (full F* and C
    extraction), the way EverParse's own corpus is tested. Because the F* /
    KaRaMeL startup cost is paid once per invocation, this is far faster than a
    {!parse_3d} call per schema for a large set. Returns [Ok ()] iff EverParse
    accepts every schema, else [Error] with the captured diagnostics naming the
    offending file(s). Schemas must have distinct names (one [.3d] module each).
    Requires [3d.exe] in PATH. *)

val write_external_typedefs : outdir:string -> Wire.Everparse.t list -> unit
(** [write_external_typedefs ~outdir schemas] writes the default
    [<Name>_ExternalTypedefs.h] for each schema that uses the WireCtx contract,
    declaring [WIRECTX] as a forward reference to the matching [<Name>Fields]
    plug struct. *)

val write_fields : outdir:string -> Wire.Everparse.t list -> unit
(** [write_fields ~outdir schemas] writes the default [<Name>_Fields.{c,h}] plug
    for each schema that uses the WireCtx contract: a typed struct (one member
    per named field) and the [<Name>Set*] switch dispatchers that populate it.
*)

val generate_c : ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
(** [generate_c ?quiet ~outdir schemas] invokes EverParse on existing [.3d]
    files to produce C parsers and generates [test.c].

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

    Requires [3d.exe] (EverParse) in PATH. *)

val run : ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
(** [run ?quiet ~outdir schemas] runs the full pipeline: writes [.3d] files,
    invokes EverParse, and produces C validators. The [quiet] flag is passed
    through to EverParse execution. *)

type packed
(** A codec with its type erased, so codecs of different record types share one
    list. Build with {!pack}. {!main} and the doc helpers take these rather than
    already-projected {!Wire.Everparse.t}, so the [doc]-vs-[schema] projection
    is chosen from the mode, not at the call site. *)

val pack : 'a Wire.Codec.t -> packed
(** [pack codec] erases [codec]'s type for a {!packed} list. *)

val generate_doc :
  ?quiet:bool ->
  ?name:string ->
  outdir:string ->
  package:string ->
  packed list ->
  unit
(** [generate_doc ?quiet ?name ~outdir ~package codecs] runs the documentation
    pipeline: project each codec with {!Wire.Everparse.doc}, merge them into one
    [<Name>.3d], and (when [3d.exe] is available) compile it to a single
    validator-only [<Name>.c] (no [_Fields] plug, no FFI). The file base is
    [name] when given, else [package], normalised to a CamelCase identifier
    (["my-pkg"] becomes [MyPkg]); [package] always names the opam package. *)

val generate_dune_doc :
  ?name:string -> outdir:string -> package:string -> packed list -> unit
(** [generate_dune_doc ?name ~outdir ~package codecs] writes a [dune.inc] for
    the single-file documentation build: rules that emit [<Name>.3d] and compile
    it to [<Name>.c], a rule that builds the validator into an installed
    [lib<name>.a] archive, a [runtest] rule that runs the differential
    self-check (see {!generate_corpus} / {!generate_agree}), and an install
    stanza (under opam [package]) for the spec, parser, and archive. [name]
    defaults to [package]; see {!generate_doc}. *)

val generate_corpus : ?count:int -> Format.formatter -> packed list -> unit
(** [generate_corpus ?count ppf codecs] prints, for each codec, [count] fuzzed
    inputs as [<codec> <hex> <verdict>] lines, where [verdict] is [1] when the
    OCaml codec accepts the bytes (decodes and validates) and [0] otherwise, and
    [<hex>] is [-] for the empty input. Lengths are biased around each codec's
    minimum size so the corpus straddles the accept/reject boundary. This is the
    oracle half of the doc pipeline's differential self-check; {!main}'s
    [corpus] subcommand calls it on stdout. *)

val generate_agree :
  ?name:string -> outdir:string -> package:string -> packed list -> unit
(** [generate_agree ?name ~outdir ~package codecs] writes [agree.c] into
    [outdir]: a C program that replays a {!generate_corpus} corpus through the
    EverParse-generated validators and exits nonzero on any input where the
    validator's accept/reject decision differs from the recorded verdict. It
    reads the [<Name>Check<Codec>] helper names from the generated
    [<Name>Wrapper.h], so {!generate_c_doc} (or [3d.exe]) must have run first.
*)

val main :
  ?name:string -> mode:[ `Ffi | `Doc ] -> package:string -> packed list -> unit
(** [main ?name ~mode ~package codecs] dispatches based on [Sys.argv]:
    - [3d] writes the [.3d] file(s)
    - [c] produces the C parser(s)
    - [dune] generates [dune.inc] with build rules, test, and install stanzas
    - otherwise runs the full pipeline.

    [mode] is mandatory, so every [gen.ml] states what it emits. With
    [~mode:`Ffi] it projects each codec with {!Wire.Everparse.schema} and drives
    the multi-file FFI pipeline, one set per codec. With [~mode:`Doc] it
    projects with {!Wire.Everparse.doc} and drives the single-file documentation
    pipeline, one [<Name>.3d] and [<Name>.c] for the whole family. The codecs
    are the same either way; only the mode changes.

    [name] overrides the doc file base (see {!generate_doc}); it has no effect
    in [`Ffi] mode, whose file names come from the individual codecs. *)

val has_3d_exe : unit -> bool
(** [has_3d_exe ()] returns [true] if [3d.exe] is available in PATH or
    [~/.local/everparse/bin/]. *)

val ensure_dir : string -> unit
(** [ensure_dir path] creates the directory [path] if it does not exist. *)

val strict_cc_flags : string
(** The strict-C11 [cc] flags the generated dune rule compiles the EverParse C
    with (and the e2e test mirrors), so the generated validators are checked
    under the same standard a downstream caller would use. *)

val everparse_type_defines : string
(** [everparse_type_defines] are the preprocessor [-D] flags mapping each 3D
    integer type ([UINT16BE], ...) to its host-order C type, needed to compile
    the wrapper of a parameterized validator (whose parameters EverParse types
    with the 3D names). Harmless for non-parameterized wrappers. *)
