(** Tooling over {!Wire.Everparse.t}.

    [Wire_3d] does not define another schema language. It takes exported schemas
    from {!Wire.Everparse}, writes their [.3d] files, invokes EverParse, and
    generates C parser artifacts around the result.

    The output directory contains a self-contained C library: [EverParse.h],
    [<Name>.h], [<Name>.c], a [<Name>.provenance] stamp, and a [test.c] that
    exercises the validators.

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

    [mode] is mandatory: [`Ffi] emits the per-codec FFI artifacts above,
    [`Standalone] a single FFI-free [<Package>.3d] spec and validator for the
    whole package. The codecs are the same either way.

    With a minimal [dune] that includes the generated rules:
    {v
      (executable (name gen) (libraries clcw wire.3d))
      (rule (mode promote) (alias 3d)
       (targets dune.inc) (action (run %{exe:gen.exe} dune)))
      (include dune.inc)
    v}

    If you want OCaml to call the generated C validators, use {!Wire_stubs} on
    the resulting {!type:Wire.Everparse.Raw.struct_} values. *)

val everparse_name : string -> string
(** [everparse_name name] returns the C identifier EverParse derives from a
    struct name: underscores are stripped and the [_]-separated segments are
    joined in CamelCase. Each segment is capitalized and any internal run of two
    or more consecutive uppercase letters collapses to a single one. E.g.
    [Grpc_message] becomes [GrpcMessage], [EP_Header] becomes [EpHeader], [CLCW]
    becomes [Clcw], and [TMFrame] becomes [Tmframe]. *)

val pascal_case : string -> string
(** [pascal_case s] is EverParse's own identifier mangling, transcribed from
    [pascal_case] in EverParse's [src/3d/Target.fst]. It drops underscores and
    CamelCases, lowercasing every character that follows an uppercase letter or
    a digit until the next lower-case one. EverParse names a validator wrapper
    [pascal_case (module ^ "_check_" ^ codec)], so use this (not
    {!everparse_name}) to predict a wrapper symbol exactly: e.g.
    [pascal_case "tpm_check_TPM2B"] is ["TpmCheckTpm2b"] and
    [pascal_case "virtio_check_Virtq_desc"] is ["VirtioCheckVirtqDesc"]. *)

val generate_3d : outdir:string -> Wire.Everparse.t list -> unit
(** [generate_3d ~outdir schemas] generates [.3d] files from Wire modules.

    Raises [Invalid_argument] if two schemas would generate the same [.3d] file
    or the same C identifier, naming both codecs: the second write would
    otherwise silently replace the first, leaving that codec's FFI stubs running
    against another spec's verified C. Names collide after capitalization
    ([header] and [Header]) and after EverParse's identifier mangling ([TMFrame]
    and [Tmframe]). The same check guards {!generate_c}, {!generate_dune},
    {!write_external_typedefs} and {!write_fields}. *)

val generate_dune :
  outdir:string -> package:string -> Wire.Everparse.t list -> unit
(** [generate_dune ~outdir ~package schemas] writes [dune.inc] listing the
    build, runtest, and install rules for the generated C artifacts. The
    [runtest] rules regenerate every committed [.3d] file and [dune.inc] into
    [.gen] targets and diff them, so source changes cannot leave those hermetic
    artifacts stale. *)

val run_everparse :
  ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
(** [run_everparse ?quiet ~outdir schemas] invokes EverParse on [.3d] files in
    [outdir].

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

    A [<Name>.provenance] file records the BLAKE2b-256 digest of [<Name>.3d] and
    the EverParse version that generated the C. Generated [dune.inc] rules
    recompute that digest during [runtest] and fail on a mismatch, so C
    staleness is detected without running EverParse. The failure is deliberately
    not promotable: only regenerating the C may refresh the stamp.

    The executable, output directory, and schema filenames are passed directly
    to EverParse without shell interpretation.

    Requires [3d.exe] in PATH. *)

val check_provenance : outdir:string -> string list -> unit
(** [check_provenance ~outdir three_d_files] recomputes the BLAKE2b-256 digest
    of each [.3d] in [outdir] and raises [Failure] if it differs from the digest
    recorded in the matching [<Name>.provenance] stamp, which means the
    committed C came from a different spec. Needs no [3d.exe]. *)

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
    projection coverage tests. The output directory and filename are passed
    without shell interpretation.

    Requires [3d.exe] in PATH. *)

val fork_pool : max_jobs:int -> (unit -> unit) array -> bool array
(** [fork_pool ~max_jobs jobs] runs each job in its own process, at most
    [max_jobs] at a time, returning each job's success (exit 0 with no
    exception) in input order. Used to overlap the per-module EverParse runs,
    whose cost is dominated by F* verification. Each job that invokes [3d.exe]
    must use a private working directory, as concurrent runs race on EverParse's
    shared intermediate files. *)

val rm_rf : string -> unit
(** [rm_rf dir] removes [dir] and its files (a flat output directory), ignoring
    errors. A cleanup helper for the private per-run directories used with
    {!fork_pool}. *)

val batch_check :
  ?max_jobs:int ->
  outdir:string ->
  Wire.Everparse.t list ->
  (unit, string) result
(** [batch_check ~outdir schemas] verifies every schema through EverParse (one
    [.3d] module each, accepted iff F* verifies it), the way EverParse's own
    corpus is tested. The cost is dominated by per-module F* verification
    (CPU-bound, several seconds each) with negligible startup, so the schemas
    are verified concurrently in a {!fork_pool} (at most [max_jobs] at once,
    default a small multiple of the cores) rather than serially in one
    [3d.exe --batch]. Each run uses a private directory. Returns [Ok ()] iff
    EverParse accepts every schema, else [Error] naming the offending schema(s)
    with their captured diagnostics. Schemas must have distinct names (one [.3d]
    module each; {!generate_3d} rejects a collision). Paths and filenames are
    passed without shell interpretation. Requires [3d.exe] in PATH. *)

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

    The EverParse-emitted [<Name>CheckWire<Codec>] wrapper is hardened after
    generation: it returns [FALSE] unless the validator consumed the whole
    buffer, so a valid record followed by trailing bytes is rejected. The raw
    [<Name>ValidateWire<Codec>] function keeps EverParse's prefix semantics and
    returns the consumed position.

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

    Requires [3d.exe] (EverParse) in PATH. *)

val run : ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
(** [run ?quiet ~outdir schemas] runs the full pipeline: writes [.3d] files,
    invokes EverParse, and produces C validators. The [quiet] flag is passed
    through to EverParse execution. *)

type packed
(** A codec with its type erased, so codecs of different record types share one
    list. Build with {!pack}. {!main} and the standalone helpers take these
    rather than an already-projected {!Wire.Everparse.t}, so the [`Ffi]-vs-
    [`Standalone] projection mode is chosen by {!main}, not at the call site. *)

val pack : 'a Wire.Codec.t -> packed
(** [pack codec] erases [codec]'s type for a {!packed} list. *)

val generate_standalone :
  ?quiet:bool ->
  ?name:string ->
  outdir:string ->
  package:string ->
  packed list ->
  unit
(** [generate_standalone ?quiet ?name ~outdir ~package codecs] runs the
    documentation pipeline: project each codec in documentation mode, merge them
    into one [<Name>.3d], and (when [3d.exe] is available) compile it to a
    single validator-only [<Name>.c] (no [_Fields] plug, no FFI). The file base
    is [name] when given, else [package], normalised to a CamelCase identifier
    (["my-pkg"] becomes [MyPkg]); [package] always names the opam package. The
    generated [<Name>CheckWire<Codec>] wrappers validate the whole buffer,
    rejecting trailing bytes (see {!generate_c}). Only the checked wrapper
    header [<Name>Wrapper.h] is installed as the public C API; the raw
    [<Name>Validate*] entrypoints (which take an unguarded [StartPosition]) stay
    build-internal, linked into the archive but not shipped as a header. *)

val generate_dune_standalone :
  ?name:string -> outdir:string -> package:string -> packed list -> unit
(** [generate_dune_standalone ?name ~outdir ~package codecs] writes a [dune.inc]
    for the single-file documentation build: a rule that compiles the committed
    [<Name>.3d] to [<Name>.c], a rule that builds the validator into an
    installed [lib<name>.a] archive, a [runtest] rule that runs the differential
    self-check (see {!generate_corpus} / {!generate_agree}), and an install
    stanza (under opam [package]) for the spec, parser, and archive. The
    [runtest] rules also regenerate the committed [.3d] and [dune.inc] into
    [.gen] targets and diff them; neither committed file is a rule target, so a
    schema that drifted from its codec fails [runtest] instead of being
    overwritten, and [dune promote] writes the new bytes on request. [name]
    defaults to [package]; see {!generate_standalone}.

    The [c/] archive builds and installs in every dune context through that
    context's own toolchain ([%{ocaml-config:c_compiler}], the [ocaml-config]
    partial linker, and the binutils that compiler resolves), so a cross build
    produces and installs a target-native archive rather than the host's. Only
    the host-side steps are gated on [(= %{context_name} default)]: regenerating
    the committed [.3d]/C from [3d.exe], and the [agree] differential test,
    which runs a built validator on the build machine. *)

val wrapper_symbols : string -> packed list -> string list
(** [wrapper_symbols base codecs] is the [<Base>CheckWire<Codec>] wrapper C
    symbol for each codec, computed exactly as EverParse names them. These are
    the only symbols a standalone archive exports; the raw [<Base>Validate*]
    validators are localized (see {!archive_link_steps}). *)

val archive_link_steps :
  macos:bool ->
  pack_linker:string ->
  objcopy:string ->
  ar:string ->
  archive:string ->
  base:string ->
  wrappers:string list ->
  string list
(** [archive_link_steps ~macos ~pack_linker ~objcopy ~ar ~archive ~base
     ~wrappers] is the post-compile shell commands that fold [<base>.o] and
    [<base>Wrapper.o] into [archive] exporting only [wrappers] and localizing
    the raw validators. Platform specific ([macos] localizes during the partial
    link with [pack_linker], elsewhere [objcopy] does it after). The tool
    commands are parameters so the emitted dune rule can pass the build
    context's toolchain (cross tools in a cross context) while the symbol-hiding
    test passes the host's; both share this one definition of the localize
    logic. *)

val generate_corpus : ?count:int -> Format.formatter -> packed list -> unit
(** [generate_corpus ?count ppf codecs] prints, for each codec, [count] inputs
    as [<codec> <params> <hex> <verdict>] lines, where [verdict] is [1] when the
    OCaml codec accepts the bytes (decodes, validates, and the record spans the
    whole input -- matching the hardened [Check] wrapper's whole-buffer
    contract) and [0] otherwise, [<params>] is the comma-separated input
    parameter values ([-] when the codec takes none), and [<hex>] is [-] for the
    empty input. [count] must be at least two. This is the oracle half of the
    doc pipeline's differential self-check; {!main}'s [corpus] subcommand calls
    it on stdout.

    The inputs come from four streams: accepting seeds, built by writing the
    values each constrained field's own declaration names (see
    {!Wire.Everparse.Raw.field_seeds}) at the offsets the codec's parse errors
    report; boundary inputs, each seed with one constrained field set to each of
    those values and its immediate neighbours; mutants of the seeds; and
    uniformly drawn bytes at lengths biased around the codec's minimum size.

    Uniform bytes alone are vacuous for a codec that pins a field:
    [magic == 0x53504F53] leaves one accepting value in [2^32], so every input
    is a reject and the differential holds equally for a validator that rejects
    everything. [generate_corpus] therefore raises [Failure] rather than emit a
    corpus with nothing on one side of the accept/reject boundary, naming the
    codec and tally. A constraint {!Wire.Everparse.Raw.field_seeds} cannot
    describe needs a hand-written corpus. *)

val generate_agree :
  ?name:string -> outdir:string -> package:string -> packed list -> unit
(** [generate_agree ?name ~outdir ~package codecs] writes [agree.c] into
    [outdir]: a C program that replays a {!generate_corpus} corpus through the
    EverParse-generated validators and exits nonzero on any input where the
    validator's accept/reject decision differs from the recorded verdict. It
    reads the [<Name>CheckWire<Codec>] helper names from the generated
    [<Name>Wrapper.h], so {!generate_standalone} (or [3d.exe]) must have run
    first. *)

val main :
  ?name:string ->
  mode:[ `Ffi | `Standalone ] ->
  package:string ->
  packed list ->
  unit
(** [main ?name ~mode ~package codecs] dispatches based on [Sys.argv]:
    - [3d] writes the [.3d] file(s)
    - [c] produces the C parser(s)
    - [dune] generates [dune.inc] with build rules, test, and install stanzas
    - [3d-gen] / [dune-gen] write the corresponding [.gen] drift-check targets
    - [provenance-check] verifies the committed provenance stamps, no EverParse
    - otherwise runs the full pipeline.

    [mode] is mandatory, so every [gen.ml] states what it emits. With
    [~mode:`Ffi] it projects each codec with {!Wire.Everparse.project} in [`Ffi]
    mode and drives the multi-file FFI pipeline, one set per codec. With
    [~mode:`Standalone] it projects in [`Standalone] mode and drives the
    single-file pipeline, one [<Name>.3d] and [<Name>.c] standalone parser for
    the whole family. The codecs are the same either way; only the mode changes.

    [name] overrides the standalone file base (see {!generate_standalone}); it
    has no effect in [`Ffi] mode, whose file names come from the individual
    codecs. *)

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
