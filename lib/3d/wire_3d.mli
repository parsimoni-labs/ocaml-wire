(** Tooling over {!Wire.Everparse.t}.

    [Wire_3d] does not define another schema language. It takes exported schemas
    from {!Wire.Everparse}, writes their [.3d] files, invokes EverParse, and
    generates C parser artifacts around the result.

    The output directory contains a self-contained C library: [EverParse.h],
    [<Name>.h], [<Name>.c], and a [test.c] that exercises the validators.

    {b Typical usage} ([gen.ml]):
    {[
      let () = Wire_3d.main ~package:"clcw" [ Wire.Everparse.schema Clcw.codec ]
    ]}

    With a minimal [dune] that includes the generated rules:
    {v
      (executable (name gen) (modules gen) (libraries clcw wire.3d))
      (rule (mode promote) (alias gen)
       (targets dune.inc) (deps gen.exe) (action (run ./gen.exe dune)))
      (include dune.inc)
    v}

    If you want OCaml to call the generated C validators, use {!Wire_stubs} on
    the resulting {!Wire.Everparse.Raw.struct_} values. *)

val everparse_name : string -> string
(** [everparse_name name] returns the EverParse-normalized identifier for a
    struct name. EverParse 3D normalizes names that start with two or more
    consecutive uppercase letters by lowercasing the whole name and capitalizing
    only the first letter (e.g., [CLCW] becomes [Clcw], [TMFrame] becomes
    [Tmframe]). Names with standard camelCase are preserved. *)

val generate_3d : outdir:string -> Wire.Everparse.t list -> unit
(** [generate_3d ~outdir schemas] generates [.3d] files from Wire modules. *)

val run_everparse :
  ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
(** [run_everparse ?quiet ~outdir schemas] invokes EverParse on [.3d] files in
    [outdir].

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

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

val main : package:string -> Wire.Everparse.t list -> unit
(** [main ~package schemas] dispatches based on [Sys.argv]:
    - [3d] runs {!generate_3d}
    - [c] runs {!generate_c}
    - [dune] generates [dune.inc] with build rules, test, and install stanzas
    - otherwise runs {!run}. *)

val has_3d_exe : unit -> bool
(** [has_3d_exe ()] returns [true] if [3d.exe] is available in PATH or
    [~/.local/everparse/bin/]. *)

val ensure_dir : string -> unit
(** [ensure_dir path] creates the directory [path] if it does not exist. *)
