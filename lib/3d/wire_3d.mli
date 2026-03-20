(** Tooling over {!Wire.C.t}.

    [Wire_3d] does not define another schema language. It takes exported schemas
    from {!Wire.C}, writes their [.3d] files, invokes EverParse, and generates C
    parser artifacts around the result.

    The output directory contains a self-contained C library: [EverParse.h],
    [<Name>.h], [<Name>.c], and a [test.c] that exercises the validators.

    {b Typical usage} ([gen.ml]):
    {[
      let () = Wire_3d.main ~package:"clcw" [ Wire.C.schema Clcw.codec ]
    ]}

    With a minimal [dune] that includes the generated rules:
    {v
      (executable (name gen) (modules gen) (libraries clcw wire.3d))
      (rule (mode promote) (alias gen)
       (targets dune.inc) (deps gen.exe) (action (run ./gen.exe dune)))
      (include dune.inc)
    v}

    If you want OCaml to call the generated C validators, use {!Wire_stubs} on
    the resulting {!Wire.C.Raw.struct_} values. *)

type schema = Wire.C.t
(** Same as {!Wire.C.t}; [Wire_3d] is tooling over this exported schema. *)

val schema_of_struct : Wire.C.Raw.struct_ -> schema
(** [schema_of_struct s] creates a schema from a fixed-size struct. The name,
    module, and wire size are derived from the struct. Raises [Failure] if the
    struct has variable-length fields. *)

val schema :
  name:string -> module_:Wire.C.Raw.module_ -> wire_size:int -> schema
(** [schema ~name ~module_ ~wire_size] creates a schema with explicit
    parameters. Prefer {!schema_of_struct} when possible. *)

val everparse_name : string -> string
(** [everparse_name name] returns the EverParse-normalized identifier for a
    struct name. EverParse 3D normalizes names that start with two or more
    consecutive uppercase letters by lowercasing the whole name and capitalizing
    only the first letter (e.g., [CLCW] becomes [Clcw], [TMFrame] becomes
    [Tmframe]). Names with standard camelCase are preserved. *)

val generate_3d : outdir:string -> schema list -> unit
(** [generate_3d ~outdir schemas] generates [.3d] files from Wire modules. *)

val run_everparse : ?quiet:bool -> outdir:string -> schema list -> unit
(** [run_everparse ?quiet ~outdir schemas] invokes EverParse on [.3d] files in
    [outdir].

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

    Requires [3d.exe] in PATH. *)

val generate_c : ?quiet:bool -> outdir:string -> schema list -> unit
(** [generate_c ?quiet ~outdir schemas] invokes EverParse on existing [.3d]
    files to produce C parsers and generates [test.c].

    If [quiet] is [true] (the default), EverParse output is suppressed. If
    [quiet] is [false], EverParse stdout/stderr are left visible.

    Requires [3d.exe] (EverParse) in PATH. *)

val generate : ?quiet:bool -> outdir:string -> schema list -> unit
(** [generate ?quiet ~outdir schemas] runs both steps: {!generate_3d} then
    {!generate_c}. The [quiet] flag is passed through to EverParse execution. *)

val main : package:string -> schema list -> unit
(** [main ~package schemas] dispatches based on [Sys.argv]:
    - [3d] runs {!generate_3d}
    - [c] runs {!generate_c}
    - [dune] generates [dune.inc] with build rules, test, and install stanzas
    - otherwise runs {!generate}. *)

val has_3d_exe : unit -> bool
(** [has_3d_exe ()] returns [true] if [3d.exe] is available in PATH or
    [~/.local/everparse/bin/]. *)

val ensure_dir : string -> unit
(** [ensure_dir path] creates the directory [path] if it does not exist. *)
