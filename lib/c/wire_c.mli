(** Generate verified C libraries from Wire codecs via EverParse.

    Given Wire schemas, generates EverParse .3d files and invokes EverParse to
    produce pure C parsers and validators. The output directory contains a
    self-contained C library: [EverParse.h], [<Name>.h], [<Name>.c], and a
    [test.c] that exercises the validators.

    {b Typical usage} ([gen.ml]):
    {[
      let () =
        Wire_c.main ~package:"clcw"
          [
            Wire_c.schema ~name:"Clcw" ~module_:Clcw.module_
              ~wire_size:(Wire.Codec.wire_size Clcw.codec);
          ]
    ]}

    With a minimal [dune] that includes the generated rules:
    {v
      (executable (name gen) (modules gen) (libraries clcw wire.c))
      (rule (mode promote) (alias gen)
       (targets dune.inc) (deps gen.exe) (action (run ./gen.exe dune)))
      (include dune.inc)
    v} *)

type schema
(** A schema bundles a name, Wire module, and wire size. *)

val schema : name:string -> module_:Wire.module_ -> wire_size:int -> schema
(** [schema ~name ~module_ ~wire_size] creates a schema for C library
    generation. *)

val generate_3d : outdir:string -> schema list -> unit
(** [generate_3d ~outdir schemas] generates [.3d] files from Wire modules. *)

val generate_c : outdir:string -> schema list -> unit
(** [generate_c ~outdir schemas] invokes EverParse on existing [.3d] files to
    produce C parsers and generates [test.c].

    Requires [3d.exe] (EverParse) in PATH. *)

val generate : outdir:string -> schema list -> unit
(** [generate ~outdir schemas] runs both steps: {!generate_3d} then
    {!generate_c}. *)

val main : package:string -> schema list -> unit
(** [main ~package schemas] dispatches based on [Sys.argv]:
    - [3d] runs {!generate_3d}
    - [c] runs {!generate_c}
    - [dune] generates [dune.inc] with build rules, test, and install stanzas
    - otherwise runs {!generate} *)
