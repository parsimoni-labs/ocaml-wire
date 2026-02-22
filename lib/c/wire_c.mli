(** Generate verified C libraries from Wire codecs via EverParse.

    Given Wire schemas, generates EverParse .3d files and invokes EverParse to
    produce pure C parsers and validators. The output directory contains a
    self-contained C library: [EverParse.h], [<Name>.h], [<Name>.c], and a
    [test.c] that exercises the validators.

    {b Typical usage:}
    {[
      let () =
        let outdir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
        Wire_c.generate ~outdir
          [
            Wire_c.schema ~name:"Clcw" ~module_:Clcw.module_
              ~wire_size:(Wire.Codec.wire_size Clcw.codec);
          ]
    ]} *)

type schema
(** A schema bundles a name, Wire module, and wire size. *)

val schema : name:string -> module_:Wire.module_ -> wire_size:int -> schema
(** [schema ~name ~module_ ~wire_size] creates a schema for C library
    generation. *)

val generate : outdir:string -> schema list -> unit
(** [generate ~outdir schemas] runs the full pipeline:
    + Generates .3d files in [outdir]
    + Invokes EverParse to produce C parsers
    + Generates [test.c] that exercises the validators

    Requires [3d.exe] (EverParse) in PATH. *)
