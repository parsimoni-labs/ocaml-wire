(** The evercddl pipeline.

    [Wire_cbor] is to [everparse-cddl.exe] what [Wire_asn1] is to
    [everparse-asn1.exe]: it writes the [.cddl] modules {!Wire.Cbor.project}
    gives and runs evercddl on them, which verifies the F* module it writes for
    each and extracts and compiles its C. See docs/cbor.md. *)

val tool : unit -> string option
(** [tool ()] is the path of [everparse-cddl.exe], found as [everparse-3d.exe]
    is ({!Wire_3d.locate_3d_exe}): in [PATH], then in [~/.local/everparse/bin].
    [None] when it is in neither. *)

val postlude : string -> string
(** [postlude exe] is the [postlude.cddl] that defines the CDDL prelude names
    for the tool [exe], under [EVERPARSE_HOME], by default the prefix [exe] is
    installed in. evercddl reads it as one more input. *)

val run_everparse :
  ?quiet:bool -> outdir:string -> Wire.Cbor.schema list -> unit
(** [run_everparse ?quiet ~outdir ss] runs evercddl in full on the [.cddl] file
    of each schema of [ss] in [outdir], with the {!postlude}, each in a job of
    the pool {!Wire_3d.run} runs EverParse in: F* verifies the module it writes,
    KaRaMeL extracts its C, and [cc] compiles it with [-Wall -Werror]. The job
    copies [<M>.c] and [<M>.h] into [outdir]. Each job's output, its last line
    [<name>: verified in <n>s], is shown as {!Wire_3d.run} shows it: only for a
    failed job if [quiet] (the default), else for every job once all have
    succeeded.

    Raises [Failure] if the tool is missing, or naming each schema it refuses or
    F* does not verify, with its output. *)

val run : ?quiet:bool -> outdir:string -> Wire.Cbor.schema list -> unit
(** [run ?quiet ~outdir ss] writes the [.cddl] files of [ss] in [outdir], then
    runs {!run_everparse}. *)
