(** The everparse-asn1 pipeline.

    [Wire_asn1] is to [everparse-asn1.exe] what [Wire_3d] is to
    [everparse-3d.exe]: it writes the [.asn] modules {!Wire.Asn1.project} gives,
    runs everparse-asn1 on them to produce verified C validators, parsers and
    DER writers, and writes the dune rules and the C programs that check them
    against the OCaml codecs. See docs/asn1.md. *)

val tool : unit -> string option
(** [tool ()] is the path of [everparse-asn1.exe], found as [everparse-3d.exe]
    is: in [PATH], then in [~/.local/everparse/bin]. *)

val c_name : string -> string
(** [c_name t] is the C identifier everparse-asn1 gives the ASN.1 type [t]:
    [validate_<c_name t>] is its validator. *)

val run_everparse :
  ?quiet:bool -> outdir:string -> Wire.Asn1.schema list -> unit
(** [run_everparse ?quiet ~outdir ss] runs everparse-asn1 on the [.asn] file of
    each schema in [outdir], with its [--root] types, producing [<M>.c] and
    [<M>.h]. Raises [Failure] if the tool is missing or refuses a module. *)

val run : ?quiet:bool -> outdir:string -> Wire.Asn1.schema list -> unit
(** [run ?quiet ~outdir ss] writes the [.asn] files and runs {!run_everparse}.
*)

val generate_dune :
  outdir:string -> package:string -> Wire.Asn1.schema list -> unit
(** [generate_dune ~outdir ~package ss] writes [dune.inc] with the rules that
    regenerate and compile the C of [ss], the regeneration gated on the default
    context, as [Wire_3d.generate_dune] does. *)

type packed
(** The type for codecs with their record type erased. *)

val pack : 'r Wire.Codec.t -> packed
(** [pack c] is [c] with its record type erased. *)

val generate_corpus : ?count:int -> Format.formatter -> packed list -> unit
(** [generate_corpus ?count ppf cs] prints [count] inputs per codec as
    [<codec> <hex> <verdict>] lines, [verdict] [1] when the OCaml codec accepts
    the bytes. *)

val generate_agree :
  ?name:string -> outdir:string -> package:string -> packed list -> unit
(** [generate_agree ?name ~outdir ~package cs] writes [agree.c]: a C program
    that replays a {!generate_corpus} corpus through each [validate_T] and
    [write_T], and fails on the first verdict, consumed length or re-encoding
    that differs from the corpus. *)
