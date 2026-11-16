(** The differential driver: one Alcobar suite over the sampled codecs of
    {!Diff_codecs.included}, each input handed to a backend's check. A backend
    is anything else derived from the same codec: the EverParse C validator, or
    the OCaml {!Wire_ocaml} generates. *)

val run : string -> (string * (bytes -> unit)) list -> unit
(** [run name cases] reports the codecs {!Diff_codecs.excluded} leaves out, then
    runs the suite [name] over [cases], each a codec's label and the check of
    one input against it. A normal run gives each codec its own case on random
    bytes; under AFL ({!Fuzz_gen.afl_mode}) the framed input picks one codec. A
    check fails through {!Alcobar.failf}. *)

val oracle : string -> Diff_codecs.c_oracle option
(** [oracle label] is {!C_oracle.oracle} for the structured case [label] when
    the validators are built. *)

val report_oracle : string -> unit
(** [report_oracle name] says whether the C validators are compared, and fails
    when [BUILD_EVERPARSE=1] but they are not built. *)
