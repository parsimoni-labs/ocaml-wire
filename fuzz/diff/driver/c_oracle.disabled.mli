(** The EverParse C validators of the structured cases, when built with
    [BUILD_EVERPARSE=1]. *)

val enabled : bool
(** [enabled] is [true] when the validators are linked. *)

val oracle : string -> Diff_codecs.c_oracle
(** [oracle label] is the validator of the structured case [label]. *)
