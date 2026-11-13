(** The EverParse C validators of the structured cases, when built with
    [BUILD_EVERPARSE=1]. *)

val enabled : bool
(** [enabled] is [true] when the validators are linked. *)

val accepted_end : string -> bytes -> int -> int -> int
(** [accepted_end label buf off len] is the number of bytes the validator of the
    case [label] consumes in the [len] bytes at [off], or [-1] when it rejects
    them or the bounds. *)
