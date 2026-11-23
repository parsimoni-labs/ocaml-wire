(** Stateful equivalence of extraction pipelining and a registered decoder. *)

val circuit : unit -> Hardcaml.Circuit.t
(** Compare every result after an enabled capture; reset aborts the observation
    window. Internal data registers may start in arbitrary states. *)
