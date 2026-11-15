(** Shared semantic comparisons for hardware proof circuits. *)

val same_result : Wire_hardcaml.t -> Wire_hardcaml.t -> Hardcaml.Signal.t
(** Equality of capacity, acceptance, consumption and every scalar output. *)
