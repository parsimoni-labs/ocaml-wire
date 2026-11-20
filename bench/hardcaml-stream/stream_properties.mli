(** Stream control and combinational equivalence checks. *)
val keep_equivalence : unit -> Hardcaml.Circuit.t
(** Compare the production contiguous-mask helper with its arithmetic
    specification for every mask at widths 1 through 64 and 128. *)

val keep_predicate : reference:bool -> Hardcaml.Circuit.t
(** Export the 64-bit predicate alone for separate input-cone synthesis. *)

val circuit : Wire_hardcaml.Stream.packing -> Hardcaml.Circuit.t
(** Assert bounded occupancy, ordered IDs, stalled stability and rejection
    isolation for arbitrary input data, masks, framing, errors and stalls. *)
