(** Padding noninterference circuit for the bounded hardware decoder. *)

val circuit : unit -> Hardcaml.Circuit.t
(** A combinational miter: unused physical bus bits cannot affect results. *)
