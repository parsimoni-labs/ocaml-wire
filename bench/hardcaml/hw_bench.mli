(** Hardware measurement candidate for the packet-stack codec. *)

val circuit : unit -> Hardcaml.Circuit.t
(** The complete 64-byte parser, registered ready/valid flow and checksum sink.
    This is a measurement candidate, not a measured timing claim. *)

val contribution : Wire_hardcaml.t -> Hardcaml.Signal.t
(** The per-frame benchmark checksum before accumulation. *)
