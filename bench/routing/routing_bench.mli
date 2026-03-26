(** APID demux routing benchmark harness. *)

val verify : n_pkts:int -> unit -> unit
(** [verify ~n_pkts ()] checks that the OCaml and C APID routing produce
    identical results. *)

val main : unit -> unit
(** [main ()] runs the APID demux throughput benchmark and prints a comparison
    table. *)
