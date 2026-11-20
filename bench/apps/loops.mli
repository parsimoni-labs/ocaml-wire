(** The application workloads over the modules [Wire_ocaml] generates, as a user
    of those modules writes them. *)

val routing : bytes -> int -> int * int * int * int
(** [routing buf n] routes [n] space packets of [buf] and counts them per route.
*)

val gateway : bytes -> int
(** [gateway buf] hashes the header of each TM frame of [buf] and of each packet
    it carries. *)

val restamp : bytes -> int -> bytes -> bytes
(** [restamp buf n out] writes the header of each of the [n] packets of [buf] at
    the same offset of [out], with the next sequence count of its route. *)

val frame : int -> bytes -> bytes
(** [frame n out] writes [n] TM frames of a packet stream into [out]. *)

val report : int -> bytes -> bytes
(** [report n out] writes [n] CLCW words into [out]. *)

val clcw : bytes -> int
(** [clcw buf] counts the CLCW words of [buf] that report an anomaly. *)
