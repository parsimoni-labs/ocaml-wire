(** TM frame reassembly benchmark harness. *)

val verify : n_frames:int -> unit -> unit
(** [verify ~n_frames ()] checks that the OCaml and C TM frame reassembly
    produce identical results. *)

val main : unit -> unit
(** [main ()] runs the TM frame reassembly benchmark and prints a comparison
    table. *)
