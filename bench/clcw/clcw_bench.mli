(** CLCW polling loop benchmark harness. *)

val verify : n_words:int -> unit -> unit
(** [verify ~n_words ()] checks that the OCaml and C CLCW polling loops produce
    identical results. *)

val main : unit -> unit
(** [main ()] runs the CLCW polling loop benchmark and prints a comparison
    table. *)
