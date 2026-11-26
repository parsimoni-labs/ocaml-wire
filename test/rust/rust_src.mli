(** Helpers over the Rust source {!Wire_rust} writes, for its tests. *)

val contains : string -> string -> bool
(** [contains s sub] is [true] iff [sub] occurs in [s]. *)

val has : string -> string -> string -> unit
(** [has what src sub] checks that [sub] occurs in [src], [what] naming [src].
*)
