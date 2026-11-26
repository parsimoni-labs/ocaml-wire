(** Helpers over the Rust source {!Wire_rust} writes, for its tests. *)

val contains : string -> string -> bool
(** [contains s sub] is [true] iff [sub] occurs in [s]. *)

val has : string -> string -> string -> unit
(** [has what src sub] checks that [sub] occurs in [src], [what] naming [src].
*)

val rs :
  ?folds:Wire_rust.fold list ->
  ?visitors:(string * string) list ->
  'r Wire.Codec.t ->
  string
(** [rs ?folds ?visitors c] is the source of the module of [c], its folds
    visited by [visitors]. *)
