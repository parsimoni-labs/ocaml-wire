(** Helpers over the Rust source {!Wire_rust} writes, for its tests. *)

val contains : string -> string -> bool
(** [contains s sub] is [true] iff [sub] occurs in [s]. *)

val has : string -> string -> string -> unit
(** [has what src sub] checks that [sub] occurs in [src], [what] naming [src].
*)

val rs : ?fold:string list -> 'r Wire.Codec.t -> string
(** [rs ?fold c] is the source of the module of [c], with a fold of the fields
    [fold] names. *)

val paths : 'r Wire.Codec.t -> string list
(** [paths c] is the path of every integer field of [c], as a fold names it. *)

val folded : 'r Wire.Codec.t -> string
(** [folded c] is {!rs} with a fold of [paths c]. *)

val section : string -> string -> string
(** [section src name] is the function [name] of [src]: from its signature to
    the end of its body. *)
