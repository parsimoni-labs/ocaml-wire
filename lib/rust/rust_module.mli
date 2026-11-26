(** The one Rust module of a codec, whatever its shape.

    Its parts are written from {!Wire_ocaml.Plan}: the check and the folds from
    the codec's plan, the views, values and errors from the plan of each record
    it reaches. Internal to {!Wire_rust}. *)

type fold = {
  name : string;
  outputs : string list option;  (** Every output for [None]. *)
  visitor : string option;
      (** The path of the type the rule names as its visitor, if any. *)
}
(** A fold the module declares. *)

val rs : codec:string -> folds:fold list -> 'r Wire.Codec.t -> string
(** [rs ~codec ~folds c] is the source of the module of [c], [codec] naming it
    in the module's header and its [CODEC] constant. Raises [Invalid_argument]
    naming the reason for a codec the walk does not cover, an expression Rust
    cannot spell, a name that is not UTF-8, and two names that would be one Rust
    name. *)
