(** The canonical registry: the curated gens, one per combinator family, that
    every fuzz suite drives off. *)

open Gen

(** A gen with its value type hidden, for uniform iteration over {!registry}. *)
type packed = Pack : 'a t -> packed

val equal : 'a t -> 'a -> 'a -> bool
(** [equal g a b] is [g]'s own value equality. *)

val codec : 'a t -> 'a Wire.Codec.t
(** [codec g] is the codec [g] generates for. *)

val binds_env : packed -> bool
(** [binds_env p] is [true] when [p]'s codec needs a [Param.env]. *)

val rename_case : string -> packed -> packed
(** [rename_case label p] renames [p]'s codec to a unique 3D struct name derived
    from [label], leaving its encoding and streams unchanged. *)

val registry : (string * packed) list
(** [registry] is the curated gens, each with a unique struct name. *)
