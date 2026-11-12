(** A Boltzmann sampler over the fixed-size, projectable grammar: records of
    geometric arity and homogeneous arrays over a leaf vocabulary that
    oversamples the weird and adversarial shapes. Driven by a [Random.State.t],
    so a seed reproduces the exact set. *)

open Compose
open Curated

val sampler_regular_leaves : any list
(** [sampler_regular_leaves] is the regular part of the leaf vocabulary. *)

val sampler_adversarial_leaves : any list
(** [sampler_adversarial_leaves] is the weird and refined part of the leaf
    vocabulary, drawn three times more often than the regular part. *)

val sampler_leaves : any list
(** [sampler_leaves] is the whole leaf vocabulary. *)

(** The shape of one sampled codec as a tree. *)
type sample_shape =
  | Leaf
  | Array of sample_shape  (** element shape *)
  | Record of sample_shape list  (** field shapes, in declaration order *)

type sample_meta = {
  shape : sample_shape;  (** The codec's shape. *)
  leaves : string list;  (** The leaf labels it draws, in draw order. *)
}
(** What a sample is made of, for coverage metrics. *)

val equal_shape : sample_shape -> sample_shape -> bool
(** [equal_shape a b] is structural equality on shapes. *)

val shape_depth : sample_shape -> int
(** [shape_depth s] is the nesting depth of [s]. *)

val shape_has_array_beside_fields : sample_shape -> bool
(** [shape_has_array_beside_fields s] holds when an array in [s] shares its
    record with another field. *)

val shape_has_nested_repetition : sample_shape -> bool
(** [shape_has_nested_repetition s] holds when an array in [s] repeats again
    inside its element. *)

val sampled : seed:int -> count:int -> (string * packed * sample_meta) list
(** [sampled ~seed ~count] is {!sample} with each sample's shape. *)

val sample : seed:int -> count:int -> (string * packed) list
(** [sample ~seed ~count] is a deterministic sample of [count] codecs, each
    renamed to a unique struct name. *)
