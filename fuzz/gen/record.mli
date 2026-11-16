(** Record composition: leaves composed into record gens the way typs compose
    into record codecs. *)

open Gen

module Codec : sig
  type ('a, 'r) field
  (** A gen bound to the projection out of its record. *)

  val ( $ ) : 'a t -> ('r -> 'a) -> ('a, 'r) field
  (** [g $ getter] binds gen [g] to record projection [getter]. *)

  (** The fields of a record gen, in order. *)
  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

  val with_names : string list -> ('f, 'r) fields -> ('f, 'r) fields
  (** [with_names names fields] names the first fields of [fields] after
      [names], in order; the rest keep their names. *)

  val v : string -> ?equal:('r -> 'r -> bool) -> 'f -> ('f, 'r) fields -> 'r t
  (** [v name builder fields] composes leaves into a record gen whose codec is
      the flat [Wire.Codec.v name builder typs] you'd write by hand. *)
end
