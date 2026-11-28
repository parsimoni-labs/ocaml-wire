(** The fields a generated fold hands out, for the generators of the
    differential fuzzer's backends. *)

type leaf = {
  path : string;  (** Its path, as [Wire_ocaml.v ~fold] names it. *)
  label : string;  (** The label the OCaml fold's [f] takes it with. *)
  output : string;  (** The output the references name it by. *)
  carrier : string option;
      (** The module of its OCaml type, [None] for [int]. *)
  own : string option;
      (** The output of its own presence, where the field is optional. *)
  layered : bool;  (** It lies under a layer, so its value is one option more. *)
}
(** An integer field a fold can name. *)

val leaves : 'r Wire.Codec.t -> leaf list
(** [leaves c] is every integer field of [c]'s views, in declaration order.
    Raises [Invalid_argument] as {!Wire_ocaml.Views.v} does. *)

val accepted : (string list -> 'a) -> leaf list -> leaf list
(** [accepted make ls] is [ls], or, when [make] of their paths raises
    [Invalid_argument], those of [ls] that [make] takes, in order: each kept
    where [make] takes it with the ones kept before it. *)

val every_other : 'a list -> 'a list
(** [every_other l] is the first, third, ... elements of [l]. *)

val outputs : leaf list -> string list
(** [outputs ls] is the outputs a fold of [ls] hands out: each field's, then its
    own presence where it has one. *)
