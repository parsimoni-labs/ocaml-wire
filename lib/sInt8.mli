(** Signed 8-bit integer.

    A native [int] narrowed to what one signed byte holds. The range is in the
    type rather than in a check at every encoder: a number outside [[-128, 127]]
    never becomes a value of this type, so nothing downstream has to ask again,
    and a case value or table index the field cannot hold is refused where it is
    written down rather than at the first byte encoded.

    [private int] on purpose. The carrier is exactly the [int] it looks like, on
    every target: no box, no conversion on the way out, and only the places that
    build a value have to say which range they are in. *)

type t = private int

val v : int -> t
(** [v n] is [n] as a signed 8-bit value. Raises [Invalid_argument] when [n] is
    outside [[-128, 127]], rather than masking it to a legal value the caller
    did not mean: 200 masks to a perfectly good -56 that no decoder can tell
    from the number that was written. *)

val v_opt : int -> t option
(** [v_opt n] is {!v} as an option, for a caller testing a number rather than
    asserting it. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. Total and exact on every target:
    every signed 8-bit value fits every OCaml [int]. *)

val zero : t
(** [zero] is 0. *)

val min_int : t
(** [min_int] is -128, the smallest value a signed byte holds. *)

val max_int : t
(** [max_int] is 127, the largest value a signed byte holds. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] are the same value. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for signed 8-bit values. *)

val get : bytes -> int -> t
(** [get buf off] reads the signed byte at offset [off]. Total: one byte spells
    only values the field holds. *)

val set : bytes -> int -> t -> unit
(** [set buf off v] writes [v] as one signed byte at offset [off]. Needs no
    range check: every {!type-t} is a value the byte holds. *)
