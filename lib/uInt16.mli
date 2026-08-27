(** Unsigned 16-bit integer.

    A native [int] narrowed to what an unsigned 16-bit field holds. The range is
    in the type rather than in a check at every encoder: a number outside
    [[0, 65535]] never becomes a value of this type, so nothing downstream has
    to ask again, and a case value or table index the field cannot hold is
    refused where it is written down rather than at the first byte encoded.

    [private int] on purpose. The carrier is exactly the [int] it looks like, on
    every target: no box, no conversion on the way out, and only the places that
    build a value have to say which range they are in. *)

type t = private int

val v : int -> t
(** [v n] is [n] as an unsigned 16-bit value. Raises [Invalid_argument] when [n]
    is outside [[0, 65535]], rather than masking it to a legal value the caller
    did not mean: 70000 masks to a perfectly good 4464 that no decoder can tell
    from the number that was written. *)

val v_opt : int -> t option
(** [v_opt n] is {!v} as an option, for a caller testing a number rather than
    asserting it. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. Total and exact on every target:
    every unsigned 16-bit value fits every OCaml [int]. *)

val zero : t
(** [zero] is 0. *)

val min_int : t
(** [min_int] is 0, the smallest value an unsigned 16-bit field holds. *)

val max_int : t
(** [max_int] is 65535, the largest value an unsigned 16-bit field holds. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] are the same value. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for unsigned 16-bit values. *)

val le : bytes -> int -> t
(** [le buf off] reads two little-endian bytes at offset [off]. Total: two bytes
    spell only values the field holds. *)

val be : bytes -> int -> t
(** [be buf off] reads two big-endian bytes at offset [off]. Total for the same
    reason as {!le}. *)

val set_le : bytes -> int -> t -> unit
(** [set_le buf off v] writes [v] as two unsigned little-endian bytes at offset
    [off]. Needs no range check: every {!type-t} is a value the field holds. *)

val set_be : bytes -> int -> t -> unit
(** [set_be buf off v] writes [v] as two unsigned big-endian bytes at offset
    [off]. Needs no range check, as for {!set_le}. *)
