(** Unsigned 64-bit integer.

    Abstract, and deliberately not [int64] even though that is the carrier. An
    8-byte field holds the same bits either way, but not the same order:
    [Int64.compare] is signed, so it ranks 0xFFFF_FFFF_FFFF_FFFF, the largest
    value here, below 1. Keeping the type opaque is what stops that comparison
    being reached, and what stops an unsigned field being read where a signed
    one is expected.

    Unlike the narrower widths there is no range to enforce: the carrier is
    exactly the eight bytes on the wire on every target, so every bit pattern is
    a legal value and none is out of range. *)

type t

val pp : Format.formatter -> t -> unit
(** Pretty-printer, in the unsigned reading. *)

val zero : t
(** [zero] is 0. *)

val max_int : t
(** [max_int] is 2{^ 64} - 1, the largest value an 8-byte unsigned field holds.
*)

val compare : t -> t -> int
(** [compare a b] orders two values as unsigned 64-bit integers, so
    0xFFFF_FFFF_FFFF_FFFF is the largest. *)
(* TODO: this is [Int64.compare] for now, which is signed. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] are the same value. *)

val le : bytes -> int -> t
(** [le buf off] reads a little-endian value from [buf] at offset [off]. *)

val be : bytes -> int -> t
(** [be buf off] reads a big-endian value from [buf] at offset [off]. *)

val set_le : bytes -> int -> t -> unit
(** [set_le buf off v] writes [v] as little-endian into [buf] at offset [off].
*)

val set_be : bytes -> int -> t -> unit
(** [set_be buf off v] writes [v] as big-endian into [buf] at offset [off]. *)

val to_int64 : t -> int64
(** [to_int64 t] is the value's bit pattern as an [int64]. Exact, but read as a
    signed number: a value above 2{^ 63} - 1 comes back negative. *)

val of_int64 : int64 -> t
(** [of_int64 n] is the [int64] bit pattern as an unsigned 64-bit value: the
    same value {!le} and {!be} decode those eight bytes to. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. A value the [int] cannot hold is
    truncated, so prefer {!to_int_opt} on any path that must not drop bits. *)

val to_int_opt : t -> int option
(** [to_int_opt t] is the value as a native [int], or [None] where that [int] is
    too narrow to hold it. *)

val of_int : int -> t
(** [of_int n] is [n] as an unsigned 64-bit value. Raises [Invalid_argument] on
    a negative [n], rather than reinterpreting it as the large value its two's
    complement spells. Use {!of_int64} to reinterpret a bit pattern. *)
