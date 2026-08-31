(** Signed 32-bit integer.

    Abstract, and deliberately not [Optint.t] even though that is the carrier: a
    32-bit unsigned value uses the same carrier, and the two must not be
    interchangeable. Where the native [int] is narrower than 32 bits [Optint.t]
    is [Int32], whose inherited comparison is signed and so wrong for the
    unsigned view; keeping each side behind its own type stops one being read
    with the other's meaning.

    The carrier is an unboxed native [int] on a 64-bit host and a boxed [int32]
    where the native [int] is narrower, so the full signed 32-bit range survives
    on js_of_ocaml and wasm_of_ocaml, where a plain [int] would drop the top
    bits. *)

type t

val pp : Format.formatter -> t -> unit
(** Pretty-printer for signed 32-bit values. *)

val zero : t
(** [zero] is 0. *)

val min_int : t
(** [min_int] is -2{^ 31}, the smallest value a 32-bit signed field holds. *)

val max_int : t
(** [max_int] is 2{^ 31} - 1, the largest value a 32-bit signed field holds. *)

val compare : t -> t -> int
(** [compare a b] orders two values as signed 32-bit integers. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] are the same value. *)

val le : bytes -> int -> t
(** [le buf off] reads a little-endian value from [buf] at offset [off]. *)

val be : bytes -> int -> t
(** [be buf off] reads a big-endian value from [buf] at offset [off]. *)

val check_encode : t -> unit
(** [check_encode v] raises [Invalid_argument] when [v] is not a signed 32-bit
    value. Only reachable where the native [int] is wider than the field, which
    is the only place a {!type-t} can hold a number 32 bits cannot. *)

val set_le : bytes -> int -> t -> unit
(** [set_le buf off v] writes [v] as little-endian into [buf] at offset [off].
    Raises [Invalid_argument] on a value {!check_encode} rejects. *)

val set_be : bytes -> int -> t -> unit
(** [set_be buf off v] writes [v] as big-endian into [buf] at offset [off].
    Raises [Invalid_argument] on a value {!check_encode} rejects. *)

val to_int32 : t -> int32
(** [to_int32 t] is the value as an [int32], exact on every platform. *)

val of_int32 : int32 -> t
(** [of_int32 n] is the [int32] as a signed 32-bit value: the same value {!le}
    and {!be} decode those four bytes to, on every platform. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. Exact on a 64-bit host; where the
    native [int] is narrower it does not hold the full range, so prefer
    {!to_int32} across such a boundary. *)

val to_int_opt : t -> int option
(** [to_int_opt t] is the value as a native [int], or [None] where that [int] is
    too narrow to hold it. Always [Some] on a 64-bit host. Prefer it to
    {!to_int} on any path that must not silently drop the top bits. *)

val of_int : int -> t
(** [of_int n] is [n] as a signed 32-bit value. Raises [Invalid_argument] when
    [n] is outside the range, rather than masking it to a legal value the caller
    did not mean. *)
