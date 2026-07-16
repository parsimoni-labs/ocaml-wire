(** Unsigned 63-bit integer. Reads 8 bytes, masks to 63 bits.

    Backed by {!Optint.Int63}: an unboxed native [int] on a 64-bit host, a boxed
    [int64] where the native [int] is narrower (there a plain-[int] composition
    would shift past the word and lose the high half). *)

type t = Optint.Int63.t

val pp : Format.formatter -> t -> unit
(** Pretty-printer for unsigned 63-bit values. *)

val le : bytes -> int -> t
(** [le buf off] reads a little-endian value from [buf] at offset [off]. *)

val be : bytes -> int -> t
(** [be buf off] reads a big-endian value from [buf] at offset [off]. *)

val set_le : bytes -> int -> t -> unit
(** [set_le buf off v] writes [v] as little-endian into [buf] at offset [off].
*)

val set_be : bytes -> int -> t -> unit
(** [set_be buf off v] writes [v] as big-endian into [buf] at offset [off]. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. Exact on a 64-bit host. *)

val of_int : int -> t
(** [of_int n] is [n] as a 63-bit value. *)
