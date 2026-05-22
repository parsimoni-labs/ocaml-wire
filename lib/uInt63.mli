(** Unsigned 63-bit integer. Reads 8 bytes, masks to 63 bits (OCaml int). *)

type t = int

val pp : Format.formatter -> t -> unit
(** Pretty-printer for unsigned 63-bit values represented as OCaml integers. *)

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
(** Identity coercion. *)

val of_int : int -> t
(** Identity coercion. *)
