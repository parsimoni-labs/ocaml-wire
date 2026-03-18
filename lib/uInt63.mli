(** Unsigned 63-bit integer. Reads 8 bytes, masks to 63 bits (OCaml int). *)

type t = int

val get_le : bytes -> int -> t
(** Read little-endian from [bytes] at offset. *)

val get_be : bytes -> int -> t
(** Read big-endian from [bytes] at offset. *)

val set_le : bytes -> int -> t -> unit
(** Write little-endian into [bytes] at offset. *)

val set_be : bytes -> int -> t -> unit
(** Write big-endian into [bytes] at offset. *)

val to_int : t -> int
(** Identity coercion. *)

val of_int : int -> t
(** Identity coercion. *)
