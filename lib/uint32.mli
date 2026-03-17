(** Unsigned 32-bit integer. Unboxed on 64-bit (fits in 63-bit OCaml int). *)

type t = int

val get_le : bytes -> int -> t
val get_be : bytes -> int -> t
val set_le : bytes -> int -> t -> unit
val set_be : bytes -> int -> t -> unit
val to_int : t -> int
val of_int : int -> t
