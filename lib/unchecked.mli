(** Buffer reads with no bounds check. The caller must have checked that the
    bytes read lie in the buffer. *)

val u8 : bytes -> int -> int
(** [u8 buf off] is the byte at [off]. *)

val u16le : bytes -> int -> int
(** [u16le buf off] is the little-endian 16-bit value at [off]. *)

val u16be : bytes -> int -> int
(** [u16be buf off] is the big-endian 16-bit value at [off]. *)

val i32le : bytes -> int -> int32
(** [i32le buf off] is the little-endian 32-bit value at [off]. *)

val i32be : bytes -> int -> int32
(** [i32be buf off] is the big-endian 32-bit value at [off]. *)
