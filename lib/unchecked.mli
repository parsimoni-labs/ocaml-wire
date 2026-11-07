(** Buffer reads with no bounds check. The caller must have checked that the
    bytes read lie in the buffer. *)

val u8 : bytes -> int -> int
val u16le : bytes -> int -> int
val u16be : bytes -> int -> int
val i32le : bytes -> int -> int32
val i32be : bytes -> int -> int32
