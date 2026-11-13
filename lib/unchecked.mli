(** Buffer reads and writes with no bounds check. The caller must have checked
    that the bytes lie in the buffer. *)

val checked : bool
(** [checked] is [true] in the [paranoid] and [afl] build profiles. Every
    function below then checks its bounds, and raises [Invalid_argument] outside
    the buffer. *)

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

val i64le : bytes -> int -> int64
(** [i64le buf off] is the little-endian 64-bit value at [off]. *)

val i64be : bytes -> int -> int64
(** [i64be buf off] is the big-endian 64-bit value at [off]. *)

(** {1 Writes}

    With no bounds check either. A 16-bit write keeps the low 16 bits of the
    value. *)

val set_u8 : bytes -> int -> int -> unit
(** [set_u8 buf off v] writes the low byte of [v] at [off]. *)

val set_u16le : bytes -> int -> int -> unit
(** [set_u16le buf off v] writes [v] as a little-endian 16-bit value. *)

val set_u16be : bytes -> int -> int -> unit
(** [set_u16be buf off v] writes [v] as a big-endian 16-bit value. *)

val set_i32le : bytes -> int -> int32 -> unit
(** [set_i32le buf off v] writes [v] as a little-endian 32-bit value. *)

val set_i32be : bytes -> int -> int32 -> unit
(** [set_i32be buf off v] writes [v] as a big-endian 32-bit value. *)

val blit_string : string -> bytes -> int -> unit
(** [blit_string s buf off] copies [s] into [buf] at [off]. *)
