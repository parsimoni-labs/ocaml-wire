(** Buffer reads and writes with no bounds check. The caller must have checked
    that the bytes lie in the buffer. *)

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

(** The same reads and writes on a bigarray of bytes, with checked reads that
    raise what [Bytes]' raise. *)
module Bigstring : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val length : t -> int
  (** [length buf] is the number of bytes in [buf]. *)

  val u8 : t -> int -> int
  (** [u8 buf off] is the byte at [off]. *)

  val u16le : t -> int -> int
  (** [u16le buf off] is the little-endian 16-bit value at [off]. *)

  val u16be : t -> int -> int
  (** [u16be buf off] is the big-endian 16-bit value at [off]. *)

  val i32le : t -> int -> int32
  (** [i32le buf off] is the little-endian 32-bit value at [off]. *)

  val i32be : t -> int -> int32
  (** [i32be buf off] is the big-endian 32-bit value at [off]. *)

  val i64be : t -> int -> int64
  (** [i64be buf off] is the big-endian 64-bit value at [off]. *)

  val set_u8 : t -> int -> int -> unit
  (** [set_u8 buf off v] writes the low byte of [v] at [off]. *)

  val set_u16le : t -> int -> int -> unit
  (** [set_u16le buf off v] writes [v] as a little-endian 16-bit value. *)

  val set_u16be : t -> int -> int -> unit
  (** [set_u16be buf off v] writes [v] as a big-endian 16-bit value. *)

  val set_i32le : t -> int -> int32 -> unit
  (** [set_i32le buf off v] writes [v] as a little-endian 32-bit value. *)

  val set_i32be : t -> int -> int32 -> unit
  (** [set_i32be buf off v] writes [v] as a big-endian 32-bit value. *)

  val get_uint8 : t -> int -> int
  (** [get_uint8 buf off] is {!u8} after a bounds check that raises
      [Invalid_argument "index out of bounds"]. *)

  val get_uint16_le : t -> int -> int
  (** [get_uint16_le] is {!u16le} with the same check. *)

  val get_uint16_be : t -> int -> int
  (** [get_uint16_be] is {!u16be} with the same check. *)

  val get_int32_le : t -> int -> int32
  (** [get_int32_le] is {!i32le} with the same check. *)

  val get_int32_be : t -> int -> int32
  (** [get_int32_be] is {!i32be} with the same check. *)

  val sub_string : t -> int -> int -> string
  (** [sub_string buf off n] copies the [n] bytes at [off], raising what
      [Bytes.sub_string] raises when they are not all in [buf]. *)

  val unsafe_blit_string : string -> int -> t -> int -> int -> unit
  (** [unsafe_blit_string s soff buf off n] copies [n] bytes of [s] from [soff]
      to [off], with no check. *)

  val to_bytes : t -> bytes
  (** [to_bytes buf] is a copy of [buf]. *)
end
