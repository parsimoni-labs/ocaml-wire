(** Variable-width unsigned integer read/write.

    Values are {!UInt63.t}: a 7-byte value needs 56 bits, which never fits a
    narrow-int platform's [int] and always fits 63 bits. *)

val read : Types.endian -> bytes -> int -> int -> UInt63.t
(** [read endian buf off size] reads [size] bytes as an unsigned value. *)

val write : Types.endian -> bytes -> int -> int -> UInt63.t -> unit
(** [write endian buf off size v] writes [v] as [size] bytes. *)
