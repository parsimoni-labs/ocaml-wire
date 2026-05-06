(** Shared bitfield utilities. *)

val byte_size : Types.bitfield_base -> int

val total_bits : Types.bitfield_base -> int
(** Total bits in a bitfield base type (8, 16, or 32). *)

val equal : Types.bitfield_base -> Types.bitfield_base -> bool
(** Check if two bitfield bases are the same type and endianness. *)

val read_word : Types.bitfield_base -> bytes -> int -> int
(** Read the base word from bytes at offset. *)

val u16_le : bytes -> int -> int
(** Inline little-endian 16-bit word read. *)

val u16_be : bytes -> int -> int
(** Inline big-endian 16-bit word read. *)

val u32_le : bytes -> int -> int
(** Inline little-endian 32-bit word read. *)

val u32_be : bytes -> int -> int
(** Inline big-endian 32-bit word read. *)

val write_word : Types.bitfield_base -> bytes -> int -> int -> unit
(** Write the base word to bytes at offset. *)

val native_bit_order : Types.bitfield_base -> Types.bit_order
(** [native_bit_order base] returns the bit order matching EverParse 3D's native
    packing for [base]: [Lsb_first] for the little-endian bases ([UINT8],
    [UINT16], [UINT32]) and [Msb_first] for the big-endian bases ([UINT16BE],
    [UINT32BE]). *)

val shift :
  bit_order:Types.bit_order -> total:int -> bits_used:int -> width:int -> int
(** [shift ~bit_order ~total ~bits_used ~width] returns the right-shift amount
    for a [width]-bit field starting at bit position [bits_used] inside a
    [total]-bit word, honoring the requested [bit_order]. *)

val extract :
  bit_order:Types.bit_order ->
  total:int ->
  bits_used:int ->
  width:int ->
  int ->
  int
(** Extract bits from a word. Bit order is independent of the base's byte order:
    {!Types.Msb_first} places the first declared field at the most significant
    bit, {!Types.Lsb_first} at the least. *)
