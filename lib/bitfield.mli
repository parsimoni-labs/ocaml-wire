(** Shared bitfield utilities. *)

val byte_size : Types.bitfield_base -> int

val total_bits : Types.bitfield_base -> int
(** Total bits in a bitfield base type (8, 16, or 32). *)

val equal : Types.bitfield_base -> Types.bitfield_base -> bool
(** Check if two bitfield bases are the same type and endianness. *)

val read_word : Types.bitfield_base -> bytes -> int -> int
(** Read the base word from bytes at offset. *)

val write_word : Types.bitfield_base -> bytes -> int -> int -> unit
(** Write the base word to bytes at offset. *)

val is_lsb_first : Types.bitfield_base -> bool
(** [true] for LSBFirst bases (UINT8, UINT16, UINT32), [false] for MSBFirst
    (UINT16BE, UINT32BE). Matches EverParse 3D convention. *)

val extract :
  base:Types.bitfield_base ->
  total:int ->
  bits_used:int ->
  width:int ->
  int ->
  int
(** Extract bits from a word. Bit ordering follows EverParse 3D: LSBFirst bases
    pack the first declared field at bit 0, MSBFirst bases pack the first
    declared field at the MSB. *)
