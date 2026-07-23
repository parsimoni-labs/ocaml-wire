(** Shared bitfield utilities. *)

val byte_size : Types.bitfield_base -> int
(** [byte_size base] is the number of bytes used by one packed bitfield base
    word. *)

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

val set_u32_le : bytes -> int -> int -> unit
(** Inline little-endian 32-bit word write of a native-[int] bitfield word. *)

val set_u32_be : bytes -> int -> int -> unit
(** Inline big-endian 32-bit word write of a native-[int] bitfield word. *)

val int_holds_u32 : bool
(** Whether the native [int] holds a full 32-bit word. False under wasm_of_ocaml
    (31-bit int) and js_of_ocaml (32-bit): the whole-word accessors above
    truncate there, so word-at-a-time callers must switch to the halves-based
    field accessors below. *)

val u32_field_le : bytes -> int -> int -> int -> int
(** [u32_field_le buf off shift mask] extracts [(word lsr shift) land mask] from
    the little-endian 32-bit word at [off] without materializing the word: exact
    on any int width for a field value below bit 31. *)

val u32_field_be : bytes -> int -> int -> int -> int
(** Big-endian {!u32_field_le}. *)

val u32_field_word_le : bytes -> int -> int -> int -> unit
(** [u32_field_word_le buf off shift v] writes a lone field as a full
    little-endian word (other bits zero). *)

val u32_field_word_be : bytes -> int -> int -> int -> unit
(** Big-endian {!u32_field_word_le}. *)

val u32_field_or_le : bytes -> int -> int -> int -> unit
(** [u32_field_or_le buf off shift v] ORs a field into the little-endian word at
    [off]. *)

val u32_field_or_be : bytes -> int -> int -> int -> unit
(** Big-endian {!u32_field_or_le}. *)

val u32_field_set_le : bytes -> int -> int -> int -> int -> unit
(** [u32_field_set_le buf off shift mask v] replaces the field, clearing its
    bits first. *)

val u32_field_set_be : bytes -> int -> int -> int -> int -> unit
(** Big-endian {!u32_field_set_le}. *)

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
