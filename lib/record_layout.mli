(** Where the fields of a fixed record sit, for the code generators.

    A generator lays a record out the same way the codec does; this is that
    layout, shared by the generators so they cannot drift apart. *)

(** How a field is stored: an integer of [width] bits, a bitfield of [width]
    bits at [shift] in its word, or a byte array of a fixed size. *)
type store =
  | Int of { width : int; endian : Types.endian; signed : bool }
  | Bits of { base : Types.bitfield_base; shift : int; width : int }
  | Bytes of int

val leaf : 'a Types.typ -> store option
(** [leaf typ] is how a field of [typ] is stored when [typ] is an integer or a
    byte array of literal size, the types a generator reads directly. *)

val width_bytes : store -> int
(** [width_bytes s] is how many bytes a field stored as [s] spans: its word's
    for a bitfield. *)

type word = {
  base : Types.bitfield_base;
  order : Types.bit_order;
  at : int;
  used : int;
}
(** The bitfield word being filled: its base, bit order, offset and the bits
    taken so far. *)

val place_bits :
  off:int ->
  word:word option ->
  base:Types.bitfield_base ->
  order:Types.bit_order ->
  width:int ->
  word * int * int
(** [place_bits ~off ~word ~base ~order ~width] places a bitfield of [width]
    bits reached at [off]. It goes into [word] when that has the same base and
    bit order and still has room, as the codec packs it, else it opens a new
    word at [off]. The result is the word, the field's shift in it and the
    offset of the field after it. *)
