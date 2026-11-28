(** The reads of the fields of a checked view ({!Codec.View}) as data. Internal
    to {!Codec}. *)

(** How a view reads a field: from an 8-byte word of the record, shifted and
    masked ({!constructor-Word}); the same from a 4-byte word
    ({!constructor-Word32}); as a slice of constant size ({!constructor-Bytes});
    as a scalar in the record's frame ({!constructor-Scalar}), [stop] bytes past
    the record's offset; or by a call ({!constructor-Get}). Every read tests
    that its bytes lie in the buffer, and answers the field's zero where they do
    not. *)
type 'a read =
  | Word : { q : int; s : int; m : int; eq : ('a, int) Type.eq } -> 'a read
  | Word32 : { q : int; s : int; m : int; eq : ('a, int) Type.eq } -> 'a read
  | Bytes : { off : int; length : int } -> Bytesrw.Bytes.Slice.t read
  | Scalar : { s : 'a Codec_repr.scalar; stop : int; zero : 'a } -> 'a read
  | Get : (bytes -> int -> 'a) -> 'a read

val scalar : 'a Codec_repr.scalar -> 'a read
(** [scalar s] is the read of [s]. *)

val read_other : 'a read -> bytes -> int -> 'a
(** [read_other f buf r] is {!read}, out of line. *)

val read : 'a read -> bytes -> int -> 'a
(** [read f buf r] is the field [f] of the record at [r] in [buf], [r]
    non-negative. A {!constructor-Word} read is inline, every other one call. *)

type 'a bits = { o : int; n : int; s : int; w : int; eq : ('a, int) Type.eq }
(** The [w] bits above bit [s] of the [n] bytes at [o] of a record, an unsigned
    field of an [int]. *)

val bits_of_scalar : 'a Codec_repr.scalar -> 'a bits option
(** [bits_of_scalar s] is [s] as bits of an [int], when it is unsigned and
    big-endian. *)

val word_field : size:int -> 'a bits -> 'a read option
(** [word_field ~size b] is how a view reads [b] from a word of a record of
    [size] bytes, on a target whose [int] holds 63 bits: the 8-byte word
    {!Codec_words.word} picks in a frame of at least 8 bytes, else the 4-byte
    word at the field's bytes or, near the end of the frame, its last 4 bytes.
    [None] on a narrower target or a frame of fewer than 4 bytes. *)
