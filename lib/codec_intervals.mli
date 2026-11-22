(** Interval tests over the fixed-offset fields of a record.

    A field's test is a set of intervals; the tests of a record flatten into one
    int array, with the tests of one byte merged into a table of its 256 values,
    which {!scan_intervals} runs with no call. Internal to {!Codec}. *)

val in_set : (int * int) array -> int -> bool
(** [in_set set v] is [true] when [v] lies in one of the intervals of [set]. *)

val scan_intervals :
  int array -> string array -> bytes -> int -> int -> int -> int
(** [scan_intervals flat tables buf off n i] is the index in [flat] of the first
    test from [i] that the record at [off] fails, or [-1]. [n] is the length of
    [flat]. The reads are not bounds-checked: the record's frame is. *)

val flatten_intervals :
  (Codec_repr.packed_scalar * (int * int) array) array ->
  int array * string array
(** [flatten_intervals tests] is the flat array and the byte tables of the tests
    [tests], each a field and the intervals its value must lie in. *)

val word : size:int -> 'a Codec_repr.scalar -> Codec_words.word option
(** [word ~size s] is the 8-byte word of a record of [size] bytes, at least 8,
    that the big-endian scalar [s] reads as bits of, [None] for a little-endian
    one. *)

val within : bytes -> int -> int -> int -> int -> int -> int -> int -> bool
(** [within buf off q l r m lo hi] is [true] when the field that the 8-byte
    big-endian word at [off + q] holds as {!Codec_words.bits} [l r m] lies
    between [lo] and [hi]. The read is unchecked, so the caller checks the
    record's frame first. *)

val single :
  size:int ->
  (Codec_repr.packed_scalar * (int * int) array) array ->
  (int * int * int * int * int * int) option
(** [single ~size tests] is [Some (q, l, r, m, lo, hi)] when [tests] is one test
    that a record of [size] bytes runs as {!within}. *)

val staged :
  size:int ->
  (Codec_repr.packed_scalar * (int * int) array) array ->
  (bytes -> int -> bool) option
(** [staged ~size tests] is [Some f] when the record of [size] bytes, at least
    8, can run [tests] as reads of its 8-byte words: [f buf off] holds when the
    record at [off] passes every test, read with no bounds check, the frame
    being checked. [None] when a test is not one interval of a big-endian field
    or bitfield, or past 8 tests. *)
