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
