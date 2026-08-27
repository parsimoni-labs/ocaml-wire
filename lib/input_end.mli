(** Where the bytes handed to a parse stop.

    The end of the buffer at the top level, but a {!Wire.nested} region hands
    its value fewer bytes than the buffer holds, and a length field the data
    places can land past the region while still lying inside the buffer. A value
    sized from a byte like that reports a shortfall no byte of the region
    justified, so the limit rides down every buffer walk rather than being
    re-derived from [Bytes.length] wherever a read is checked.

    [private int] with a module of its own, rather than one more [int] argument:
    it travels beside record bases and field offsets, which are [int] too, and
    only the compiler can keep the three from being transposed at the several
    hundred sites that pass them on. *)

type t = private int

val of_bytes : bytes -> t
(** [of_bytes buf] stops at the end of [buf]: the whole buffer was handed to the
    parse. *)

val narrow : t -> int -> t
(** [narrow t n] stops at [n], or where [t] already stops when that comes first:
    a region cannot hand its value bytes the parse around it was not handed
    either. Never before the start of the buffer. *)

val to_int : t -> int
(** [to_int t] is the offset one past the last byte the parse was handed. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer, for the offset an error message quotes. *)
