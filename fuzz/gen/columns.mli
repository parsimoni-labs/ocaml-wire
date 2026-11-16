(** Flat records: columns laid side by side as the fields of one codec.

    {!Wire.Codec} picks its fast paths from a record's own field list. The
    compositions of {!Record} wrap every field that is itself a record as a
    sub-codec, so they seldom lay such fields side by side; a flat record does.
    Internal to {!Fuzz_gen}. *)

open Gen

type cell
(** The value of one column. *)

type column
(** One field of a flat record, with its value as a {!cell}. *)

type group
(** Adjacent columns drawn together, so a length and the span it sizes stay
    consistent. *)

type group_mk = { glabel : string; mk : string -> group }
(** A group to instantiate under a column name, and the label of its shape. *)

(** How a span's size reads its length field. *)
type span_size = Plain | Plus of int | Times of int

type len_kind
(** A length field: its column, and the reference a span's size reads. *)

val u8_group : group_mk
(** [u8_group] is one [uint8] column. *)

val i8_group : group_mk
(** [i8_group] is one [int8] column. *)

val u16be_group : group_mk
(** [u16be_group] is one [uint16be] column. *)

val u16le_group : group_mk
(** [u16le_group] is one [uint16] column. *)

val i16be_group : group_mk
(** [i16be_group] is one [int16be] column. *)

val u32be_group : group_mk
(** [u32be_group] is one [uint32be] column. *)

val u32le_group : group_mk
(** [u32le_group] is one [uint32] column. *)

val range8_group : lo:int -> hi:int -> group_mk
(** [range8_group ~lo ~hi] is a [uint8] whose own constraint keeps it between
    [lo] and [hi]: an interval test. *)

val even8_group : group_mk
(** [even8_group] is a [uint8] with a constraint no interval states: a general
    check step. *)

val enum8_group : (string * int) list -> group_mk
(** [enum8_group cases] is a closed enum over a [uint8]: the check pass tests
    its cases as a set. *)

val where8_group : group_mk
(** [where8_group] is a [uint8] under a [where]. *)

val map8_group : group_mk
(** [map8_group] is a [uint8] under a [map]. *)

val bits_group : ?limits:int list -> Wire.bitfield -> int list -> group_mk
(** [bits_group ?limits base widths] is bitfields of [widths] sharing one [base]
    word. Each of [limits] constrains the bitfield of the same rank, which the
    check pass folds into one byte table. *)

val fixed_bytes_group : slice:bool -> int -> group_mk
(** [fixed_bytes_group ~slice n] is a byte array of [n] bytes, a byte slice when
    [slice]: a blit when it is an array. *)

val span_len : span_size -> int -> int * int
(** [span_len size n] is the length field's value for a span of [n] bytes, and
    [n] rounded to a length [size] can describe. *)

val span_group : ?size:span_size -> ?slice:bool -> len_kind -> group_mk
(** [span_group ?size ?slice lk] is the length field [lk] and the byte span it
    sizes through [size], a byte slice when [slice]. *)

val len_u8 : len_kind
(** [len_u8] is a [uint8] length field. *)

val len_u16be : len_kind
(** [len_u16be] is a [uint16be] length field. *)

val len_u16le : len_kind
(** [len_u16le] is a [uint16] length field. *)

val len_u32be : len_kind
(** [len_u32be] is a [uint32be] length field. *)

val len_map8 : len_kind
(** [len_map8] is a [uint8] length field under a [map]. *)

val rest_group : group_mk
(** [rest_group] is everything left: a [Wire.all_bytes] tail, which only ends a
    record. *)

val flapping : cell array -> cell array -> int -> cell array
(** [flapping a b phase] is a value whose getters alternate between the records
    [a] and [b], starting from [phase]: what an encoder that reads a field twice
    gets wrong. *)

val flat : ?impure:bool -> string -> group list -> cell array t
(** [flat ?impure name groups] generates for the record [name] whose fields are
    the columns of [groups], side by side in one [Wire.Codec.v]. With [impure],
    positives include values whose getters flap. *)

val flat_of : ?impure:bool -> string -> group_mk list -> string * cell array t
(** [flat_of ?impure name mks] is the label of the shape [mks] and {!flat} of
    its groups, with columns named by rank. *)
