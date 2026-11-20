(** Records of at most 7 bytes as the [int] of their bytes, big-endian: the
    writes of {!Codec.writer} and the reads of {!Codec.word_reader}. Internal to
    {!Codec}. *)

(** {1 Writes} *)

type 'a slot = {
  eq : ('a, int) Type.eq;
  hi : int;
  wm : int;
  pos : int;
  width : int;
}
(** Where a field's value goes in the record's [int]: the proof its value is an
    [int], the bits of a value too wide for a bitfield ([hi], 0 for a field
    whose carrier is its range), the mask of its own bits, its shift and its
    width in bits. *)

(** A field a writer writes: into the record's [int], or by its own [set]. *)
type 'a wfield = Slot of 'a slot | Setter of (bytes -> int -> 'a -> unit)

(** The fields of a writer, in the order of its arguments. *)
type ('f, 'r) wlist =
  | WNil : (unit, 'r) wlist
  | WCons : 'a wfield * ('f, 'r) wlist -> ('a -> 'f, 'r) wlist

val run_of_bytes : bytes -> int -> int
(** [run_of_bytes b n] is the [n] first bytes of [b] as an [int], big-endian. *)

val slot :
  ('a, int) Type.eq ->
  n:int ->
  at:int ->
  nb:int ->
  s:int ->
  w:int ->
  bits:bool ->
  'a slot
(** [slot eq ~n ~at ~nb ~s ~w ~bits] is the slot of a field at [at] holding [nb]
    bytes, [w] bits of them above bit [s] of its word, in a record of [n] bytes;
    [bits] for a bitfield, whose values are checked against [w]. *)

val all_slots : ('f, 'r) wlist -> bool
(** [all_slots ws] is [true] when every field of [ws] is a {!Slot}. *)

val clear_slots : ('f, 'r) wlist -> int -> int
(** [clear_slots ws t] is [t] with the bits of the slots of [ws] cleared. *)

val run_writer :
  name:string -> n:int -> int -> ('f, 'r) wlist -> bytes -> int -> 'f
(** [run_writer ~name ~n t ws buf off] takes one argument per slot of [ws] and
    writes at [off] the record [t] of [n] bytes with those slots set from them,
    after every check. *)

val set_writer :
  name:string -> n:int -> bytes -> ('f, 'r) wlist -> bytes -> int -> 'f
(** [set_writer ~name ~n b ws buf off] takes one argument per setter of [ws],
    copies the [n] bytes of [b] to [off], then calls each setter with its
    argument. *)

(** {1 Reads} *)

type 'r word = int
(** The bytes of a record of a ['r] codec, big-endian. *)

type ('a, 'r) part
(** A field of a record word. *)

val part :
  ('a, int) Type.eq -> pos:int -> width:int -> signed:bool -> ('a, 'r) part
(** [part eq ~pos ~width ~signed] is the field of [width] bits above bit [pos]
    of a word, sign-extended when [signed]. *)

type 'r reader
(** A reader of the records of a ['r] codec. *)

val reader : int -> 'r reader
(** [reader n] reads records of [n] bytes. *)

val read_word : 'r reader -> bytes -> int -> 'r word
(** [read_word r buf off] is the record at [off], in one 8-byte load where [buf]
    has 8 bytes from [off] on. Raises [Invalid_argument] when [buf] does not
    hold the record at [off]. *)

val of_word : ('a, 'r) part -> 'r word -> 'a
(** [of_word p w] is the field [p] of [w]. *)
