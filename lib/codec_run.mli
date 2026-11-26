(** Records of at most 7 bytes as the [int] of their bytes, big-endian: the
    writes of {!Codec.writer} and the reads of {!Codec.Word}. Internal to
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

(** The reads of {!Codec.Word}. *)
module Word : sig
  type 'r t = int
  (** The bytes of a record of a ['r] codec, big-endian. *)

  type ('a, 'r) field
  (** A field of a record's word. *)

  val field :
    ('a, int) Type.eq -> pos:int -> width:int -> signed:bool -> ('a, 'r) field
  (** [field eq ~pos ~width ~signed] is the field of [width] bits above bit
      [pos] of a word, sign-extended when [signed]. *)

  type 'r reader
  (** A reader of the records of a ['r] codec. *)

  val reader : int -> fields:(string * int * int) list -> 'r reader
  (** [reader n ~fields] reads records of [n] bytes whose fields, in order, are
      [fields]: each one's name, offset in the record and width. *)

  type 'r check = int
  (** The record's word, or [-1] for a record that does not lie in the buffer.
  *)

  val check : 'r reader -> bytes -> int -> 'r check
  (** [check r buf off] is the record at [off], in one 8-byte load where [buf]
      has 8 bytes from [off] on. Never raises. *)

  val ok : 'r check -> bool
  (** [ok k] is [true] iff [k] is a record's word. *)

  val get : 'r check -> 'r t
  (** [get k] is the word of [k]. Raises [Invalid_argument] unless [ok k]. *)

  val error : 'r reader -> bytes -> int -> Types.parse_error
  (** [error r buf off] is the error {!Codec.get} answers for the first field
      [buf] cuts. Raises [Invalid_argument] for a negative [off] or a record
      [check] accepts. *)

  val read : ('a, 'r) field -> 'r t -> 'a
  (** [read p w] is the field [p] of [w]. *)
end
