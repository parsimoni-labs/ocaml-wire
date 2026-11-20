(** A fixed record whose every field is read as bits of an 8-byte big-endian
    word of the record: one load per word, no dispatch and no call per field.
    Internal to {!Codec}. *)

type word = { q : int; l : int; r : int; m : int; same : bool }
(** A field read as bits of the word at [q] in its record: the word shifted left
    by [l], then right by [r] keeping its sign, then masked by [m], all ones for
    a signed field. {!field-same} when the field before it reads the same word.
*)

(** The words of a record's fields, in field order, each with the proof that its
    field's type is [int]. *)
type (_, _) words =
  | WNil : ('r, 'r) words
  | WCons :
      word * ('a, int) Type.eq * ('rest, 'result) words
      -> ('a -> 'rest, 'result) words

val load : bytes -> int -> int -> int64
(** [load buf base q] is the big-endian 8-byte word at [base + q] of [buf], read
    with no bounds check outside the [paranoid] profile. *)

val bits : int64 -> int -> int -> int -> int
(** [bits w l r m] is [w] shifted left by [l], then right by [r] keeping its
    sign, then masked by [m]. *)

val bit_width : int -> int
(** [bit_width m] is the number of bits of the mask [m], a run of low ones. *)

val word : size:int -> int -> bytes:int -> s:int -> w:int -> signed:bool -> word
(** [word ~size o ~bytes ~s ~w ~signed] reads the [w] bits above bit [s] of the
    [bytes] bytes at [o], in a record of [size] bytes, at least 8: from the
    8-byte word that starts the record's 8 bytes holding [o], else from the one
    at [o], else from the record's last. {!field-same} is [false]. *)

val stage :
  short:(int -> bytes -> int -> 'result) ->
  size:int ->
  'mid ->
  ('mid, 'result) words ->
  (bytes -> int -> 'result) option
(** [stage ~short ~size f ws] decodes a record of [size] bytes at an offset:
    [short size buf off] when [buf] does not hold it there, else [f] applied to
    every field's value in one saturated call. [None] for no field or more than
    32. *)
