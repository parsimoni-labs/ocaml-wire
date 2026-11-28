(** Deterministic CBOR (RFC 8949 section 4.2.1) below the CBOR types.

    Heads, strings, map key order and whole data items, as EverCBOR's
    deterministic API accepts them (docs/cbor.md, Deterministic encoding and who
    enforces it): definite lengths, every head argument in the shortest form,
    map keys in strictly increasing bytewise lexicographic order of their
    encodings, text in UTF-8, simple values other than [24] to [31], any tag,
    and no floating-point number. A reader takes the bytes [off] to [limit] of a
    buffer, never reads outside them, and answers bad bytes with an [Error],
    never an exception. A writer emits the one deterministic encoding.

    An argument is the 64 bits of the head as an [int64], read unsigned: [-1L]
    is [2{^64}-1]. *)

(** {1:errors Errors} *)

(** The type for the rules bytes break. *)
type reason =
  | Eof of int
      (** [Eof n]: [n] bytes are needed where the limit leaves less. *)
  | Reserved  (** Additional information [28] to [30]. *)
  | Indefinite
      (** Additional information [31]: an indefinite length or a break. *)
  | Not_shortest  (** A head argument that fits in a shorter head. *)
  | Float  (** A floating-point number, which EverCBOR refuses. *)
  | Simple  (** A simple value below [32] in a two-byte head. *)
  | Utf8  (** A text string that is not UTF-8 (RFC 3629). *)
  | Unsorted  (** A map key that comes before the previous key. *)
  | Duplicate  (** A map key equal to the previous key. *)
  | Trailing of int  (** [Trailing n]: [n] bytes after the data item. *)

type error = { at : int; reason : reason }
(** The type for errors: the offset where the bytes break the rule [reason]. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error] prints an error as [reason@at]. *)

(** {1:heads Heads} *)

type head = {
  major : int;  (** The major type, [0] to [7]. *)
  arg : int64;  (** The argument: a value, a length, a count or a tag. *)
  next : int;  (** Where the head ends. *)
}
(** The type for heads (RFC 8949 section 3). For major type [7], [arg] is the
    simple value: [20] and [21] are [false] and [true], [22] is [null]. *)

val head : bytes -> off:int -> limit:int -> (head, error) result
(** [head buf ~off ~limit] is the head at [off], in the shortest form. Raises
    [Invalid_argument] if [off] and [limit] are not within [buf]. *)

val string_end : bytes -> head -> limit:int -> (int, error) result
(** [string_end buf h ~limit] is where the content of the byte string or text
    string of head [h] ends, before [limit]; the content of a text string is
    UTF-8. Raises [Invalid_argument] if [h] is not of major type [2] or [3]. *)

val ordered_keys : bytes -> int * int -> int * int -> (unit, error) result
(** [ordered_keys buf (s, e) (s', e')] is [Ok ()] iff the map key encoded at [s]
    to [e] comes strictly before the one at [s'] to [e'], bytewise
    lexicographic; the error is at [s']. *)

(** {1:items Data items} *)

(** Nesting has no bound, as in EverCBOR, whose deterministic validator accepts
    its test vector of [2200] nested arrays. Every function below reads, writes
    or prints in a loop, not by recursion: no data item exhausts the stack, and
    memory stays linear in the bytes. *)

val skip : bytes -> off:int -> limit:int -> (int, error) result
(** [skip buf ~off ~limit] checks the one data item at [off], before [limit],
    and is where it ends: CDDL's [any]. Raises [Invalid_argument] as {!head}
    does. *)

(** The type for data items. Integers carry the argument of their head. *)
type t =
  | Uint of int64  (** [Uint n] is the unsigned integer [n]. *)
  | Nint of int64  (** [Nint n] is the negative integer [-1 - n]. *)
  | Bytes of string  (** A byte string. *)
  | Text of string  (** A text string. *)
  | Array of t list  (** An array. *)
  | Map of (t * t) list  (** A map, its entries in any order. *)
  | Tag of int64 * t  (** [Tag (n, v)] is [v] under tag [n]. *)
  | Simple of int  (** A simple value. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints a data item in diagnostic notation (RFC 8949 section 8). *)

val decode : bytes -> off:int -> limit:int -> (t * int, error) result
(** [decode buf ~off ~limit] is the data item at [off], before [limit], and
    where it ends. Its map entries come in the order of the bytes. Raises
    [Invalid_argument] as {!head} does. *)

val of_string : string -> (t, error) result
(** [of_string s] is the one data item [s] holds, with nothing after it. *)

(** {1:writing Writing} *)

val write_head : Buffer.t -> int -> int64 -> unit
(** [write_head b major arg] adds the head of [major] and [arg], in the shortest
    form. Raises [Invalid_argument] if [major] is not [0] to [7] or if [major]
    is [7] and [arg] is not a simple value. *)

val write_map : Buffer.t -> (string * string) list -> unit
(** [write_map b entries] adds the map of the encoded keys and values [entries],
    in the order of their keys. Raises [Invalid_argument] on two equal keys. *)

val encode : Buffer.t -> t -> unit
(** [encode b v] adds the deterministic encoding of [v]. Raises
    [Invalid_argument] on a text string that is not UTF-8, a simple value
    outside [0] to [23] and [32] to [255], and two equal keys in one map. *)

val to_string : t -> string
(** [to_string v] is the deterministic encoding of [v], as {!encode}. *)
