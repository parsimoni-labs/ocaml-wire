(** Buffers named by a type.

    [match Buf.v bytes with Buf.Any b -> ...] gives [bytes] a type ['b Buf.t]
    that no other buffer has. A check that a record fits in [b] then returns a
    proof of type ['b ...], which reads accept only together with [b]: a proof
    made on one buffer cannot be used to read another. Naming costs nothing at
    run time: [b] is [bytes] and a proof is an [int]. *)

type 'b t = private bytes
(** The type of a buffer named ['b]. *)

type 'b length = private int
(** The length of the buffer named ['b], read once. *)

(** A buffer under a fresh name. *)
type any = Any : 'b t -> any [@@unboxed]

val v : bytes -> any
(** [v buf] is [buf] under a name of its own. *)

val length : 'b t -> 'b length
(** [length b] is the length of [b]. *)

val pp : Format.formatter -> 'b t -> unit
(** [pp] prints a buffer's length, not its bytes. *)

(** The same for a bigarray of bytes. *)
module Bigstring : sig
  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** A bigarray of bytes. *)

  type 'b t = private bigstring
  (** The type of a bigarray named ['b]. *)

  type 'b length = private int
  (** The length of the bigarray named ['b], read once. *)

  (** A bigarray under a fresh name. *)
  type any = Any : 'b t -> any [@@unboxed]

  val v : bigstring -> any
  (** [v buf] is [buf] under a name of its own. *)

  val length : 'b t -> 'b length
  (** [length b] is the length of [b]. *)

  val pp : Format.formatter -> 'b t -> unit
  (** [pp] prints a bigarray's length, not its bytes. *)
end
