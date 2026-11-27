(** Buffers named by a type.

    [match Buf.v bytes with Buf.Any b -> ...] gives [bytes] a type ['b Buf.t]
    that no other buffer has. A check that a record fits in [b] then returns a
    proof of type ['b ...], which reads accept only together with [b]: a proof
    made on one buffer cannot be used to read another. Naming costs nothing at
    run time: [b] is [bytes] and a proof is an [int]. A buffer the caller holds
    [local] keeps its name [local]. *)

type 'b t = private bytes
(** The type of a buffer named ['b]. *)

type 'b length = private int
(** The length of the buffer named ['b], read once. *)

type any = Any : 'b t -> any [@@unboxed]  (** A buffer under a fresh name. *)

val v : bytes -> any @@ portable
(** [v buf] is [buf] under a name of its own. *)

val local : bytes @ local -> any @ local @@ portable
(** [local buf] is {!v} for a buffer the caller holds [local]: the name is
    [local] too. *)

val[@zero_alloc] length : 'b t @ local -> 'b length @@ portable
(** [length b] is the length of [b]. *)

val pp : Format.formatter -> 'b t -> unit
(** [pp] prints a buffer's length, not its bytes. *)
