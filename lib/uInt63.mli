(** Carrier for [uint ~size], the 1-to-7-byte unsigned integers. Seven bytes
    need 56 bits, so every value of every width it serves is represented exactly
    and nothing is masked.

    Backed by {!Optint.Int63}: an unboxed native [int] on a 64-bit host, a boxed
    [int64] where the native [int] is narrower (there a plain-[int] composition
    would shift past the word and lose the high half). *)

type t = Optint.Int63.t

val pp : Format.formatter -> t -> unit
(** Pretty-printer. *)

val check_encode : size:int -> t -> unit
(** [check_encode ~size v] raises [Invalid_argument] when [v] is negative or
    needs more than [size] bytes. Truncating it would leave a legal [size]-byte
    number on the wire that nothing downstream can tell from the one the caller
    meant. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. Exact on a 64-bit host. *)

val of_int : int -> t
(** [of_int n] is [n] as a {!type-t}. *)
