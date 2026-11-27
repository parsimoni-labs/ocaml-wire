(** The operands the vectors run the primitives on, OCaml's and OxCaml's alike:
    boundary values and random ones from a fixed seed, and the buffers the loads
    read. *)

val random : Random.State.t
(** [random] is the state the random operands are drawn from. *)

val ints : int list
(** [ints] are the [int] operands. *)

val int64s : int64 list
(** [int64s] are the [int64] operands. *)

val int32s : int32 list
(** [int32s] are the [int32] operands. *)

val lit : int -> string
(** [lit n] is [n] as an F* literal. *)

val pattern64 : int64 -> string
(** [pattern64 w] is the bit pattern of [w], as F* holds an [int64]. *)

val pattern32 : int32 -> string
(** [pattern32 x] is {!pattern64} for an [int32]. *)

val norm : Format.formatter -> string -> unit
(** [norm ppf fact] states [fact], which F* checks by normalising. *)

val buffers : bytes list
(** [buffers] are the buffers the loads read. *)

val seq : bytes -> string
(** [seq b] is [b] as an F* sequence whose every byte Z3 reads in one step. *)
