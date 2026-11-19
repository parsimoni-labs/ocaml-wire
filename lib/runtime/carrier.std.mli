(** The integer [UInt32] and [SInt32] hold a value in: [Optint.t], the native
    [int] where it has more than 32 bits and a boxed [int32] elsewhere, with the
    operations of [Optint] they use. carrier.ox.mli is the same for OxCaml. *)

type t = Optint.t
(** A value of at least 32 bits. *)

val is_immediate : (t, int, int32) Optint.Conditional.t
(** [is_immediate] is [True] where {!type-t} is the native [int]. *)

val zero : t
(** [zero] is [0]. *)

val [@zero_alloc] of_int : int -> t
(** [of_int n] is [n]. *)

val [@zero_alloc] to_int : t -> int
(** [to_int v] is [v] as an [int]. *)

val of_int32 : int32 -> t
(** [of_int32 n] is [n], its sign kept. *)

val to_int32 : t -> int32
(** [to_int32 v] is [v] as an [int32]. Raises [Invalid_argument] if it does not
    fit in 32 bits. *)

val [@zero_alloc] logand : t -> t -> t
(** [logand a b] is the bitwise and of [a] and [b]. *)

val [@zero_alloc] shift_right_logical : t -> int -> t
(** [shift_right_logical v n] is [v] shifted right by [n] bits, zeroes shifted
    in. *)

val [@zero_alloc] compare : t -> t -> int
(** [compare a b] orders [a] and [b] as [Optint.compare] does. *)

val [@zero_alloc] equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] are the same value. *)

val to_string : t -> string
(** [to_string v] is [v] in decimal. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints a value in decimal. *)
