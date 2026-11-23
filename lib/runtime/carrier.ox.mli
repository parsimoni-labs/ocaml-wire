(** The integer [UInt32] and [SInt32] hold a value in, for OxCaml: the native
    [int], as OxCaml targets only 64-bit hosts, with the semantics of
    [Optint]'s native implementation, which carrier.std.mli re-exports. It is
    written here because [optint]'s interface cannot say its functions are
    [portable]. *)

type t [@@immediate]
(** A value of at least 32 bits. *)

val is_immediate : (t, int, int32) Optint.Conditional.t @@ portable
(** [is_immediate] is [True] where {!type-t} is the native [int]. *)

val zero : t @@ portable
(** [zero] is [0]. *)

val[@zero_alloc] of_int : int -> t @@ portable
(** [of_int n] is [n]. *)

val[@zero_alloc] to_int : t -> int @@ portable
(** [to_int v] is [v] as an [int]. *)

val of_int32 : int32 -> t @@ portable
(** [of_int32 n] is [n], its sign kept. *)

val to_int32 : t -> int32 @@ portable
(** [to_int32 v] is [v] as an [int32]. Raises [Invalid_argument] if it does
    not fit in 32 bits. *)

val[@zero_alloc] logand : t -> t -> t @@ portable
(** [logand a b] is the bitwise and of [a] and [b]. *)

val[@zero_alloc] shift_right_logical : t -> int -> t @@ portable
(** [shift_right_logical v n] is [v] shifted right by [n] bits, zeroes
    shifted in. *)

val[@zero_alloc] compare : t -> t -> int @@ portable
(** [compare a b] orders [a] and [b] as [Optint.compare] does. *)

val[@zero_alloc] equal : t -> t -> bool @@ portable
(** [equal a b] is [true] when [a] and [b] are the same value. *)

val to_string : t -> string @@ portable
(** [to_string v] is [v] in decimal. *)

val pp : Format.formatter -> t -> unit @@ portable
(** [pp] prints a value in decimal. *)
