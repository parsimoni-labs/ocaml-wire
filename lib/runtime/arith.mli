(** Native-integer arithmetic that reports overflow instead of wrapping.

    A codec evaluates [+], [-] and [*] in its expressions with {!checked_add},
    {!checked_sub} and {!checked_mul}, which raise the error the codec reports.
    Code generated from a codec uses them where it reports that error, and
    {!add}, {!sub} and {!mul} on its paths that only accept or reject, where an
    overflow rejects. *)

val add_overflows : int -> int -> int -> bool
(** [add_overflows a b s] is [true] when [s], the wrapped [a + b], overflowed.
*)

val sub_overflows : int -> int -> int -> bool
(** [sub_overflows a b d] is [true] when [d], the wrapped [a - b], overflowed.
*)

val mul_overflows : int -> int -> int -> bool
(** [mul_overflows a b p] is [true] when [p], the wrapped [a * b], overflowed.
*)

exception Overflow
(** Raised, without a backtrace, by {!add}, {!sub} and {!mul}. *)

val add : int -> int -> int
(** [add a b] is [a + b]. Raises {!Overflow} when it does not fit. *)

val sub : int -> int -> int
(** [sub a b] is [a - b]. Raises {!Overflow} when it does not fit. *)

val mul : int -> int -> int
(** [mul a b] is [a * b]. Raises {!Overflow} when it does not fit. *)

val checked_add : int -> int -> int
(** [checked_add a b] is [a + b]. Raises {!Errors.Parse_error} with
    {!Errors.constructor-Value_out_of_range} at offset 0 when it does not fit.
*)

val checked_sub : int -> int -> int
(** [checked_sub a b] is [a - b], as {!checked_add}. *)

val checked_mul : int -> int -> int
(** [checked_mul a b] is [a * b], as {!checked_add}. *)
