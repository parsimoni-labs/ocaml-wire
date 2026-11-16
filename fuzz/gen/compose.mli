(** Recursive nested composition: arbitrary codecs built by picking a combinator
    and filling its inners, restricted to the compositions Wire accepts. *)

open Gen

(** A gen with its value type hidden, with its byte [size] ([Some n] when fixed)
    and a [label] naming its shape. *)
type any = Any : { g : 'a t; size : int option; label : string } -> any

val printable_byte : int Wire.expr -> bool Wire.expr
(** [printable_byte b] holds when [b] is printable ASCII. *)

val fixed_leaves : any list
(** [fixed_leaves] are the fixed-size leaves, usable as any element or field. *)

val var_leaves : any list
(** [var_leaves] are the self-delimiting and trailing variable-size leaves. *)

val env_leaves : any list
(** [env_leaves] are the leaves whose codec reads a [Param.input]. *)

val array_of : int -> any -> any option
(** [array_of k a] is an array of [k] [a]s, or [None] when [a] is not fixed. *)

val pair_of : ?names:string list -> string -> any -> any -> any
(** [pair_of ?names name a b] is the two-field record [name] over [a] and [b],
    its fields named after [names] when given. *)

val rec3_of : ?names:string list -> string -> any -> any -> any -> any
(** [rec3_of name a b c] is {!pair_of} for three fields. *)

val rec4_of : ?names:string list -> string -> any -> any -> any -> any -> any
(** [rec4_of name a b c d] is {!pair_of} for four fields. *)

val gen_any : int -> any Alcobar.gen
(** [gen_any depth] generates a codec nesting combinators up to [depth] levels
    over the leaves. *)

val flat_check_order : any
(** [flat_check_order] is a flat record whose checks run in field order: a range
    test, then an enum. *)

val wide : int -> any
(** [wide k] is a flat record of [k] columns whose every field has a staged
    write. *)

val flat_table : any
(** [flat_table] is a flat record whose checks share one byte, which the check
    pass folds into a byte table. *)

val flat_fixed_steps : any
(** [flat_fixed_steps] is a fixed flat record with a check no interval states,
    so its check pass runs as steps. *)

val flat_leaves : any list
(** [flat_leaves] is the flat records used as composition leaves, one per fast-
    path shape. *)
