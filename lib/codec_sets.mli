(** Sets of ints as sorted, disjoint intervals: the values a constraint on a
    field allows. Internal to {!Codec}. *)

val set_inter : (int * int) list -> (int * int) list -> (int * int) list
(** [set_inter a b] is the values in both [a] and [b]. *)

val set_union : (int * int) list -> (int * int) list -> (int * int) list
(** [set_union a b] is the values in [a] or [b]. *)

val all_ints : (int * int) list
(** [all_ints] is every [int]. *)

val self_set : string -> bool Types.expr -> (int * int) list option
(** [self_set name e] is the values of the field [name] that [e] accepts, when
    [e] only compares the field with literals, through [&&] and [||]; [None] for
    any other condition. *)
