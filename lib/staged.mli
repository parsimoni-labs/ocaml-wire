(** Staged computations.

    Forces users to explicitly unstage functions to make specialization visible.
    The cost of building a specialized function is paid once when {!unstage} is
    called. *)

type +'a t

val stage : 'a -> 'a t
val unstage : 'a t -> 'a
