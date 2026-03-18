(** Expression evaluator, action interpreter, and parsing context.

    Shared by {!Wire} (streaming decode) and {!Codec} (zero-copy access). Not
    part of the public API — consumers use {!Wire.decode} or {!Codec.decode}
    instead. *)

(** {1 Context} *)

type ctx
(** Evaluation context: field bindings, sizeof_this, field_pos. *)

val empty : ctx
(** Empty context. *)

val bind : ctx -> string -> int -> ctx
(** [bind ctx name v] adds a field binding. *)

val set_pos : ctx -> sizeof_this:int -> field_pos:int -> ctx
(** Update the positional metadata for the current field. *)

(** {1 Type-to-int conversion} *)

val int_of : 'a Types.typ -> 'a -> int
(** [int_of typ v] converts a typed value to [int] for context storage. *)

(** {1 Expression evaluation} *)

val expr : ctx -> 'a Types.expr -> 'a
(** Evaluate an expression in the given context. *)

(** {1 Action execution} *)

val action : ctx -> Types.action option -> ctx
(** [action ctx act] executes an optional action block, returning the updated
    context. Raises {!Types.Parse_error} on [return false] or [abort]. *)
