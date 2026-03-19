(** Wire fields.

    A field is a slot with a wire type, optional constraint, and optional
    action. Named fields ({!v}) can be referenced in expressions via {!ref}.
    Anonymous fields ({!anon}) are padding — they cannot be referenced. *)

type 'a t
(** A field carrying values of type ['a]. *)

val v :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:Types.action ->
  'a Types.typ ->
  'a t
(** [v name typ] creates a named field. *)

val anon : ?action:Types.action -> 'a Types.typ -> 'a t
(** [anon typ] creates an anonymous (padding) field. It cannot be referenced. *)

val ref : 'a t -> int Types.expr
(** [ref f] returns the expression referencing this field. Raises
    [Invalid_argument] on anonymous fields. *)

val name : 'a t -> string
(** Field name. Raises [Invalid_argument] on anonymous fields. *)

val name_opt : 'a t -> string option
(** Field name, or [None] for anonymous fields. *)

val typ : 'a t -> 'a Types.typ
(** Wire type. *)

val constraint_ : 'a t -> bool Types.expr option
(** Field constraint, if any. *)

val action : 'a t -> Types.action option
(** Field action, if any. *)

type packed =
  | Pack : 'a t -> packed
      (** Existentially packed field for heterogeneous lists. *)

val to_decl : 'a t -> Types.field
(** Convert to a {!Types.field} declaration. *)
