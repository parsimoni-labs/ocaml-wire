(** Wire fields.

    Named fields ({!t}) carry a name, type, constraint, and action. They can be
    referenced in expressions ({!ref}) and bound into codecs with {!Codec.($)}.

    Anonymous fields ({!anon}) are padding -- they have a type but no name and
    cannot be referenced. The type system prevents misuse. *)

type 'a t
(** A named field carrying values of type ['a]. *)

val pp : Format.formatter -> 'a t -> unit

type 'a anon
(** An anonymous (padding) field. Cannot be referenced. *)

val v :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:Types.action ->
  'a Types.typ ->
  'a t
(** [v name typ] creates a named field. *)

val anon : 'a Types.typ -> 'a anon
(** [anon typ] creates an anonymous (padding) field. *)

val ref : 'a t -> int Types.expr
(** [ref f] returns the expression referencing this field's underlying integer
    value. Works on any field whose wire type is or wraps an integer, including
    [bool] fields created with {!Wire.bit}. *)

val name : 'a t -> string
(** Field name. *)

val typ : 'a t -> 'a Types.typ
(** Wire type. *)

val constraint_ : 'a t -> bool Types.expr option
(** Field constraint, if any. *)

val action : 'a t -> Types.action option
(** Field action, if any. *)

type packed =
  | Named : 'a t -> packed
  | Anon : 'a anon -> packed
      (** Existentially packed field for heterogeneous lists. *)

val to_decl : packed -> Types.field
(** Convert a packed field to a {!Types.field} declaration. *)
