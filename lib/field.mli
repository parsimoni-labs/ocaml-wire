(** Named wire fields.

    A field is a named slot with a wire type, optional constraint, and optional
    action. Fields are used both for codec construction ({!Wire.Codec.bind}) and
    3D struct declarations ({!Wire.C.struct_}). *)

type 'a t
(** A named field carrying values of type ['a]. *)

val v :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:Types.action ->
  'a Types.typ ->
  'a t
(** [v name typ] creates a named field. *)

val ref : 'a t -> int Types.expr
(** [ref f] returns the expression referencing this field by name. *)

val name : 'a t -> string
(** Field name. *)

val typ : 'a t -> 'a Types.typ
(** Wire type. *)

val constraint_ : 'a t -> bool Types.expr option
(** Field constraint, if any. *)

val action : 'a t -> Types.action option
(** Field action, if any. *)

val to_decl : 'a t -> Types.field
(** Convert to a {!Types.field} declaration (for struct construction). *)
