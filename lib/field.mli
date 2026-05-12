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
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  'a Types.typ ->
  'a t
(** [v name typ] creates a named field.

    [?self_constraint] receives a reference to the field being declared and
    returns a constraint over it; the typical use is to prove a later
    size-expression safe, e.g.:

    {[
    open Wire

    let f_len =
      Field.v "Length" uint16be ~self_constraint:(fun self ->
          Expr.(self >= int 7))
    ]}

    projects to [UINT16BE Length {{ Length >= 7 }}], which EverParse's SMT can
    then use to discharge [byte-size (Length - 7)] on a later field. If both
    [?constraint_] and [?self_constraint] are given, the two are combined with
    [And].

    For optional / repeating payloads use {!optional} / {!optional_or} /
    {!repeat} -- those decorations only have a 3D projection at the top of a
    struct field, never as a nested type. *)

val optional :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  present:bool Types.expr ->
  'a Types.typ ->
  'a option t
(** [optional name ~present t] is a field present iff [present] evaluates to
    [true]. Absent fields decode as [None] and consume zero bytes. *)

val optional_or :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  present:bool Types.expr ->
  default:'a ->
  'a Types.typ ->
  'a t
(** [optional_or name ~present ~default t] is a field that decodes to the inner
    value when [present], or [default] when absent. No option wrapper. *)

val repeat :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  size:int Types.expr ->
  'a Types.typ ->
  'a list t
(** [repeat name ~size t] parses elements of type [t] until [size] bytes have
    been consumed. *)

val repeat_seq :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  seq:('a, 'seq) Types.seq_map ->
  size:int Types.expr ->
  'a Types.typ ->
  'seq t
(** Repeat with a custom sequence builder. *)

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

val decl_of_packed : packed -> Types.field
(** [decl_of_packed p] is the {!Types.field} declaration of [p]. *)
