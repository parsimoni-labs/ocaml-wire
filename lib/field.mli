(** Wire fields.

    Named fields ({!t}) carry a name, type, constraint, and action. They can be
    referenced in expressions ({!ref}) and bound into codecs with {!Codec.($)}.

    Anonymous fields ({!anon}) are padding -- they have a type but no name and
    cannot be referenced. The type system prevents misuse. *)

type 'a t
(** A named field carrying values of type ['a]. *)

val pp : Format.formatter -> 'a t -> unit
(** Pretty-print the field name. *)

type 'a anon
(** An anonymous (padding) field. Cannot be referenced. *)

val v :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?self_int64:(int64 Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  ?doc:string ->
  'a Types.typ ->
  'a t
(** [v name typ] creates a named field. [?doc] attaches a free-text note (e.g.
    an RFC section) that the documentation projection renders as a [/* ... */]
    comment on the field in the generated 3D.

    [?self_constraint] receives a reference to the field being declared and
    returns a constraint over it, e.g.:

    {[
    open Wire

    let f_len =
      Field.v "Length" uint16be ~self_constraint:(fun self ->
          Expr.(self <= int 1500))
    ]}

    projects to [UINT16BE Length {{ Length <= 1500 }}]. A later span sized
    [Length - 7] needs no constraint here: the projection states [Length >= 7]
    on [Length] itself. [?self_int64] is the same convenience for full-width
    64-bit field constraints, using {!Expr.int64} literals and {!int64}
    references. If [?constraint_] and a self constraint are both given, the
    constraints are combined with [And].

    For optional / repeating payloads use {!optional} / {!optional_or} /
    {!repeat} -- those decorations only have a 3D projection at the top of a
    struct field, never as a nested type. *)

val optional :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?self_int64:(int64 Types.expr -> bool Types.expr) ->
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
  ?self_int64:(int64 Types.expr -> bool Types.expr) ->
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
  ?self_int64:(int64 Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  size:int Types.expr ->
  'a Types.typ ->
  'a list t
(** [repeat name ~size t] parses elements of type [t] until exactly [size] bytes
    have been consumed. Encoding rejects values whose encoded size differs from
    the budget. *)

val repeat_seq :
  string ->
  ?constraint_:bool Types.expr ->
  ?self_constraint:(int Types.expr -> bool Types.expr) ->
  ?self_int64:(int64 Types.expr -> bool Types.expr) ->
  ?action:Types.action ->
  seq:('a, 'seq) Types.seq_map ->
  size:int Types.expr ->
  'a Types.typ ->
  'seq t
(** Repeat with a custom sequence builder and the same exact byte budget. *)

val union :
  string ->
  ?action:Types.action ->
  ?doc:string ->
  on:'k t ->
  casetype:string ->
  ('a, 'k) Types.case_def list ->
  'a t
(** [union name ~on ~casetype cases] is a field holding one of [cases], selected
    by the value of the earlier field [on]. Unlike {!Types.casetype}, whose tag
    is the first bytes of the value itself, the tag here is a field of its own,
    so other fields may sit between it and the union, and several unions may
    switch on the same tag. The union occupies the bytes of the selected case
    only.

    [on] must be integer-valued (an integer, a bitfield, an enum or a
    [variants]) and declared before the union; every {!Types.case} gives its
    [~index] in [on]'s type. Decoding a tag no case claims fails with
    [Invalid_tag] unless a {!Types.default} branch catches it. Encoding raises
    [Invalid_argument] when the value's case is not the one the tag field holds.

    Projects to a 3D [casetype] named [casetype], taking the tag as its
    parameter, and to a field [casetype(on) name]. *)

val anon : 'a Types.typ -> 'a anon
(** [anon typ] creates an anonymous (padding) field. *)

val ref : 'a t -> int Types.expr
(** [ref f] returns the expression referencing this field's underlying integer
    value. Works on any field whose wire type is or wraps an integer, including
    [bool] fields created with {!Wire.bit}. *)

val int : 'a t -> int Types.expr
(** [int f] returns this field as a native-integer expression. *)

val int64 : 'a t -> int64 Types.expr
(** [int64 f] returns the expression referencing this field's full 64-bit value,
    without truncating to OCaml's native integer range. Raises
    [Invalid_argument] on a field with no 64-bit slot; the accepted set is the
    one {!Types.has_int64_slot} names, which is wider than any single carrier
    type ([uint64], [int64] and [float64] all have one). *)

val name : 'a t -> string
(** Field name. *)

val id : 'a t -> int
(** Internal identity shared by bindings made from this field. *)

val typ : 'a t -> 'a Types.typ
(** Wire type. *)

val constraint_ : 'a t -> bool Types.expr option
(** Field constraint, if any. *)

val action : 'a t -> Types.action option
(** Field action, if any. *)

val doc : 'a t -> string option
(** Field note attached via {!v}'s [?doc], if any. *)

(** Existentially packed field for heterogeneous lists. *)
type packed = Named : 'a t -> packed | Anon : 'a anon -> packed

val decl_of_packed : packed -> Types.field
(** [decl_of_packed p] is the {!Types.field} declaration of [p]. *)
