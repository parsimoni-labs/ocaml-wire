(** Zero-copy record codecs for binary wire formats. *)

type ('a, 'r) field
(** A field specification for a value of type ['a] in a record of type ['r]. *)

type 'r t
(** A sealed record codec for type ['r]. *)

type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

val bind : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field
(** [bind f proj] binds a field to a record projection for codec use. *)

val field :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:Types.action ->
  'a Types.typ ->
  ('r -> 'a) ->
  ('a, 'r) field
(** [field name typ proj] is [bind (Field.v name typ) proj]. Convenience
    shorthand when the field is not referenced elsewhere. *)

val view : string -> ?where:bool Types.expr -> 'f -> ('f, 'r) fields -> 'r t
(** Seal a list of fields into a record codec. *)

val wire_size : 'r t -> int
(** Fixed wire size in bytes. Raises if variable-length. *)

val min_wire_size : 'r t -> int
(** Minimum wire size in bytes (for variable-length codecs). *)

val wire_size_at : 'r t -> bytes -> int -> int
(** Compute the actual wire size from a buffer at a given offset. *)

val is_fixed : 'r t -> bool
(** [is_fixed c] is [true] iff the codec [c] has a fixed wire size. *)

val decode : 'r t -> bytes -> int -> 'r
(** [decode c buf off] decodes a record from [buf] at offset [off]. *)

val encode : 'r t -> 'r -> bytes -> int -> unit
(** [encode c r buf off] encodes record [r] into [buf] at offset [off]. *)

val to_struct : 'r t -> Types.struct_
(** Project to a {!Types.struct_} declaration. *)

val get : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a) Staged.t
(** Staged zero-copy field getter. *)

val set : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a -> unit) Staged.t
(** Staged zero-copy field setter. *)

val pp : Format.formatter -> 'r t -> unit
(** Pretty-print a codec (shows its name). *)

val field_ref : ('a, 'r) field -> int Types.expr
(** Expression referencing a field by name. *)
