(** Zero-copy record codecs for binary wire formats. *)

type ('a, 'r) field
(** A field specification for a value of type ['a] in a record of type ['r]. *)

type 'r t
(** A sealed record codec for type ['r]. *)

type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

val field :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:Types.action ->
  'a Types.typ ->
  ('r -> 'a) ->
  ('a, 'r) field
(** Declare a named field with optional constraint and action. *)

val view :
  string ->
  ?params:Types.param list ->
  ?where:bool Types.expr ->
  'f ->
  ('f, 'r) fields ->
  'r t
(** Seal a list of fields into a record codec. *)

val wire_size : 'r t -> int
(** Fixed wire size in bytes. Raises if variable-length. *)

val min_wire_size : 'r t -> int
(** Minimum wire size in bytes (for variable-length codecs). *)

val wire_size_at : 'r t -> bytes -> int -> int
(** Compute the actual wire size from a buffer at a given offset. *)

val is_fixed : 'r t -> bool
(** [true] iff the codec has a fixed wire size. *)

val decode : ?env:Param.env -> 'r t -> bytes -> int -> 'r
(** Decode a record from [bytes] at offset. *)

val encode : 'r t -> 'r -> bytes -> int -> unit
(** Encode a record into [bytes] at offset. *)

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
