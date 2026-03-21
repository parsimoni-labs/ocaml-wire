(** Zero-copy record codecs for binary wire formats. *)

type ('a, 'r) field
(** A field bound to a record projection. *)

type 'r t
(** A sealed record codec for type ['r]. *)

type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field
(** [f $ proj] binds a field to a record projection. *)

val v : string -> ?where:bool Types.expr -> 'f -> ('f, 'r) fields -> 'r t
(** [v name constructor fields] seals a codec from a field list. *)

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

type bitfield
(** A bitfield accessor — shift and mask for one field in a packed word. *)

val bitfield : 'r t -> (int, 'r) field -> bitfield
(** [bitfield codec field] returns a bitfield accessor. Raises if [field] is not
    a bitfield. *)

val load_word : bitfield -> (bytes -> int -> int) Staged.t
(** [load_word bf] returns a staged word reader. Force once, then call the
    resulting function to read the packed base word. Fields sharing the same
    base word return readers that read the same bytes — call once and pass the
    result to multiple {!extract} calls. *)

val extract : bitfield -> int -> int
(** [extract bf word] extracts the field from a pre-loaded word value. Pure
    shift+mask, no memory access. *)
