(** Zero-copy record codecs for binary wire formats.

    {2 Access model}

    Three ways to access decoded data, with different trade-offs:

    - {!decode} constructs a full record. It checks field constraints,
      [where]-clauses, and fires field actions. This is the safest path but
      allocates the record value.

    - {!get} / {!set} provide zero-copy field access. {!get} fires the field's
      [~action] (if any), so output parameters are updated. It does {b not}
      check record-level [~where] clauses or other fields' [~constraint_]
      checks. Fields without actions have zero overhead.

    - {!validate} checks all field [~constraint_] checks and [~where] clauses
      without constructing a record and without firing actions. Use it before a
      batch of {!get} calls on untrusted input. *)

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
(** [decode c buf off] decodes a record from [buf] at offset [off].

    Raises {!Types.Parse_error} if the buffer is too short or a field constraint
    or where-clause fails. *)

val env : 'r t -> Param.env
(** [env c] creates a fresh parameter environment for codec [c], with all params
    initialised to 0. *)

val decode_with : 'r t -> Param.env -> bytes -> int -> 'r
(** [decode_with c env buf off] decodes with parameters. Input params are read
    from [env]; output params are written back to [env] after decoding.

    Raises {!Types.Parse_error} on constraint/where-clause failure. *)

val encode : 'r t -> 'r -> bytes -> int -> unit
(** [encode c r buf off] encodes record [r] into [buf] at offset [off].

    Raises [Invalid_argument] if the destination buffer is too short. *)

val to_struct : 'r t -> Types.struct_
(** Project to a {!Types.struct_} declaration. *)

val validate : 'r t -> bytes -> int -> unit
(** [validate c buf off] checks field [~constraint_] and [~where] clauses
    without constructing a record and without firing actions.

    Raises {!Types.Parse_error} on constraint/where-clause failure. *)

val get :
  ?env:Param.env -> 'r t -> ('a, 'r) field -> (bytes -> int -> 'a) Staged.t
(** [get ?env c f] is a staged zero-copy getter for field [f] in codec [c]. If
    [f] has an action, it fires on every read. [env] syncs output parameters
    after each action; fields without actions have zero overhead regardless of
    [env]. Does not check record-level where-clauses or other fields'
    constraints -- call {!validate} first on untrusted input. *)

val set : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a -> unit) Staged.t
(** Staged zero-copy field setter. Does not check constraints or fire actions --
    call {!validate} after a batch of writes to verify constraints still hold.
*)

val raw_decode : 'r t -> bytes -> int -> 'r
(** [raw_decode c buf off] decodes without validation. Internal use. *)

val raw_encode : 'r t -> 'r -> bytes -> int -> unit
(** [raw_encode c r buf off] encodes a record. Internal use. *)

val wire_size_info : 'r t -> [ `Fixed of int | `Variable of bytes -> int -> int ]
(** Wire size information for embedding. *)

val name : 'r t -> string
(** [name c] returns the codec's name. *)

val field_readers : 'r t -> (string * (bytes -> int -> int)) list
(** [field_readers c] returns the int-valued field readers of [c], indexed by
    field name. Used for cross-codec name resolution when [c] is embedded as a
    sub-codec via {!Wire.codec}. *)

val pp : Format.formatter -> 'r t -> unit
(** Pretty-print a codec (shows its name). *)

val field_ref : ('a, 'r) field -> int Types.expr
(** Expression referencing a field by name. *)

type bitfield
(** A bitfield accessor -- shift and mask for one field in a packed word. *)

val bitfield : 'r t -> (int, 'r) field -> bitfield
(** [bitfield codec field] returns a bitfield accessor. Raises if [field] is not
    a bitfield. *)

val load_word : bitfield -> (bytes -> int -> int) Staged.t
(** [load_word bf] returns a staged word reader. Force once, then call the
    resulting function to read the packed base word. Fields sharing the same
    base word return readers that read the same bytes -- call once and pass the
    result to multiple {!extract} calls. *)

val extract : bitfield -> int -> int
(** [extract bf word] extracts the field from a pre-loaded word value. Pure
    shift+mask, no memory access. *)
