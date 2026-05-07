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

val env : 'r t -> Param.env
(** [env c] creates a fresh parameter environment for codec [c], with all params
    initialised to 0. *)

val decode :
  ?env:Param.env -> 'r t -> bytes -> int -> ('r, Types.parse_error) result
(** [decode ?env c buf off] decodes a record from [buf] at offset [off].

    If [?env] is supplied, input params are read from it and output params are
    written back to it after decoding. *)

val decode_exn : ?env:Param.env -> 'r t -> bytes -> int -> 'r
(** [decode_exn ?env c buf off] is like {!decode} but raises
    {!Types.Parse_error} on failure. *)

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

val slice_offset :
  'r t -> (Bytesrw.Bytes.Slice.t, 'r) field -> (bytes -> int -> int) Staged.t
(** [slice_offset c f] is a staged reader that returns the absolute byte offset
    of slice field [f] within the buffer (i.e. [base + relative_off]).

    The naive [Slice.first (Codec.get c f buf base)] pattern allocates a fresh
    [Slice.t] (4 words) inside [Codec.get] only to discard it after extracting
    one int. [slice_offset] skips the make and returns the int directly: stage
    once with [Codec.slice_offset c f |> Staged.unstage], then call the
    resulting [buf -> base -> int] reader on the hot path.

    Type-restricted to [Slice.t] fields, so passing a non-slice field is a
    compile-time error. *)

val slice_length :
  'r t -> (Bytesrw.Bytes.Slice.t, 'r) field -> (bytes -> int -> int) Staged.t
(** [slice_length c f] is a staged reader returning the byte length of slice
    field [f]. *)

type validator
(** A struct validator without a constructor. The same int-array validation
    kernel that backs {!validate} on a [Codec.t], but built directly from a
    {!Types.struct_}. Used by [Wire.of_string] /[Wire.decode] for [Struct] types
    so all struct validation goes through the same code path. *)

val validator_of_struct : Types.struct_ -> validator
(** [validator_of_struct s] compiles [s] into a validator. Constraints, [where]
    clauses, and per-field actions are compiled to operate on a per-decode int
    array; the resulting validator is reusable. *)

val validate_struct : validator -> bytes -> int -> unit
(** [validate_struct v buf off] runs the validator. Raises {!Types.Parse_error}
    on failure. *)

val struct_size_of : validator -> bytes -> int -> int
(** [struct_size_of v buf off] is the byte size of the struct starting at [off].
    Independent of [buf] for fixed-size structs; inspects the buffer otherwise.
*)

val struct_min_size : validator -> int
(** Minimum byte size accepted. *)

val wire_size_info_of_validator :
  validator -> [ `Fixed of int | `Variable of bytes -> int -> int ]
(** Wire-size info (parallels {!wire_size_info} for codecs). *)

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
