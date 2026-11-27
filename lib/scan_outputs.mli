(** The outputs of a record as a scanner hands them out, read by walking the
    record's description over bytes the codec accepts. Internal to {!Codec}. *)

type t
(** The type for the outputs of one record, by name. *)

val names : Types.struct_ -> (string * bool) list
(** [names s] is the outputs of a record of [s], in declaration order: each
    named scalar field, its name the path of field labels joined with ['_'], and
    for each optional field [<path>_present], flagged [true]. Raises
    [Invalid_argument] on a field a scanner does not hand out or walk past. *)

val read : Types.struct_ -> bytes -> int -> t
(** [read s buf off] is the outputs of the record of [s] at [off], an absent
    field as [0] or [false]. The record must be one the codec of [s] accepts. *)

val values : t -> int64 array
(** [values o] is the outputs of [o] in the order of {!names}, a presence as
    [1L] or [0L]. *)

val int : t -> string -> int64
(** [int o name] is the integer output [name] of [o]. Raises [Invalid_argument]
    if [o] has no integer output [name]. *)

val bool : t -> string -> bool
(** [bool o name] is the presence [name] of [o]. Raises [Invalid_argument] if
    [o] has no presence [name]. *)
