(** The exception a codec and the code generated from one raise to stop at the
    first error, and the functions that raise it.

    This is plumbing for the code [Wire_ocaml] generates and for the codec
    interpreter, not an API: every decoder catches {!exception-Parse_error} and
    answers its {!Error.t} as a value. *)

exception Parse_error of Error.t

(** {1 Raising}

    What a fixed-size record raises, for code generated from it. Each raises
    {!exception-Parse_error}, or [Invalid_argument] where noted. *)

val negative_offset : int -> 'a
(** [negative_offset off] raises [Invalid_argument]: a record cannot start at
    the negative offset [off]. *)

val eof : at:int -> expected:int -> got:int -> 'a
(** [eof ~at ~expected ~got] raises {!Error.constructor-Unexpected_eof} at [at].
*)

val frame : size:int -> len:int -> int -> 'a
(** [frame ~size ~len off] raises what reading a record of [size] bytes at [off]
    in a buffer of [len] bytes raises when it does not fit: {!negative_offset}
    or {!eof}. *)

val field_constraint : at:int -> field:string -> int -> 'a
(** [field_constraint ~at ~field v] raises a violated constraint of the field
    [field], whose value is [v]. *)

val invalid_enum :
  at:int -> field:string list -> value:int -> valid:int list -> 'a
(** [invalid_enum ~at ~field ~value ~valid] raises
    {!Error.constructor-Invalid_enum}. *)

val invalid_tag : at:int -> field:string -> int -> 'a
(** [invalid_tag ~at ~field tag] raises {!Error.constructor-Invalid_tag}: a
    lookup has no case [tag]. *)

val where_clause : at:int -> 'a
(** [where_clause ~at] raises a violated where clause of the record at [at]. *)

val relocate : at:int -> field:string list -> Error.t -> 'a
(** [relocate ~at ~field e] raises [e] as found at [at] in [field]: where a
    codec reports an overflow in a field's constraint or a where clause. *)

(** {1 Encoding}

    Each raises [Invalid_argument]. *)

val encode_too_short : name:string -> size:int -> len:int -> int -> 'a
(** [encode_too_short ~name ~size ~len off] raises the error for a record [name]
    of [size] bytes that does not fit at [off] in a buffer of [len] bytes. *)

val encode_rejected : name:string -> Error.t -> 'a
(** [encode_rejected ~name e] raises the error for a record [name] whose encoded
    bytes fail its own constraints with [e]. *)

val unsigned_too_wide : bits:int -> int -> 'a
(** [unsigned_too_wide ~bits v] raises the error for [v], which an unsigned
    field of [bits] bits cannot hold. *)

val byte_length : expected:int -> actual:int -> 'a
(** [byte_length ~expected ~actual] raises the error for a byte array of
    [actual] bytes where [expected] are needed. *)

val enum_encode : name:string -> valid:int list -> int -> 'a
(** [enum_encode ~name ~valid v] raises the error for [v], which is not one of
    the cases [valid] of the enum [name]. *)

val check_unsigned : bits:int -> int -> unit
(** [check_unsigned ~bits v] raises {!unsigned_too_wide} unless an unsigned
    field of [bits] bits holds [v]. *)

val check_length : expected:int -> string -> unit
(** [check_length ~expected s] raises {!byte_length} unless [s] has [expected]
    bytes. *)

val check_enum : name:string -> valid:int list -> int -> unit
(** [check_enum ~name ~valid v] raises {!enum_encode} unless [v] is one of the
    cases [valid] of the enum [name]. *)
