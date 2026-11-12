(** Encode-side guards for the Wire DSL.

    Decode rejects each value these guards refuse, so emitting one would put
    bytes on the wire that this library's own decoder, and the EverParse
    validator generated from the same schema, refuse. Each raises
    [Invalid_argument]: an unencodable value is a caller error, not malformed
    input. {!Types} re-exports this module. *)

open Dsl

val raise_no_matching_case : unit -> 'a
(** Raise [Invalid_argument] for a casetype value no case projects. Such a value
    has no encoding, so it has no size either: the size path and both encode
    paths raise through here so all three say the same thing. *)

val enum_values : (string * int) list -> int list
(** The value set of an enum's named cases. *)

val enum_member : int list -> int -> bool
(** [enum_member valid v] is [true] when [v] is one of [valid]. The single
    membership rule decode and encode share, so the two halves agree on what a
    closed enum admits. *)

val check_enum_decode : at:int -> cases:(string * int) list -> int -> unit
(** Check a decoded value against a closed enum's named cases. An unlisted value
    raises {!Dsl.constructor-Invalid_enum} carrying the case set. Scans the
    cases in place, so a value that is a member costs no allocation however many
    cases there are. *)

val check_enum_encode : name:string -> valid:int list -> int -> unit
(** Check a value against a closed enum's case set before encoding it. An
    unlisted value raises [Invalid_argument]: decode rejects it, so emitting it
    would produce bytes this library cannot read back. *)

val check_all_zeros_encode : string -> unit
(** Check an {!Dsl.val-all_zeros} padding value is all zero bytes before
    encoding it. A non-zero byte raises [Invalid_argument]. *)

val check_zeroterm_encode : string -> unit
(** Check a {!Dsl.val-zeroterm} value carries no NUL before encoding it. An
    embedded NUL raises [Invalid_argument]: it terminates the string on the
    wire, so the bytes would decode back short. Internal helper shared by every
    encoder path. *)

val check_zeroterm_region : region:int -> len:int -> unit
(** Check a {!Dsl.val-zeroterm_at_most} value of [len] bytes leaves room for its
    NUL terminator inside a [region]-byte span. Internal helper shared by every
    encoder path. *)

val check_unsigned_encode : bits:int -> int -> unit
(** Check an integer fits an unsigned [bits]-wide field before encoding it. The
    accepted range is [[0, 2^bits - 1]], exactly what the decoder produces, so
    encode stays inverse to decode; anything else raises [Invalid_argument],
    because masking it would put a different, equally legal, number on the wire
    that no later check can tell from the one the caller meant. Used by
    [bits ~width], the one shape still carried in a bare [int]. Where the native
    [int] is no wider than the slice the guard narrows to what is still
    checkable, so it never rejects a value the target can represent. *)

val check_where_encode : bool expr -> bool -> unit
(** [check_where_encode cond ok] raises [Invalid_argument] when [ok] is [false],
    naming [cond] in the message. Used by the encoders to refuse a value its own
    [where] refinement rejects. *)

(** {1 Checked scalar writers}

    The signed 32-bit writers, under the names the encode paths use. Every
    fixed-width scalar writes through its own carrier, whose range is already
    the field's. *)

val set_int32_le : bytes -> int -> SInt32.t -> unit
(** [set_int32_le buf off v] writes [v] as four signed little-endian bytes. *)

val set_int32_be : bytes -> int -> SInt32.t -> unit
(** [set_int32_be buf off v] writes [v] as four signed big-endian bytes. *)
