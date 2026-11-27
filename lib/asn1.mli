(** ASN.1 DER descriptions.

    An ASN.1 type describes the DER encoding (X.690) of OCaml values. Its
    combinators follow asn1-combinators ([Asn.S]); a SEQUENCE or SET binds
    {!Field.t} components to a record with {!Codec.( $ )}, as {!Codec.v} does,
    and {!codec} gives the {!Codec.t} that decodes, validates, encodes and
    checks it. A DER value nests in a binary codec through {!Wire.codec}, and a
    binary codec nests in DER through {!containing}.

    Each ASN.1 type projects to an [.asn] module for everparse-asn1, and a shape
    everparse-asn1 cannot express is refused when it is built: the combinator
    raises [Invalid_argument] naming the shape and the reason. Bad bytes are a
    decode [Error], never an exception. See docs/asn1.md. *)

type 'a t
(** The type for ASN.1 types with OCaml values ['a]. *)

type cls = [ `Universal | `Application | `Private ]
(** The type for tag classes other than context-specific. *)

(** {1:oid Object identifiers} *)

(** Object identifiers, with arcs below [2{^32}]. *)
module Oid : sig
  type t
  (** The type for object identifiers. *)

  val base : int -> int -> t
  (** [base a b] is the object identifier [a.b]. Raises [Invalid_argument]
      unless [a] is [0], [1] or [2], and [b] is below [40] when [a] is not [2].
  *)

  val ( <| ) : t -> int -> t
  (** [o <| n] is [o] with the arc [n] appended. Raises [Invalid_argument] if
      [n] is negative or not below [2{^32}]. *)

  val ( <|| ) : t -> int list -> t
  (** [o <|| ns] is [o] with the arcs [ns] appended, as {!( <| )}. *)

  val equal : t -> t -> bool
  (** [equal o o'] is [true] iff [o] and [o'] have the same arcs. *)

  val compare : t -> t -> int
  (** [compare] orders object identifiers arc by arc. *)

  val of_string : string -> t option
  (** [of_string s] is the object identifier of the dotted string [s]. *)

  val to_string : t -> string
  (** [to_string o] is the dotted string of [o]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints an object identifier as {!to_string} does. *)
end

(** {1:prim Primitives} *)

val bool : bool t
(** [bool] is BOOLEAN: [0x00] or [0xFF]. *)

val integer : string t
(** [integer] is INTEGER as its minimal two's complement content octets. *)

val bounded_integer : min:int64 -> max:int64 -> int64 t
(** [bounded_integer ~min ~max] is [INTEGER (min..max)]. Raises
    [Invalid_argument] if [min > max]. *)

val enumerated : (string * int) list -> int t
(** [enumerated ns] is [ENUMERATED] with the named numbers [ns]. Raises
    [Invalid_argument] on an empty list, a repeated name or a repeated number.
*)

val null : unit t
(** [null] is NULL. *)

val oid : Oid.t t
(** [oid] is OBJECT IDENTIFIER. *)

val bit_string : (int * string) t
(** [bit_string] is BIT STRING as its count of unused bits, [0] to [7], and its
    octets, whose unused bits are zero. *)

val octet_string : string t
(** [octet_string] is OCTET STRING. *)

val utf8_string : string t
(** [utf8_string] is UTF8String: valid UTF-8. *)

val printable_string : string t
(** [printable_string] is PrintableString: its X.680 alphabet. *)

val ia5_string : string t
(** [ia5_string] is IA5String: 7-bit characters. *)

val numeric_string : string t
(** [numeric_string] is NumericString: digits and space. *)

val visible_string : string t
(** [visible_string] is VisibleString: printable 7-bit characters. *)

val teletex_string : string t
(** [teletex_string] is TeletexString, content unchecked. *)

val bmp_string : string t
(** [bmp_string] is BMPString as its UTF-16BE content octets. *)

val universal_string : string t
(** [universal_string] is UniversalString as its UTF-32BE content octets. *)

val utc_time : string t
(** [utc_time] is UTCTime as its characters, [YYMMDDhhmmssZ]. *)

val generalized_time : string t
(** [generalized_time] is GeneralizedTime as its characters,
    [YYYYMMDDhhmmss[.f]Z]. *)

val any : string t
(** [any] is ANY: one whole DER element, tag and length included. *)

(** {1:constraints Constraints} *)

val size : min:int -> max:int -> string t -> string t
(** [size ~min ~max t] is [t (SIZE (min..max))], counted in characters. Raises
    [Invalid_argument] unless [t] is an OCTET STRING or a character string, and
    [0 <= min <= max < 65536]. *)

(** {1:tags Tags} *)

val implicit : ?cls:cls -> int -> 'a t -> 'a t
(** [implicit ?cls n t] is [[cls n] IMPLICIT t]. [cls] defaults to
    context-specific. Raises [Invalid_argument] if [t] is a CHOICE or ANY, which
    have no tag of their own to replace. *)

val explicit : ?cls:cls -> int -> 'a t -> 'a t
(** [explicit ?cls n t] is [[cls n] EXPLICIT t]. [cls] defaults to
    context-specific. *)

(** {1:constructed Constructed types} *)

val required : string -> 'a t -> 'a Field.t
(** [required name t] is a component [name] of type [t]. *)

val optional : string -> 'a t -> 'a option Field.t
(** [optional name t] is a component [name] of type [t] marked OPTIONAL. *)

val default : string -> 'a -> 'a t -> 'a Field.t
(** [default name v t] is a component [name] of type [t] marked [DEFAULT v]:
    absent when it is [v]. Raises [Invalid_argument] unless [t] is a primitive
    other than OBJECT IDENTIFIER, possibly under one EXPLICIT tag. *)

val sequence : string -> 'f -> ('f, 'r) Codec.fields -> 'r t
(** [sequence name make fields] is the SEQUENCE [name] of [fields]. Raises
    [Invalid_argument] on a field not made by this module, a repeated name, and
    a run of OPTIONAL or DEFAULT components whose tags overlap each other or the
    next component's. *)

val set : string -> 'f -> ('f, 'r) Codec.fields -> 'r t
(** [set name make fields] is the SET [name] of [fields]. Raises
    [Invalid_argument] as {!sequence} does, on two components with one tag, and
    on more than 64 components. *)

val sequence_of : ?non_empty:bool -> 'a t -> 'a list t
(** [sequence_of ?non_empty t] is [SEQUENCE OF t], [SEQUENCE SIZE (1..MAX) OF t]
    if [non_empty] (defaults to [false]). *)

val set_of : ?non_empty:bool -> 'a t -> 'a list t
(** [set_of ?non_empty t] is [SET OF t], its elements in DER order. *)

type 'a alternative
(** The type for alternatives of a CHOICE of values ['a]. *)

val alternative :
  string ->
  'w t ->
  inject:('w -> 'a) ->
  project:('a -> 'w option) ->
  'a alternative
(** [alternative name t ~inject ~project] is the alternative [name] of type [t].
*)

val choice : 'a alternative list -> 'a t
(** [choice alts] is [CHOICE] over [alts]. Raises [Invalid_argument] on an empty
    list, two alternatives with one tag, and an ANY alternative. *)

(** {1:defined_by Components defined by a key} *)

type ('a, 'k) case
(** The type for the cases of a component of values ['a] defined by a key of
    values ['k]. *)

val case :
  'k -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> ('a, 'k) case
(** [case k t ~inject ~project] is the component of type [t] under the key [k].
*)

val absent : 'k -> inject:'a -> project:('a -> bool) -> ('a, 'k) case
(** [absent k ~inject ~project] is no component under the key [k]: it decodes as
    [inject], and encodes a value [project] accepts. *)

val fallback :
  ?optional:bool ->
  'w t ->
  inject:('k -> 'w option -> 'a) ->
  project:('a -> ('k * 'w option) option) ->
  ('a, 'k) case
(** [fallback ?optional t ~inject ~project] is the component of type [t] under
    every other key, OPTIONAL if [optional] (defaults to [false]). *)

val defined_by : string -> on:'k Field.t -> ('a, 'k) case list -> 'a Field.t
(** [defined_by name ~on cases] is the component [name] whose type the key [on]
    selects among [cases]; with no {!fallback}, another key is refused. Raises
    [Invalid_argument] unless [on] is an earlier mandatory component of the same
    SEQUENCE, of a primitive type with no constraint, and on a repeated key or a
    second {!fallback}. *)

(** {1:bytes Bytes} *)

val keep_bytes : 'a t -> ('a * string) t
(** [keep_bytes t] is [t] with the DER bytes of each decoded value; {!codec}'s
    encoder writes the value and ignores the bytes. *)

val containing : 'a Types.typ -> 'a t
(** [containing t] is an OCTET STRING whose content is exactly one value of [t].
*)

(** {1:values Values and codecs} *)

val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w t -> 'a t
(** [map ~decode ~encode t] is [t] read as ['a]. *)

val codec : 'a t -> 'a Codec.t
(** [codec t] is the codec of the DER encoding of [t]. A view of it reads each
    component through {!Codec.View.checked_field}, and sets none in place. *)

val fold :
  ('c, 'r) Codec.View.codec ->
  ('a list, 'r) Codec.field ->
  'b Buf.t ->
  ('b, 'c) Codec.View.t ->
  init:'acc ->
  f:('acc -> 'a -> 'acc) ->
  'acc
(** [fold c l b v ~init ~f] folds [f] over the elements of the SEQUENCE OF or
    SET OF component [l] of the view [v], in encoding order. An element of bytes
    written after the check that no longer holds ends the fold. Raises
    [Invalid_argument] unless [l] is a mandatory component of the SEQUENCE or
    SET [c] names whose type, under its tags, is a SEQUENCE OF or SET OF. *)

(** {1:projection Projection to everparse-asn1} *)

type schema
(** The type for ASN.1 modules, the input of everparse-asn1. *)

val project : string -> 'r Codec.t -> schema
(** [project m c] is the ASN.1 module [m] of [c]: [c]'s own types if [c] is an
    ASN.1 codec, else the ASN.1 types [c]'s fields hold. Raises
    [Invalid_argument] if [c] holds none. *)

val contained : 'r Codec.t -> Everparse.t list
(** [contained c] is the 3D schema of each binary codec under a {!containing} of
    [c]. *)

val name : schema -> string
(** [name s] is the module name of [s]. *)

val filename : schema -> string
(** [filename s] is the [.asn] file name of [s]. *)

val roots : schema -> string list
(** [roots s] is the types of [s] given to everparse-asn1's [--root]. *)

val pp : Format.formatter -> schema -> unit
(** [pp] prints a schema as its [.asn] text. *)

val write : outdir:string -> schema list -> unit
(** [write ~outdir ss] writes the [.asn] file of each schema in [outdir]. *)
