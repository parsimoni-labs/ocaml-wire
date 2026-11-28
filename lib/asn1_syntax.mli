(** The syntax of ASN.1 types.

    {!t} is the type the DER codec and the [.asn] printer of [Asn1] are
    functions over; its combinators refuse, with [Invalid_argument], a shape
    everparse-asn1 does not express. [Asn1] is its public face, and documents
    each combinator. *)

type cls = [ `Universal | `Application | `Private ]
(** The type for tag classes other than context-specific. *)

(** Object identifiers, with arcs below [2{^32}]. *)
module Oid : sig
  type t = UInt32.t array
  (** The type for object identifiers: their arcs, at least two. *)

  val arc : int64 -> UInt32.t
  (** [arc n] is the arc [n], below [2{^32}]. *)

  val int64_of_arc : UInt32.t -> int64
  (** [int64_of_arc a] is the arc [a] as a non-negative number. *)

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

(** Relative object identifiers, with arcs below [2{^32}]. *)
module Relative_oid : sig
  type t = UInt32.t array
  (** The type for relative object identifiers: their arcs, at least one. *)

  val v : int list -> t
  (** [v ns] is the relative object identifier of the arcs [ns]. Raises
      [Invalid_argument] if [ns] is empty, or an arc is negative or not below
      [2{^32}]. *)

  val equal : t -> t -> bool
  (** [equal r r'] is [true] iff [r] and [r'] have the same arcs. *)

  val compare : t -> t -> int
  (** [compare] orders relative object identifiers arc by arc. *)

  val of_string : string -> t option
  (** [of_string s] is the relative object identifier of the dotted string [s].
  *)

  val to_string : t -> string
  (** [to_string r] is the dotted string of [r]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints a relative object identifier as {!to_string} does. *)
end

(** {1:tags Tags} *)

(** The type for tag classes. *)
type tag_class =
  | Universal  (** UNIVERSAL. *)
  | Application  (** APPLICATION. *)
  | Context  (** Context-specific. *)
  | Private  (** PRIVATE. *)

type tag = { cls : tag_class; constructed : bool; number : int }
(** The type for identifiers: a class, a form and a number. *)

val equal_class : tag_class -> tag_class -> bool
(** [equal_class a b] is [true] iff [a] and [b] are one class. *)

val universal : ?constructed:bool -> int -> tag
(** [universal ?constructed n] is the UNIVERSAL tag [n], primitive unless
    [constructed]. *)

val pp_tag : Format.formatter -> tag -> unit
(** [pp_tag] prints a tag as [.asn] writes it, [[APPLICATION 3]]. *)

(** {1:types Types} *)

type chars =
  | Utf8
  | Printable
  | Ia5
  | Numeric
  | Visible
  | Teletex
  | Videotex
  | Graphic
  | General
  | Object_descriptor
  | Bmp
  | Universal_chars

type time = Utc | Generalized | Date | Time_of_day | Date_time
type iri = Oid_iri | Relative_oid_iri

type _ prim =
  | Bool : bool prim
  | Integer : string prim
  | Bounded_integer : { min : int64; max : int64 } -> int64 prim
  | Enumerated : (string * int) list -> int prim
  | Null : unit prim
  | Oid : Oid.t prim
  | Relative_oid : Relative_oid.t prim
  | Bit_string : (int * string) prim
  | Octet_string : string prim
  | Chars : chars -> string prim
  | Time : time -> string prim
  | Iri : iri -> string prim

type _ t =
  | Prim : 'a prim -> 'a t
  | Any : string t
  | Size : { min : int; max : int; inner : string t } -> string t
  | Implicit : { tag : tag; inner : 'a t } -> 'a t
  | Explicit : { tag : tag; inner : 'a t } -> 'a t
  | Sequence : 'r record -> 'r t
  | Set : 'r record -> 'r t
  | Sequence_of : { non_empty : bool; elem : 'a t } -> 'a list t
  | Set_of : { non_empty : bool; elem : 'a t } -> 'a list t
  | Choice : 'a alternative list -> 'a t
  | Keep_bytes : 'a t -> ('a * string) t
  | Containing : 'a Types.typ -> 'a t
  | Map : { inner : 'w t; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a t

(** A SEQUENCE or SET: [fields] builds the record with [make], [components] are
    the same fields as this module reads them, in order. *)
and 'r record =
  | Record : {
      name : string;
      make : 'f;
      fields : ('f, 'r) Codec.fields;
      components : component list;
    }
      -> 'r record

and 'a alternative =
  | Alternative : {
      name : string;
      typ : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> 'a alternative

(** A component of a SEQUENCE or SET, as {!val-required}, {!val-optional},
    {!val-default} and {!val-defined_by} make it. The key of a {!val-defined_by}
    component is the field whose value selects the case, known by its
    [Field.id]. *)
and _ decorated =
  | Required : 'a t -> 'a decorated
  | Optional : 'a t -> 'a option decorated
  | Default : { value : 'a; typ : 'a t } -> 'a decorated
  | Defined_by : {
      key_name : string;
      key_id : int;
      key_typ : 'k t;
      cases : ('a, 'k) case list;
    }
      -> 'a decorated

and ('a, 'k) case =
  | Case : {
      key : 'k;
      typ : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> ('a, 'k) case
  | Absent : { key : 'k; inject : 'a; project : 'a -> bool } -> ('a, 'k) case
  | Fallback : {
      optional : bool;
      typ : 'w t;
      inject : 'k -> 'w option -> 'a;
      project : 'a -> ('k * 'w option) option;
    }
      -> ('a, 'k) case

and component =
  | Component : {
      name : string;
      id : int;
      decorated : 'a decorated;
    }
      -> component

type _ Types.der +=
  | Component_of : 'a decorated -> 'a Types.der
        (** The typ of the [Field.t] of a component. *)

(** The type for the tags a value of a type can start with. *)
type tags =
  | All  (** ANY's: every tag. *)
  | Tags of tag list  (** The tags a value can start with. *)

val prim_number : 'a prim -> int
(** [prim_number p] is the UNIVERSAL tag number of [p]. *)

val union : tags -> tags -> tags
(** [union a b] is the tags of [a] and of [b]. *)

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

val relative_oid : Relative_oid.t t
(** [relative_oid] is RELATIVE-OID. *)

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

val videotex_string : string t
(** [videotex_string] is VideotexString, content unchecked. *)

val graphic_string : string t
(** [graphic_string] is GraphicString, content unchecked. *)

val general_string : string t
(** [general_string] is GeneralString, content unchecked. *)

val object_descriptor : string t
(** [object_descriptor] is ObjectDescriptor, content unchecked. *)

val bmp_string : string t
(** [bmp_string] is BMPString as its UTF-16BE content octets. *)

val universal_string : string t
(** [universal_string] is UniversalString as its UTF-32BE content octets. *)

val utc_time : string t
(** [utc_time] is UTCTime as its characters, [YYMMDDhhmmssZ]. *)

val generalized_time : string t
(** [generalized_time] is GeneralizedTime as its characters,
    [YYYYMMDDhhmmss[.f]Z]. *)

val date : string t
(** [date] is DATE as its characters, [YYYYMMDD], the year from 1582. *)

val time_of_day : string t
(** [time_of_day] is TIME-OF-DAY as its characters, [hhmmss]. *)

val date_time : string t
(** [date_time] is DATE-TIME as its characters, [YYYYMMDDhhmmss], the year from
    1582. *)

val oid_iri : string t
(** [oid_iri] is OID-IRI as its UTF-8 characters, [/] then labels separated by
    [/]. *)

val relative_oid_iri : string t
(** [relative_oid_iri] is RELATIVE-OID-IRI as its UTF-8 characters, labels
    separated by [/]. *)

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
    other than OBJECT IDENTIFIER and RELATIVE-OID, possibly under one EXPLICIT
    tag, and on a time or IRI [v] that is not a DER value of [t]. *)

val sequence : string -> 'f -> ('f, 'r, 'r) Codec.Defs.t -> 'r t
(** [sequence name make fields] is the SEQUENCE [name] of [fields]. Raises
    [Invalid_argument] on a field not made by this module, a repeated name, and
    a run of OPTIONAL or DEFAULT components whose tags overlap each other or the
    next component's. *)

val set : string -> 'f -> ('f, 'r, 'r) Codec.Defs.t -> 'r t
(** [set name make fields] is the SET [name] of [fields]. Raises
    [Invalid_argument] as {!sequence} does, on two components with one tag, and
    on more than 64 components. *)

val sequence_of : ?non_empty:bool -> 'a t -> 'a list t
(** [sequence_of ?non_empty t] is [SEQUENCE OF t], [SEQUENCE SIZE (1..MAX) OF t]
    if [non_empty] (defaults to [false]). *)

val set_of : ?non_empty:bool -> 'a t -> 'a list t
(** [set_of ?non_empty t] is [SET OF t], its elements in DER order. *)

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

(** {1:asn Names and literals of the [.asn] text} *)

val ident : upper:bool -> string -> string
(** [ident ~upper s] is [s] made an ASN.1 name (X.680 12.2-12.4): a type or
    module name if [upper], else a value name. *)

val fresh : taken:(string -> bool) -> string -> string
(** [fresh ~taken base] is [base] if [taken] does not hold it, else the first
    free [base-2], [base-3], ... *)

val unique_idents : string list -> string list
(** [unique_idents names] is [names], in order, each made an ASN.1 value name
    and unique. *)

val enum_names : (string * int) list -> (string * int) list
(** [enum_names ns] is [ns] with each name made an ASN.1 name, unique. *)

val in_size : min:int -> max:int -> string t -> string -> bool
(** [in_size ~min ~max t v] is [true] iff [v], a value of [t], has [min] to
    [max] characters as SIZE counts them. *)

val time_ok : time -> string -> bool
(** [time_ok k s] is [true] iff [s] is the DER contents of a time of kind [k].
*)

val time_keyword : time -> string
(** [time_keyword k] is the [.asn] keyword of [k]. *)

val iri_ok : iri -> string -> bool
(** [iri_ok k s] is [true] iff [s] is the DER contents of an IRI of kind [k]. *)

val iri_keyword : iri -> string
(** [iri_keyword k] is the [.asn] keyword of [k]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp] prints a type in [.asn] notation, a SEQUENCE or SET by its name. *)

val literal : oid:(Oid.t -> string) -> 'a t -> 'a -> (string, string) result
(** [literal ~oid t v] is the [.asn] literal of the value [v] of [t], or why it
    has none. [oid] names an OBJECT IDENTIFIER value. *)

val oid_value : Oid.t -> string
(** [oid_value o] is the [.asn] value of [o], [{ 1 2 840 }]. *)
