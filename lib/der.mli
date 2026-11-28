(** DER (X.690) below the ASN.1 types.

    Identifiers, lengths, the contents rules of the universal types, ANY, and
    the octets a writer emits, as everparse-asn1's specification states them. A
    check raises the parse error ({!Types.raise_parse_error}) of the first rule
    the bytes break; it never raises another exception on bytes within its
    bounds. *)

(** {1:errors Errors} *)

val rule_broken : at:int -> 'a
(** [rule_broken ~at] stops at a DER rule the bytes at [at] break. *)

val trailing : at:int -> int -> 'a
(** [trailing ~at n] stops at [n] bytes left over at [at]. *)

val eof : at:int -> expected:int -> int -> 'a
(** [eof ~at ~expected lim] stops at [expected] bytes needed at [at], before the
    limit [lim]. *)

val in_component : string -> (unit -> 'a) -> 'a
(** [in_component name f] is [f ()], its parse error named under [name]. *)

val cannot_write : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [cannot_write fmt] raises [Invalid_argument] for a value DER cannot write.
*)

(** {1:headers Identifiers and lengths} *)

type id = {
  id0 : int;  (** The first octet: class, constructed bit, short number. *)
  number : int64;  (** The tag number, below [2{^32}]. *)
  next : int;  (** Where the identifier ends. *)
}
(** The type for identifiers (X.690 8.1.2). *)

val identifier : bytes -> int -> int -> id
(** [identifier buf pos lim] is the identifier at [pos], which ends before
    [lim]: its number in the fewest octets. *)

val constructed_of : int -> bool
(** [constructed_of id0] is the constructed bit of the first octet [id0]. *)

type header = {
  hid : id;  (** The identifier. *)
  cstart : int;  (** Where the contents start. *)
  cend : int;  (** Where the contents end. *)
}
(** The type for identifiers and lengths. *)

val header : bytes -> int -> int -> header
(** [header buf pos lim] is the identifier and the definite, minimal length at
    [pos] (X.690 8.1.3, 10.1), whose contents end before [lim]. *)

val order_of : id -> int * int64
(** [order_of id] is the place of [id] in the canonical order of tags: its class
    bits, then its number. *)

val tag_lt : int * int64 -> int * int64 -> bool
(** [tag_lt a b] is [true] iff [a] comes before [b] in the canonical order of
    tags (X.690 8.6). *)

(** {1:contents Contents} *)

val byte : bytes -> int -> int
(** [byte buf i] is the octet at [i]. *)

val subidentifier : bytes -> int -> int -> int64 * int
(** [subidentifier buf p ce] is the base-128 subidentifier at [p] and where it
    ends, before [ce] (X.690 8.19.2). *)

val check_subidentifiers : bytes -> int -> int -> unit
(** [check_subidentifiers buf cs ce] checks that [cs]..[ce] is one or more
    subidentifiers: the contents of an OBJECT IDENTIFIER or a RELATIVE-OID. *)

val integer_ok : bytes -> int -> int -> bool
(** [integer_ok buf cs ce] is [true] iff [cs]..[ce] is an INTEGER in the fewest
    octets (X.690 8.3.2). *)

val minimal_integer : string -> bool
(** [minimal_integer s] is {!integer_ok} on the octets [s]. *)

val int64_of_content : bytes -> int -> int -> int64 option
(** [int64_of_content buf cs ce] is the INTEGER [cs]..[ce], or [None] when it
    takes more than 8 octets. *)

val integer_of_int64 : int64 -> string
(** [integer_of_int64 v] is the INTEGER contents of [v], in the fewest octets.
*)

val utf8_ok : string -> bool
(** [utf8_ok s] is [true] iff [s] is well-formed UTF-8 (RFC 3629). *)

val printable_char : char -> bool
(** [printable_char c] is [true] iff [c] is in the PrintableString alphabet. *)

val numeric_char : char -> bool
(** [numeric_char c] is [true] iff [c] is a digit or a space. *)

val visible_char : char -> bool
(** [visible_char c] is [true] iff [c] is a printable 7-bit character. *)

val ia5_char : char -> bool
(** [ia5_char c] is [true] iff [c] is a 7-bit character. *)

val bmp_ok : string -> bool
(** [bmp_ok s] is [true] iff [s] is UCS-2 code units, no surrogate. *)

val universal_ok : string -> bool
(** [universal_ok s] is [true] iff [s] is UCS-4 Unicode scalar values. *)

val utc_ok : string -> bool
(** [utc_ok s] is [true] iff [s] is a DER UTCTime, [YYMMDDhhmmssZ]. *)

val generalized_ok : string -> bool
(** [generalized_ok s] is [true] iff [s] is a DER GeneralizedTime,
    [YYYYMMDDhhmmss[.f]Z]. *)

val date_ok : string -> bool
(** [date_ok s] is [true] iff [s] is a DER DATE, [YYYYMMDD] from year 1582. *)

val time_of_day_ok : string -> bool
(** [time_of_day_ok s] is [true] iff [s] is a DER TIME-OF-DAY, [hhmmss]. *)

val date_time_ok : string -> bool
(** [date_time_ok s] is [true] iff [s] is a DER DATE-TIME, [YYYYMMDDhhmmss] from
    year 1582. *)

val oid_iri_ok : string -> bool
(** [oid_iri_ok s] is [true] iff [s] is an OID-IRI: UTF-8, [/] then labels
    separated by [/]. *)

val relative_oid_iri_ok : string -> bool
(** [relative_oid_iri_ok s] is [true] iff [s] is a RELATIVE-OID-IRI: UTF-8
    labels separated by [/]. *)

val any_end : bytes -> int -> int -> int
(** [any_end buf pos lim] checks the one DER value of any tag at [pos], before
    [lim], and is where it ends: ANY. *)

val lex_le : bytes -> int -> int -> int -> int -> bool
(** [lex_le buf s e s' e'] is [true] iff the octets [s]..[e] come no later than
    [s']..[e'] in lexicographic order, a prefix first (X.690 11.6). *)

val region_is : bytes -> int -> int -> string -> bool
(** [region_is buf s e str] is [true] iff the octets [s]..[e] are [str]. *)

(** {1:writing Writing} *)

val base128 : Buffer.t -> int64 -> unit
(** [base128 b n] adds [n] in base 128, in the fewest octets. *)

val tlv : cls:int -> constructed:bool -> number:int -> string -> string
(** [tlv ~cls ~constructed ~number content] is the value of class bits [cls],
    constructed bit [constructed] and tag number [number] with the contents
    [content]. *)
