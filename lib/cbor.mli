(** Deterministic CBOR descriptions.

    {b Experimental.} This module is an experimental part of 2.0.0: its API will
    certainly break in 2.1.0.

    A CBOR type describes the deterministic encoding (RFC 8949 section 4.2.1) of
    OCaml values, as a CDDL rule (RFC 8610) does. Its combinators follow
    ocaml-cbor's [Cbor.Codec] and the CDDL prelude; a map record or an array
    record binds {!Field.t} members to a record with {!Codec.( $ )}, as
    {!Codec.v} does, and {!codec} gives the {!Codec.t} that decodes, validates
    and encodes it. A CBOR value nests in a binary or DER codec through
    {!Wire.codec} under {!Wire.nested}, and a binary codec nests in CBOR through
    {!containing}.

    Each CBOR type projects to a [.cddl] rule for evercddl, EverParse's CDDL
    front end, which accepts deterministic CBOR only. A shape evercddl cannot
    express is refused when it is built: the combinator raises
    [Invalid_argument] naming the shape and the reason. Bad bytes, including a
    valid but non-deterministic encoding, are a decode [Error], never an
    exception. See docs/cbor.md. *)

type 'a t
(** The type for CBOR types with OCaml values ['a]. *)

(** The type for CBOR integers, from [-2{^64}] to [2{^64}-1]: the major type and
    the argument of the head. *)
type integer =
  | Uint of UInt64.t  (** [Uint n] is the unsigned integer [n]. *)
  | Nint of UInt64.t  (** [Nint n] is the negative integer [-1 - n]. *)

type literal = [ `Int of int | `Text of string ]
(** The type for CDDL literals, the values of map keys and of {!val-literal}
    types. A text literal is ASCII and shorter than [2{^16}] bytes. *)

(** {1:prim Primitives} *)

val uint : UInt64.t t
(** [uint] is [uint], major type 0. *)

val nint : UInt64.t t
(** [nint] is [nint], major type 1: the value [n] stands for [-1 - n]. *)

val int : integer t
(** [int] is [int], [uint / nint]. *)

val range : min:int64 -> max:int64 -> int64 t
(** [range ~min ~max] is the integer range [min..max]. Raises [Invalid_argument]
    if [min > max]. *)

val bytes : string t
(** [bytes] is [bstr], major type 2. *)

val text : string t
(** [text] is [tstr], major type 3: valid UTF-8. *)

val bool : bool t
(** [bool] is [bool], the simple values [20] and [21]. *)

val null : unit t
(** [null] is [nil], the simple value [22]. *)

val literal : literal -> unit t
(** [literal v] is the CDDL literal type [v]. Raises [Invalid_argument] on a
    text literal that is not ASCII or not shorter than [2{^16}] bytes. *)

val any : string t
(** [any] is [any]: the bytes of one whole data item, head included. *)

(** {1:constraints Constraints} *)

val size : min:int -> max:int -> string t -> string t
(** [size ~min ~max t] is [t .size (min..max)], counted in bytes. Raises
    [Invalid_argument] unless [t] is {!bytes} or {!text}, and [0 <= min <= max].
*)

(** {1:tags Tags} *)

val tag : int -> 'a t -> 'a t
(** [tag n t] is [#6.n(t)]. Raises [Invalid_argument] if [n] is negative. *)

(** {1:arrays Arrays} *)

val array : ?non_empty:bool -> 'a t -> 'a list t
(** [array ?non_empty t] is [[* t]], [[+ t]] if [non_empty] (defaults to
    [false]). *)

val elem : string -> 'a t -> 'a Field.t
(** [elem name t] is the array entry [name: t]. *)

val elem_opt : string -> 'a t -> 'a option Field.t
(** [elem_opt name t] is the array entry [? name: t]. *)

val tuple : string -> 'f -> ('f, 'r, 'r) Codec.Defs.t -> 'r t
(** [tuple name make fields] is the rule [name = [ fields ]] of the {!elem} and
    {!elem_opt} entries [fields], in order. Raises [Invalid_argument] on no
    entry, a field not made by {!elem} or {!elem_opt}, a repeated name, and an
    optional entry that can also start the entries after it, which evercddl
    cannot split one way. *)

(** {1:maps Maps} *)

val assoc : 'k t -> 'v t -> ('k * 'v) list t
(** [assoc k v] is [{ * k => v }], its entries in deterministic key order. *)

val mem : string -> literal -> 'a t -> 'a Field.t
(** [mem name k t] is the member [name] under the key [k], of type [t]. Raises
    [Invalid_argument] on a text key as {!val-literal} does. *)

val mem_opt : string -> literal -> 'a t -> 'a option Field.t
(** [mem_opt name k t] is the member [name] under the key [k], of type [t],
    marked [?]. Raises [Invalid_argument] on a text key as {!val-literal} does.
*)

type 'a member
(** The type for members of an {!exclusive} group of values ['a]. *)

val member :
  literal -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a member
(** [member k t ~inject ~project] is the member under the key [k], of type [t].
    Raises [Invalid_argument] on a text key as {!val-literal} does. *)

val exclusive : string -> 'a member list -> 'a option Field.t
(** [exclusive name ms] is the members [ms] of which at most one is present.
    Raises [Invalid_argument] on fewer than two members and a repeated key. *)

val keep_unknown : string -> 'k t -> 'v t -> ('k * 'v) list Field.t
(** [keep_unknown name k v] is [* k => v]: the entries no other member of the
    record matches, in deterministic key order. *)

val record : string -> 'f -> ('f, 'r, 'r) Codec.Defs.t -> 'r t
(** [record name make fields] is the rule [name = { fields }] of the members
    [fields]. A key no member matches is refused unless a {!keep_unknown} member
    takes it. Raises [Invalid_argument] on a field not made by {!mem},
    {!mem_opt}, {!exclusive} or {!keep_unknown}, a repeated name, two members
    with one key, and more than one {!keep_unknown}. *)

(** {1:choices Choices} *)

type 'a case
(** The type for the alternatives of a choice of values ['a]. *)

val case :
  string -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a case
(** [case name t ~inject ~project] is the alternative [name] of type [t]. *)

val choice : 'a case list -> 'a t
(** [choice cs] is [c1 / c2 / ...] over [cs]; a value matches at most one.
    Raises [Invalid_argument] on an empty list, a repeated case name, and two
    cases whose types meet as evercddl decides it: two maps ({!record} or
    {!assoc}) always meet, and {!any} meets every type. *)

val nullable : 'a t -> 'a option t
(** [nullable t] is [t / nil]. Raises [Invalid_argument] if [t] matches [nil].
*)

(** {1:bytes Bytes} *)

val encoded : 'a t -> 'a t
(** [encoded t] is [bstr .cbor t]: a byte string whose content is exactly one
    deterministic encoding of a value of [t]. *)

val containing : 'a Types.typ -> 'a t
(** [containing t] is a [bstr] whose content is exactly one value of the binary
    type [t]. Raises [Invalid_argument] if [t] is a DER or a CBOR codec, which
    no backend would check there: CBOR content is {!encoded}'s. *)

val keep_bytes : 'a t -> ('a * string) t
(** [keep_bytes t] is [t] with the bytes of each decoded value; {!codec}'s
    encoder writes the value and ignores the bytes. *)

(** {1:values Values and codecs} *)

val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w t -> 'a t
(** [map ~decode ~encode t] is [t] read as ['a]. *)

val codec : 'a t -> 'a Codec.t
(** [codec t] is the codec of the deterministic CBOR encoding of [t]. A view of
    it reads each member through {!Codec.View.field}, and sets none in place: a
    CBOR head moves every later offset. *)

(** {1:projection Projection to evercddl} *)

type schema
(** The type for CDDL modules, the input of evercddl. *)

val project : string -> 'r Codec.t -> schema
(** [project m c] is the CDDL module [m] of [c]: [c]'s own rules if [c] is a
    CBOR codec, else the CBOR rules [c]'s fields hold, and those under the
    containings of a DER codec. Raises [Invalid_argument] if [c] holds none. *)

val contained : 'r Codec.t -> Everparse.t list
(** [contained c] is the 3D schema of each binary codec under a {!containing} of
    [c]. *)

val name : schema -> string
(** [name s] is the module name of [s], evercddl's [--mname]. *)

val filename : schema -> string
(** [filename s] is the [.cddl] file name of [s]. *)

val pp : Format.formatter -> schema -> unit
(** [pp] prints a schema as its [.cddl] text. *)

val write : outdir:string -> schema list -> unit
(** [write ~outdir ss] writes the [.cddl] file of each schema in [outdir]. *)
