(** The CDDL modules evercddl reads (docs/cbor.md, Constructs and their
    projections).

    {!typ} is CDDL as [Cbor] projects to it: [Cbor] translates each of its types
    to one, and this module names the rules and prints them. *)

type literal = [ `Int of int | `Text of string ]
(** The type for CDDL literals. *)

(** The type for CDDL types. *)
type typ =
  | Uint  (** [uint] *)
  | Nint  (** [nint] *)
  | Int  (** [int] *)
  | Bytes  (** [bstr] *)
  | Text  (** [tstr] *)
  | Bool  (** [bool] *)
  | Null  (** [nil] *)
  | Any  (** [any] *)
  | Literal of literal  (** the literal value *)
  | Range of int64 * int64  (** [min..max] *)
  | Size of { min : int; max : int; inner : typ }  (** [t .size (min..max)] *)
  | Tag of int * typ  (** [#6.n(t)] *)
  | Array of { non_empty : bool; elem : typ }  (** [[* t]] or [[+ t]] *)
  | Tuple of { name : string; entries : entry list }
      (** the rule [name = [ ... ]] *)
  | Assoc of typ * typ  (** [{ * k => v }] *)
  | Record of { name : string; members : member list }
      (** the rule [name = { ... }] *)
  | Choice of (string * typ) list  (** [t1 / t2 / ...], each case named *)
  | Nullable of typ  (** [t / nil] *)
  | Det_cbor of typ  (** [bstr .everparse-det-cbor t] *)
  | Containing of binary  (** [bstr] holding a binary value *)

and entry = { name : string; optional : bool; typ : typ }
(** The type for array entries: [name : t], [? name : t] if [optional]. *)

(** The type for map members. *)
and member =
  | Mem of { name : string; key : literal; optional : bool; typ : typ }
      (** [k => t], [? k => t] if [optional] *)
  | Exclusive of { name : string; members : (literal * typ) list }
      (** at most one of [k => t] *)
  | Keep_unknown of { name : string; key : typ; value : typ }  (** [* k => v] *)

(** The type for the binary types under a {!constructor-Containing}. *)
and binary = Binary : 'a Types.typ -> binary

val pp_typ : Format.formatter -> typ -> unit
(** [pp_typ] prints a type as the CDDL of a rule's right-hand side, naming no
    rule: a tuple or a record is its name. *)

val held_in :
  whole:(string -> Types.struct_ -> (string * typ) list option) ->
  string ->
  'a Types.typ ->
  (string * typ) list
(** [held_in ~whole path t] is the CDDL types the binary type [t] holds, each
    with the name it takes if it has none of its own: [whole p s] for a codec of
    struct [s] at path [p] that is not binary, and those the fields of a binary
    codec hold, named by their path. *)

val held :
  whole:(string -> Types.struct_ -> (string * typ) list option) ->
  base:string ->
  Types.struct_ ->
  (string * typ) list
(** [held ~whole ~base s] is [held_in ~whole base] of the codec of struct [s].
*)

type schema
(** The type for CDDL modules. *)

val project : string -> (string * typ) list -> schema
(** [project m ts] is the module [m] of the types [ts], each a rule: a tuple or
    a record under its own name, any other type under the name it comes with.
    Raises [Invalid_argument] if [m] is not a module name evercddl takes. *)

val contained : (string * typ) list -> Everparse.t list
(** [contained ts] is the 3D schema of each binary type under a
    {!constructor-Containing} of [ts], once each. *)

val name : schema -> string
(** [name s] is the module name of [s]. *)

val filename : schema -> string
(** [filename s] is the [.cddl] file name of [s]. *)

val pp : Format.formatter -> schema -> unit
(** [pp] prints a schema as its [.cddl] text. *)

val write : outdir:string -> schema list -> unit
(** [write ~outdir ss] writes the [.cddl] file of each schema in [outdir].
    Raises [Invalid_argument] if two different schemas have one name. *)
