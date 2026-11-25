(** The views generated beside a scanner: a check that answers a view, an
    accessor per named field, and a module per record a field holds. Internal to
    {!Wire_ocaml}. *)

open Planner

(** A named member of a record's view. *)
type member =
  | Scalar of { name : string; field : field }  (** An integer field. *)
  | Present of { name : string; presence : output }
      (** The presence of an optional field that holds no record. *)
  | Sub of { name : string; sub : sub; presence : output option }
      (** A record a field holds, with its presence when optional. *)
  | Bytes of { name : string; label : string; at : bytes_at; slice : bool }
      (** A named field of bytes, its label, where its bytes are, and whether it
          is a slice rather than a string. *)

(** Where the bytes of a field are: at a constant offset in a prefix, or in a
    span of the plan. *)
and bytes_at = Fixed of field | Sized of node

type record = {
  id : int;  (** The id of its codec; [-1] for the root. *)
  codec_name : string;
  module_name : string option;  (** [None] for the root. *)
  struct_ : Wire.Private.Types.struct_;  (** Its codec's struct. *)
  plan : plan;  (** Its plan, with the records it holds kept apart. *)
  subs : sub list;
  fixed : int;
      (** The bytes at constant offsets a view of it is known to hold. *)
  fixed_size : int option;
  members : member list;
}
(** A record a scan reaches. *)

type t = { root : record; records : record list; size : int option }
(** The views of a codec: its root, and every record it holds, each once, those
    a record holds before it, and the codec's size when it is constant, which
    takes [check]'s window away. *)

val pp : t Fmt.t
(** [pp] prints the plan of the views: each record, the records it holds first,
    with the accessor of each of its members. *)

val module_of_codec : string -> string
(** [module_of_codec name] is the module of the records of the codec [name]:
    [CapturedIPv4] is [Captured_ipv4]. *)

val carrier : Planner.field -> (string * string) option
(** [carrier f] is the module of the type of [f]'s value and the proof that its
    values are [int]s, or [None] when that type is [int]. *)

val root_names : string list
(** [root_names] is the values the module of every codec defines at its top
    level. *)

val of_codec : ?reserved:string list -> 'r Wire.Codec.t -> t
(** [of_codec ?reserved c] is the views of [c]. An accessor takes its field's
    name, suffixed with ['_'] where that is a value of the module, one of
    {!root_names} or of [reserved], as a keyword is. Raises
    {!Planner.Unsupported} on a shape the scanner does not handle. *)

val ml : ?values:(record -> string) -> target:target -> t -> string * bool
(** [ml ?values ~target t] is the views of [t] but the root's [check], which the
    walks write, and whether they read [mask32]. They read [Int32_u] and
    [Int64_u] unbound. [values r] is written after the members of [r], in its
    module. *)

val slice_of : Wire.Private.Types.struct_ -> string -> bool option
(** [slice_of s label] is whether the named field of bytes [label] of [s] is a
    slice rather than a string, [None] when it is no such field. *)

val mli :
  ?values:(Format.formatter -> record -> unit) ->
  Format.formatter ->
  target:target ->
  t ->
  unit
(** [mli ?values ppf ~target t] prints the interface of the views of [t], with
    [values ppf r] after the members of [r]. *)

val member_name : member -> string
(** [member_name m] is the name of the accessor of [m]. *)
