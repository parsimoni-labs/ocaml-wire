(** The members of the module generated for a fixed record of named scalars.

    The record type, its decoder, encoder, getters and views, on [bytes] and on
    a bigarray of bytes, which {!Writer} writes into the module of such a codec,
    and the bridge that checks the module against its codec. Internal to
    {!Wire_ocaml}. *)

open Planner
open Emitter

val cold : src:src -> live:Sset.t -> mode
(** [cold ~src ~live] is the mode of the walk that finds the codec's error, off
    the hot path. *)

val zero_alloc : [< `OCaml | `OxCaml ] -> string
(** [zero_alloc target] is the attribute the OxCaml target adds to a hot path:
    an allocation check. *)

val uses : string -> string -> bool
(** [uses text name] is [true] iff the identifier [name] occurs in [text]. *)

val pp_wide :
  Format.formatter -> codec:string -> wide:bool -> uses:(string -> bool) -> unit
(** [pp_wide ppf ~codec ~wide ~uses] writes, when [wide], the check that makes
    the module refuse to load on a target whose ints have 32 bits or fewer, and
    [mask32] if [uses] it. *)

val pp_header :
  Format.formatter ->
  codec:string ->
  name:string ->
  uses:(string -> bool) ->
  unit
(** [pp_header ppf ~codec ~name ~uses] writes the head of the generated module:
    what it links and the helpers among [codec_name], [cast] and [no_error] the
    body [uses]. *)

val refuse : codec:string -> string -> 'a
(** [refuse ~codec reason] raises [Invalid_argument]: [codec] cannot be
    specialised, for [reason]. *)

val all_fields : plan -> field list
(** [all_fields plan] is the fields of the record [plan] reads, in wire order.
*)

val has_checks : plan -> bool
(** [has_checks plan] is [true] iff a field of [plan] or its where clause has a
    check. *)

val type_of : field -> string
(** [type_of f] is the OCaml type of [f]'s value, for a field with no map: its
    carrier for an integer, [int] for a bitfield and [string] for a byte array.
*)

val lanes : plan -> size:int -> (field * (int * int)) list option
(** [lanes plan ~size] is each field with its shift and width in a SIMD lane, or
    [None] when the record has no lanes. *)

val record_plan : maps:(string * typ) list -> 'a Wire.Codec.t -> plan * int
(** [record_plan ~maps c] is the plan of [c] and its size, when [c] is a record
    of named scalars, each at a constant offset. Raises {!Planner.Unsupported}
    otherwise. *)

val own_names : string list
(** [own_names] is the names the record's members define, at top level or in
    [Bigstring]. *)

val field_names : field list -> string list * (string * string list) list
(** [field_names fields] is the names [Get]'s reads call, and each field's name
    with the names it defines, those in [Get] qualified. *)

val pp_maps : Format.formatter -> field list -> unit
(** [pp_maps ppf fields] binds each map of [fields] once. *)

val pp_body :
  ?simd:bool ->
  ?accessor:(field -> string) ->
  ?buffer:[ `Bytes | `Bigstring ] ->
  maps:(string * string) list ->
  Format.formatter ->
  target:[ `OCaml | `OxCaml ] ->
  plan:plan ->
  size:int ->
  unit
(** [pp_body ?simd ?accessor ?buffer ~maps ppf ~target ~plan ~size] writes the
    record's members for a record of [size] bytes, reading [buffer] (default
    [`Bytes]); see {!Wire_ocaml.v}. A field under a map holds the OCaml type
    [maps] gives its path. With [accessor], the name of each field's accessor in
    the module's views, it writes none of [Get], [Valid] and [read_<field>],
    [View] and [view_<field>]: the views read the fields. *)

val ident : string -> string
(** [ident name] is the OCaml label of the field [name]. *)
