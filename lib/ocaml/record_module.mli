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
  digest:string ->
  uses:(string -> bool) ->
  unit
(** [pp_header ppf ~codec ~name ~digest ~uses] writes the head of the generated
    module: what it links, the [digest] that names the shape of its codec, and
    the helpers among [codec_name], [cast] and [no_error] the body [uses]. *)

val refuse : codec:string -> string -> 'a
(** [refuse ~codec reason] raises [Invalid_argument]: [codec] cannot be
    specialised, for [reason]. *)

val all_fields : plan -> field list
(** [all_fields plan] is the fields of the record [plan] reads, in wire order.
*)

val has_checks : plan -> bool
(** [has_checks plan] is [true] iff a field of [plan] or its where clause has a
    check. *)

val has_carrier : field -> bool
(** [has_carrier f] is [true] iff [f] holds its value in a carrier of its own,
    [Value.<field>]. *)

val type_of : field -> string
(** [type_of f] is the OCaml type of [f]'s typed value, for a field with no map.
*)

val schema_type : field -> string
(** [schema_type f] is the OCaml type a handle of [f] reads, the codec's own:
    [int] or [string] where {!type_of} is the field's carrier. *)

val field_fields : field list -> field list
(** [field_fields fields] is the fields among [fields] that have a handle in the
    generated module. *)

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
  Format.formatter ->
  target:[ `OCaml | `OxCaml ] ->
  make:string option ->
  plan:plan ->
  size:int ->
  unit
(** [pp_body ?simd ppf ~target ~make ~plan ~size] writes the record's members
    for a record of [size] bytes; see {!Wire_ocaml.ml}. *)

val ident : string -> string
(** [ident name] is the OCaml label of the field [name]. *)

val bridge :
  ?maps:(string * typ) list ->
  codec:string ->
  core:string ->
  'a Wire.Codec.t ->
  string
(** [bridge ?maps ~codec ~core c] is the module that checks the generated [core]
    against [c]; see {!Wire_ocaml.bridge}. *)

val handles :
  ?maps:(string * Planner.typ) list ->
  codec:string ->
  'r Wire.Codec.t ->
  field list
(** [handles ?maps ~codec c] is the fields of [c] with a handle. Raises
    [Invalid_argument] as {!bridge} does. *)
