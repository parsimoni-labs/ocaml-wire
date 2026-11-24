(** The module generated for a record codec.

    The record type, its decoder, encoder, getters and views, on [bytes] and on
    a bigarray of bytes, and the bridge that checks the module against its
    codec. Internal to {!Wire_ocaml}. *)

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

(** Who defines a name: the generated module itself, or a field. *)
type source = Own | Field of string

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

val prepare :
  ?target:[ `OCaml | `OxCaml ] ->
  ?simd:bool ->
  ?make:string ->
  ?maps:(string * typ) list ->
  codec:string ->
  'a Wire.Codec.t ->
  plan * int
(** [prepare ?target ?simd ?make ?maps ~codec c] is the plan and the size of the
    module generated for [c]. Raises [Invalid_argument] as {!ml} does. *)

val ident : string -> string
(** [ident name] is the OCaml label of the field [name]. *)

val ml :
  ?target:[ `OCaml | `OxCaml ] ->
  ?simd:bool ->
  ?make:string ->
  ?maps:(string * typ) list ->
  codec:string ->
  'a Wire.Codec.t ->
  string
(** [ml ?target ?simd ?make ?maps ~codec c] is the module generated for [c]; see
    {!Wire_ocaml.ml}. *)

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
