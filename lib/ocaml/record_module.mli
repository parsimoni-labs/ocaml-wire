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

val store : src:src -> Format.formatter -> field -> unit
(** [store ~src ppf f] writes the unchecked write of the field [f]. *)

val pp_wide : Format.formatter -> codec:string -> wide:bool -> unit
(** [pp_wide ppf ~codec ~wide] writes, when [wide], the check that makes the
    module refuse to load on a target whose ints have 32 bits or fewer. *)

val pp_header :
  Format.formatter -> codec:string -> name:string -> digest:string -> unit
(** [pp_header ppf ~codec ~name ~digest] writes the head of the generated
    module: what it links, and the [digest] that names the shape of its codec.
*)

val refuse : codec:string -> string -> 'a
(** [refuse ~codec reason] raises [Invalid_argument]: [codec] cannot be
    specialised, for [reason]. *)

(** Who defines a name: the generated module itself, or a field. *)
type source = Own | Field of string

val ident : string -> string
(** [ident name] is the OCaml label of the field [name]. *)

val ml :
  ?target:[ `OCaml | `OxCaml ] ->
  ?make:string ->
  ?maps:(string * typ) list ->
  codec:string ->
  'a Wire.Codec.t ->
  string
(** [ml ?target ?make ?maps ~codec c] is the module generated for [c]; see
    {!Wire_ocaml.ml}. *)

val bridge :
  ?maps:(string * typ) list ->
  codec:string ->
  core:string ->
  'a Wire.Codec.t ->
  string
(** [bridge ?maps ~codec ~core c] is the module that checks the generated [core]
    against [c]; see {!Wire_ocaml.bridge}. *)
