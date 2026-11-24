(** The interfaces of the modules {!Wire_ocaml} generates.

    Each interface states the public API of its module and nothing else, with
    doc comments generated from the codec and, for the OxCaml target, the modes
    and allocation checks of the implementation. Internal to {!Wire_ocaml}. *)

val record :
  ?target:[ `OCaml | `OxCaml ] ->
  ?simd:bool ->
  ?make:string * string ->
  ?maps:(string * string * Planner.typ) list ->
  codec:string ->
  'a Wire.Codec.t ->
  string
(** [record ?target ?simd ?make ?maps ~codec c] is the interface of
    {!Record_module.ml}'s module for [c]. [make] is the constructor's path and
    the path of the type it returns; each of [maps] is a map's path, the OCaml
    type of its values and the map. Raises [Invalid_argument] as
    {!Record_module.ml} does. *)

val scanner :
  ?target:[ `OCaml | `OxCaml ] ->
  ?fields:string list ->
  ?step:string ->
  codec:string ->
  'a Wire.Codec.t ->
  string
(** [scanner ?target ?fields ?step ~codec c] is the interface of
    {!Scan_module.scanner}'s module for [c]. Raises [Invalid_argument] as
    {!Scan_module.scanner} does. *)

val bridge : maps:bool -> swap:bool -> codec:string -> core:string -> string
(** [bridge ~maps ~swap ~codec ~core] is the preamble of the interface of the
    bridge that checks [core] against the codec at [codec], and with [maps] the
    maps of its fields; with [swap] the bridge also defines the modules that
    swap with [core]. *)
