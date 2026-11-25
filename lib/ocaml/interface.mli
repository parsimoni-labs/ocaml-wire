(** The interfaces of the modules {!Wire_ocaml} generates.

    Each interface states the public API of its module and nothing else, with
    doc comments generated from the codec and, for the OxCaml target, the modes
    and allocation checks of the implementation. {!Writer} puts the parts of one
    together. Internal to {!Wire_ocaml}. *)

type target = [ `OCaml | `OxCaml ]

val pp_digest : target -> Format.formatter -> unit
(** [pp_digest target ppf] states the module's [digest]. *)

val record_preamble :
  Format.formatter ->
  target:target ->
  simd:bool ->
  make:(string * string) option ->
  maps:(string * string * Planner.typ) list ->
  codec:string ->
  plan:Planner.plan ->
  size:int ->
  unit
(** [record_preamble ppf ~target ~simd ~make ~maps ~codec ~plan ~size] is the
    head of the interface of the module of a fixed record of [size] bytes: what
    it is and its fields. [make] is the constructor's path and the path of the
    type it returns; each of [maps] is a map's path, the OCaml type of its
    values and the map. *)

val record_members :
  Format.formatter ->
  target:target ->
  simd:bool ->
  make:(string * string) option ->
  maps:(string * string * Planner.typ) list ->
  codec:string ->
  plan:Planner.plan ->
  size:int ->
  unit
(** [record_members ppf ~target ~simd ~make ~maps ~codec ~plan ~size] states the
    members a fixed record adds: its size, record, decoders, writes, reads and
    views, on [bytes] and on a bigarray. *)

val scan_preamble :
  Format.formatter ->
  codec:string ->
  plan:Planner.plan ->
  selected:Planner.output list ->
  unit
(** [scan_preamble ppf ~codec ~plan ~selected] is the head of the interface of a
    module whose folds hand out [selected]. *)

val scan_members :
  Format.formatter ->
  target:target ->
  selected:Planner.output list ->
  step:string option ->
  unit
(** [scan_members ppf ~target ~selected ~step] states the folds, with the step
    folds of the module [step]. *)

val bridge : maps:bool -> swap:bool -> codec:string -> core:string -> string
(** [bridge ~maps ~swap ~codec ~core] is the preamble of the interface of the
    bridge that checks [core] against the codec at [codec], and with [maps] the
    maps of its fields; with [swap] the bridge also defines the modules that
    swap with [core]. *)
