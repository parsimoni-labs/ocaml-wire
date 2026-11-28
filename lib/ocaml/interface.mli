(** The interfaces of the modules {!Wire_ocaml} generates.

    Each interface states the public API of its module and nothing else, with
    doc comments generated from the codec and, for the OxCaml target, the modes
    and allocation checks of the implementation. {!Writer} puts the parts of one
    together. Internal to {!Wire_ocaml}. *)

type target = [ `OCaml | `OxCaml ]

val record_preamble :
  Format.formatter ->
  target:target ->
  simd:bool ->
  maps:(string * string * Planner.typ) list ->
  codec:string ->
  plan:Planner.plan ->
  size:int ->
  unit
(** [record_preamble ppf ~target ~simd ~maps ~codec ~plan ~size] is the head of
    the interface of the module of a fixed record of [size] bytes: what it is
    and its fields. Each of [maps] is a map's path, the OCaml type of its values
    and the map. *)

val record_members :
  ?accessor:(Planner.field -> string) ->
  Format.formatter ->
  target:target ->
  simd:bool ->
  maps:(string * string * Planner.typ) list ->
  codec:string ->
  plan:Planner.plan ->
  size:int ->
  unit
(** [record_members ?accessor ppf ~target ~simd ~maps ~codec ~plan ~size] states
    the members a fixed record adds: its size, record, decoders, writes, and,
    with [accessor], the views of a module with no walk, [accessor f] the
    accessor of each field [f]. *)

val scan_preamble :
  Format.formatter -> codec:string -> plan:Planner.plan -> unit
(** [scan_preamble ppf ~codec ~plan] is the head of the interface of a module
    whose walk covers its codec. *)

val pp_fold :
  Format.formatter ->
  target:target ->
  paths:string list ->
  types:string list ->
  unit
(** [pp_fold ppf ~target ~paths ~types] states [fold], whose [f] takes the
    fields [paths], of the OCaml types [types]. *)
