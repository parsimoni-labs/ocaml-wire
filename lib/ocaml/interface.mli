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
  ?views:bool ->
  ?buffer:[ `Bytes | `Bigstring ] ->
  Format.formatter ->
  target:target ->
  simd:bool ->
  make:(string * string) option ->
  maps:(string * string * Planner.typ) list ->
  codec:string ->
  plan:Planner.plan ->
  size:int ->
  unit
(** [record_members ?views ?buffer ppf ~target ~simd ~make ~maps ~codec ~plan
     ~size] states the members a fixed record adds: its size, record, decoders,
    writes, reads and views, on [buffer] (default [`Bytes]). With [views], the
    module's views read the fields on [bytes], and {!record_after_views} states
    what reads through them. *)

val record_after_views :
  Format.formatter ->
  target:target ->
  simd:bool ->
  make:(string * string) option ->
  maps:(string * string * Planner.typ) list ->
  codec:string ->
  plan:Planner.plan ->
  size:int ->
  unit
(** [record_after_views ppf ~target ~simd ~make ~maps ~codec ~plan ~size] states
    the reads of a word or of a 32-bit [int] of a view, the field handles and
    [Global], after the module's views. *)

val scan_preamble :
  Format.formatter -> codec:string -> plan:Planner.plan -> unit
(** [scan_preamble ppf ~codec ~plan] is the head of the interface of a module
    whose folds hand out outputs of [plan]. *)

val scan_members :
  Format.formatter ->
  target:target ->
  plan:Planner.plan ->
  folds:Scan_module.fold list ->
  unit
(** [scan_members ppf ~target ~plan ~folds] states each of [folds]. *)
