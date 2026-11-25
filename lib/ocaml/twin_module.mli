(** The interpreted twin of a generated module: a functor over the codec value
    that reads it, through [Wire.Codec.View] and [Wire.Codec.validate], as the
    generated module reads its bytes. Internal to {!Wire_ocaml}. *)

val ml :
  codec:string ->
  digest:string ->
  views:View_module.t option ->
  folds:Scan_module.fold list ->
  string
(** [ml ~codec ~digest ~views ~folds] is the source of the twin of the module of
    the codec named [codec] whose shape has [digest], with [views] and [folds].
    Raises [Invalid_argument] for a field the twin cannot place. *)

val mli :
  codec:string ->
  plan:Planner.plan ->
  views:View_module.t option ->
  folds:Scan_module.fold list ->
  string
(** [mli ~codec ~plan ~views ~folds] is the interface of the twin {!val-ml}
    writes, that of the generated module for the [`OCaml] target restricted to
    the views, [min_size], [validate] and the folds. *)
