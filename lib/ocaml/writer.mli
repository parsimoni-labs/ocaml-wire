(** The one module generated for a codec, whatever its shape.

    Its parts are written from the codec's plan, each where the plan has it: the
    folds, the check and the views of every codec the walk covers, and the
    members a fixed record of named scalars adds. Internal to {!Wire_ocaml}. *)

open Planner

type target = [ `OCaml | `OxCaml ]

type require = [ `Record | `Walk | `Any ]
(** The part a caller needs: the fixed record's members, the walk's folds, or
    either. *)

type fold = {
  name : string;
  outputs : string list option;  (** Every output for [None]. *)
}
(** A fold the module declares. *)

type t
(** The module of a codec. *)

val v :
  ?target:target ->
  ?simd:bool ->
  ?maps:(string * string * typ) list ->
  ?folds:fold list ->
  require:require ->
  codec:string ->
  'a Wire.Codec.t ->
  t
(** [v ?target ?simd ?maps ?folds ~require ~codec c] is the module of [c], with
    [folds] (default none) when the walk covers [c]; see {!Wire_ocaml.v}. Raises
    [Invalid_argument] naming the field and the reason when [c] has no part
    [require] names, or when two of the names the module defines would be one.
*)

val ml :
  ?decide:(Decision.t -> unit) -> visitors:(string * string) list -> t -> string
(** [ml ?decide ~visitors t] is the source of [t], each fold calling the module
    [visitors] names for it, which names one for each. [decide] is given each
    decision its walks take, in the order it writes them. *)

val mli : visitors:(string * string) list -> t -> string
(** [mli ~visitors t] is the interface of [ml ~visitors t]. *)

val views : t -> View_module.t option
(** [views t] is the views of [t], if its walk covers its codec. *)

val twin_ml : visitors:(string * string) list -> t -> string
(** [twin_ml ~visitors t] is the source of the interpreted twin of
    [ml ~visitors t]: a functor over the codec value that reads it as [t] reads
    its bytes. *)

val twin_mli : visitors:(string * string) list -> t -> string
(** [twin_mli ~visitors t] is the interface of [twin_ml ~visitors t]. *)
