(** The one module generated for a codec, whatever its shape.

    Its parts are written from the codec's plan, each where the plan has it: the
    fold, the check and the views of every codec the walk covers, and the
    members a fixed record of named scalars adds. Internal to {!Wire_ocaml}. *)

open Planner

type target = [ `OCaml | `OxCaml ]

type require = [ `Record | `Walk | `Any ]
(** The part a caller needs: the fixed record's members, the walk, or either. *)

type t
(** The module of a codec. *)

val v :
  ?target:target ->
  ?simd:bool ->
  ?maps:(string * string * typ) list ->
  ?fold:string list ->
  require:require ->
  codec:string ->
  'a Wire.Codec.t ->
  t
(** [v ?target ?simd ?maps ?fold ~require ~codec c] is the module of [c], with a
    fold of the fields [fold] names by their paths; see {!Wire_ocaml.v}. A fold
    needs the walk. Raises [Invalid_argument] naming the field and the reason
    when [c] has no part [require] names, or for a path no fold reads. *)

val ml : ?decide:(Decision.t -> unit) -> t -> string
(** [ml ?decide t] is the source of [t]. [decide] is given each decision its
    walks take, in the order it writes them. *)

val mli : t -> string
(** [mli t] is the interface of [ml t]. *)

val views : t -> View_module.t option
(** [views t] is the views of [t], if its walk covers its codec. *)
