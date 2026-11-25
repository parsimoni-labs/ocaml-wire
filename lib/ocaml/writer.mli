(** The one module generated for a codec, whatever its shape.

    Its parts are written from the codec's plan, each where the plan has it: the
    folds, the check and the views of every codec the walk covers, and the
    members a fixed record of named scalars adds. Internal to {!Wire_ocaml}. *)

open Planner

type target = [ `OCaml | `OxCaml ]

type require = [ `Record | `Walk | `Any ]
(** The part a caller needs: the fixed record's members, the walk's folds, or
    either. *)

type t
(** The module of a codec. *)

val v :
  ?target:target ->
  ?buffer:[ `Bytes | `Bigstring ] ->
  ?simd:bool ->
  ?make:string * string ->
  ?maps:(string * string * typ) list ->
  ?fields:string list ->
  ?step:string ->
  require:require ->
  codec:string ->
  'a Wire.Codec.t ->
  t
(** [v ?target ?buffer ?simd ?make ?maps ?fields ?step ~require ~codec c] is the
    module of [c] on [buffer] (default [`Bytes]); see {!Wire_ocaml.ml} and
    {!Wire_ocaml.scanner}. A module on a bigarray is a fixed record's, with no
    walk. Raises [Invalid_argument] naming the field and the reason when [c] has
    no part [require] names, or when two of the names the module defines would
    be one. *)

val ml : ?decide:(Decision.t -> unit) -> t -> string
(** [ml ?decide t] is the source of [t]. [decide] is given each decision its
    walks take, in the order it writes them. *)

val mli : t -> string
(** [mli t] is the interface of [ml t]. *)

val views : t -> View_module.t option
(** [views t] is the views of [t], if its walk covers its codec. *)
