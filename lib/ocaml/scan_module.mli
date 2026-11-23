(** The scanner generated for a structured codec.

    A function that checks a record in place and hands its named scalars to a
    consumer, without building a value. Internal to {!Wire_ocaml}. *)

open Planner

val codec_plan : 'a Wire.Codec.t -> plan
(** [codec_plan c] is the plan of [c], with no check of the OCaml names its
    outputs take. Raises {!Planner.Unsupported} on a shape the generator does
    not handle. *)

val scan_plan : 'a Wire.Codec.t -> plan
(** [scan_plan c] is the plan a scanner of [c] follows. Raises
    {!Planner.Unsupported} on a shape the generator does not handle. *)

val scanner_fields : 'a Wire.Codec.t -> (string * bool) list
(** [scanner_fields c] is the scalars a scanner of [c] hands out; see
    {!Wire_ocaml.scanner_fields}. *)

type scan = {
  plan : plan;
  selected : output list;
  m : Emitter.mode;
  ppf : Format.formatter;
  taken : (string, unit) Hashtbl.t;
}
(** What a scanner is written from: the plan, the outputs it hands out, the mode
    of its walks, where it writes and the names of the functions it has written.
*)

val slots : plan -> output list -> output list
(** [slots plan outputs] is the outputs among [outputs] that are slots; see
    {!Wire_ocaml.Plan.slots}. *)

val scanner :
  ?decide:(Decision.t -> unit) ->
  ?target:[> `OCaml | `OxCaml ] ->
  ?fields:string list ->
  ?step:string ->
  codec:string ->
  'a Wire.Codec.t ->
  string
(** [scanner ?decide ?target ?fields ?step ~codec c] is the scanner generated
    for [c]; see {!Wire_ocaml.scanner}. [decide] is given each decision its
    walks take, in the order it writes them. *)

val selected : plan -> string list option -> output list
(** [selected plan names] is the outputs of [plan] named in [names], all of them
    when [names] is [None]. Raises {!Planner.Unsupported} on a name [plan] does
    not hand out. *)
