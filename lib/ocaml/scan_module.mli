(** The scanner generated for a structured codec.

    A function that checks a record in place and hands its named scalars to a
    consumer, without building a value. Internal to {!Wire_ocaml}. *)

open Planner

val scan_plan : 'a Wire.Codec.t -> plan
(** [scan_plan c] is the plan a scanner of [c] follows. Raises
    {!Planner.Unsupported} on a shape the generator does not handle. *)

val scanner_fields : 'a Wire.Codec.t -> (string * bool) list
(** [scanner_fields c] is the scalars a scanner of [c] hands out; see
    {!Wire_ocaml.scanner_fields}. *)

type scan = {
  plan : plan;
  selected : output list;
  num : Emitter.num;
  ppf : Format.formatter;
}
(** What a scanner is written from: the plan, the outputs it hands out, how it
    holds integers and where it writes. *)

val scanner :
  ?target:[> `OCaml | `OxCaml ] ->
  ?fields:string list ->
  codec:string ->
  'a Wire.Codec.t ->
  string
(** [scanner ?target ?fields ~codec c] is the scanner generated for [c]; see
    {!Wire_ocaml.scanner}. *)
