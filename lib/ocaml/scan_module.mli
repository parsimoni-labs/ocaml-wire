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

val walks :
  ?decide:(Decision.t -> unit) ->
  target:[ `OCaml | `OxCaml ] ->
  selected:output list ->
  ?step:string ->
  taken:(string, unit) Hashtbl.t ->
  plan ->
  string
(** [walks ?decide ~target ~selected ?step ~taken plan] is the walks of a module
    over [plan]: [fold], [fold_frames], with [step] the step folds, on OxCaml
    the unboxed folds, then [check]. Each function it writes is named apart from
    [taken], which it adds to. [decide] is given each decision the walks take,
    in the order it writes them. *)

val without_dead_bindings : string -> string
(** [without_dead_bindings text] is [text] without the bindings it never reads.
*)

val mentions : string -> string -> bool
(** [mentions x text] is [true] when the identifier [x] occurs in [text]. *)

val selected : plan -> string list option -> output list
(** [selected plan names] is the outputs of [plan] named in [names], all of them
    when [names] is [None]. Raises {!Planner.Unsupported} on a name [plan] does
    not hand out. *)

type fstar = {
  body : string;
  notes : Emitter.note list;
  handed_out : output list;
}
(** The F* rendering of a scanner's [fold]: its text, what its walk states of
    each binding, and the outputs it hands out. *)

val fstar : ?fields:string list -> 'a Wire.Codec.t -> fstar
(** [fstar ?fields c] is the F* rendering of [fold] in the scanner of [c]: the
    walk of the OCaml one, with in place each function the OCaml one cuts out.
    Raises {!Planner.Unsupported} on a codec whose walk needs a slot. *)
