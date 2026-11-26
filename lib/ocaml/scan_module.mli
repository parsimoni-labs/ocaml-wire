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
  fold : string;  (** The fold being written. *)
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

type fold = {
  name : string;
  selected : output list;  (** The outputs it hands out. *)
  visitor : string option;  (** The module it calls by name, if any. *)
}
(** A fold a module declares. *)

val walks :
  ?decide:(Decision.t -> unit) ->
  target:[ `OCaml | `OxCaml ] ->
  folds:fold list ->
  ?size:int ->
  taken:(string, unit) Hashtbl.t ->
  plan ->
  string
(** [walks ?decide ~target ~folds ?size ~taken plan] is the walks of a module
    over [plan]: for each of [folds] named [n], without a visitor [n] and
    [n_frames], which take their consumer as arguments; with a visitor [n],
    [n_frames] and [n_index], which call it by name. Then [check]:
    [check b length ~off ~len], or [check b length off] for a codec of constant
    [size]. Each function it writes is named apart from [taken], which it adds
    to. [decide] is given each decision the walks take, in the order it writes
    them. *)

val without_dead_bindings : string -> string
(** [without_dead_bindings text] is [text] without the bindings it never reads.
*)

val mentions : string -> string -> bool
(** [mentions x text] is [true] when the identifier [x] occurs in [text]. *)

val selected : plan -> string list option -> output list
(** [selected plan names] is the outputs of [plan] named in [names], all of them
    when [names] is [None]. Raises {!Planner.Unsupported} on a name [plan] does
    not hand out. *)

type frame = { name : string; params : (string * string) list; need : int }
(** The walk of a frame of a frame loop rendered as an F* function: its name,
    its parameters each with its F* type, which states what the loop ensures of
    it, and the bytes the loop knows a frame holds. *)

type fstar = {
  body : string;
  notes : Emitter.note list;
  handed_out : output list;
  frame : frame option;
}
(** The F* rendering of a scanner's [fold]: its text, what its walk states of
    each binding, the outputs it hands out and, for the walk of a frame, the
    function it is. *)

val fstar : ?fields:string list -> 'a Wire.Codec.t -> fstar
(** [fstar ?fields c] is the F* rendering of [fold] in the scanner of [c]: the
    walk of the OCaml one, with in place each function the OCaml one cuts out.
    Raises {!Planner.Unsupported} on a codec whose walk needs a slot. *)

val fstar_step : ?fields:string list -> step:string -> 'a Wire.Codec.t -> fstar
(** [fstar_step ?fields ~step c] is {!fstar} for [fold_step] and
    [fold_step_within], the fold of [c] named [fold_step] whose visitor is the
    module [step], with values in [int64]: they call [step]'s [invalid] and
    [valid] by name. *)

val fstar_ox : ?fields:string list -> 'a Wire.Codec.t -> fstar
(** [fstar_ox ?fields c] is {!fstar} for the [fold] of the OxCaml scanner of
    [c], whose values and positions are [int64_u], written as their 64-bit
    patterns. *)

val fstar_ox_step :
  ?fields:string list -> step:string -> 'a Wire.Codec.t -> fstar
(** [fstar_ox_step ?fields ~step c] is {!fstar_step} for the OxCaml scanner of
    [c]. *)

val fstar_frame :
  ?fields:string list ->
  ox:bool ->
  index:bool ->
  step:string ->
  'a Wire.Codec.t ->
  fstar
(** [fstar_frame ?fields ~ox ~index ~step c] is the F* rendering of the walk of
    a frame of the loop of [fold_step_frames], or with [index] of
    [fold_step_index], of the fold {!fstar_step} renders, of OxCaml with [ox]:
    the walk of a frame the loop knows to hold the bytes it tests, with the
    fields it loads ahead as parameters. *)
