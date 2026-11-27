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
  args : arg list option;
      (** The arguments the fold hands [f], which it takes with [invalid] as
          arguments; [None] for a fold that calls a visitor by name. *)
}
(** What a scanner is written from: the plan, the outputs it hands out, the mode
    of its walks, where it writes, the names of the functions it has written and
    how its fold is given its consumer. *)

and arg = {
  value : output;  (** Its output. *)
  typed : string -> string;
      (** The text of its value in its carrier, from that of an [int]. *)
  own : output option;  (** Its presence, when the field is optional. *)
  layered : bool;  (** It is reached through a layer: one option more. *)
  layer : output option;
      (** The presence of the innermost optional layer of its path. *)
}
(** A field a fold hands its [f]. *)

val slots : plan -> output list -> output list
(** [slots plan outputs] is the outputs among [outputs] that are slots; see
    {!Wire_ocaml.Plan.slots}. *)

val step : string
(** [step] is the name of the walks of the fold of a module, as the F* rendering
    names them: [fold_step], [fold_step_frames] and [fold_step_index]. *)

type fold = {
  selected : output list;  (** The outputs its walk binds and hands out. *)
  args : arg list;  (** What it hands [f], in order. *)
}
(** The fold a module declares. *)

val walks :
  ?decide:(Decision.t -> unit) ->
  target:[ `OCaml | `OxCaml ] ->
  ?fold:fold ->
  ?size:int ->
  taken:(string, unit) Hashtbl.t ->
  plan ->
  string
(** [walks ?decide ~target ?fold ?size ~taken plan] is the walks of a module
    over [plan]: with [fold], {!step}, its [_frames] and [_index], which take
    [~f] and [~invalid] after [~init] and call [f] with the [args] of [fold] in
    order where a record passes and [invalid] where it does not. Then [check]:
    [check b ~off ~len], or [check b ~off] for a codec of constant [size]. Each
    function it writes is named apart from [taken], which it adds to. [decide]
    is given each decision the walks take, in the order it writes them. *)

val pp_fold : Format.formatter -> name:string -> size:int option -> unit
(** [pp_fold ppf ~name ~size] writes [fold] of the module [name], whose records
    take [size] bytes when constant, over the walks of {!walks}, and the
    functions that find the error of the first record it refuses. It calls
    [validate], [check], [is_ok] and [no_error], and [end_at] where [size] is
    [None]. *)

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
(** The F* rendering of a fold: its text, what its walk states of each binding,
    the outputs it hands out and, for the walk of a frame, the function it is.
*)

val fstar_step : ?fields:string list -> step:string -> 'a Wire.Codec.t -> fstar
(** [fstar_step ?fields ~step c] is the F* rendering of [fold_step] and
    [fold_step_within], the fold of [c] named [fold_step] whose visitor is the
    module [step], with values in [int64]: they call [step]'s [invalid] and
    [valid] by name. It is the walk of the OCaml one, with in place each
    function the OCaml one cuts out. Raises {!Planner.Unsupported} on a codec
    whose walk needs a slot. *)

val fstar_ox_step :
  ?fields:string list -> step:string -> 'a Wire.Codec.t -> fstar
(** [fstar_ox_step ?fields ~step c] is {!fstar_step} for the OxCaml scanner of
    [c], whose values and positions are [int64_u], written as their 64-bit
    patterns. *)

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
