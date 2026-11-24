(** The error of a record that does not fit its input.

    The validator 3D generates checks each field as it reaches it, so a record
    that does not fit reports the first field in order that fails its checks
    within the input, and the end of input only when none does. These run a
    record's checks in that order, and only on a record that does not fit. *)

open Codec_repr

type faults = runtime -> Input_end.t -> bytes -> int -> unit
(** The checks of a record at an offset, in field order, each field read once it
    fits: raises the first that fails, or end of input at the first field that
    does not fit. *)

val unfit : faults:faults -> int -> runtime -> Input_end.t -> bytes -> int -> 'a
(** [unfit ~faults min_size runtime input_end buf off] raises the [Parse_error]
    of a record of [min_size] bytes at [off] that does not fit before
    [input_end]: the error [faults] raises, else the end of input. *)

val short : faults:faults -> int -> bytes -> int -> 'a
(** [short ~faults min_size buf off] is {!unfit} at the end of [buf], with no
    parameters. *)

val check_frame :
  faults:faults -> int -> runtime -> Input_end.t -> bytes -> int -> unit
(** [check_frame ~faults min_size runtime input_end buf off] checks that the
    record of [min_size] bytes at [off] ends before [input_end], raising what
    {!unfit} raises when it does not. *)

val check_measured :
  faults:faults -> int -> runtime -> Input_end.t -> bytes -> int -> int -> unit
(** [check_measured ~faults min_size runtime input_end buf off end_off] checks
    that the record at [off], measured to end at [end_off] before its checks,
    holds [min_size] bytes and ends before [input_end]; when it does not, the
    error [faults] raises comes before the end of input. *)

val record :
  has_checks:bool ->
  n_total:int ->
  param_base:int ->
  param_handles:Param.packed list ->
  runtime_of:(slots -> runtime) ->
  where:bool Types.expr option ->
  compiled_where:(slots -> bool) option ->
  (string list * (slots -> faults)) list ->
  faults
(** [record ~has_checks ~n_total ~param_base ~param_handles ~runtime_of ~where
     ~compiled_where faults_rev] is the faults of a record whose fields' checks,
    last first, are [faults_rev], each with the names it brings in scope. Its
    [where] clause, compiled as [compiled_where], is checked with the last field
    it reads, as the refinement the projection makes it, or before every field
    when it reads none. It runs on [n_total] slots of its own, the parameters
    seeded from [param_base] and read through [runtime_of]. A record without
    checks ([has_checks] false) has no fault but its end. *)
