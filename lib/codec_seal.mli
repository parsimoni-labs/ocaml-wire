(** Assembly of the record codec.

    {!add_field} applies a field's plan to the record under construction,
    folding its constraint, [where] condition and action into the check pass.
    {!seal} turns the finished record into a codec: the frame check, the
    validators over the scratch, the decoder that builds the value and the
    encoder that writes it and checks the result. Internal to {!Codec}. *)

open Codec_repr

val guarded_populate :
  ('a, 'r) Codec_compile.compiled_field ->
  'a Types.typ ->
  slots ->
  runtime ->
  Input_end.t ->
  bytes ->
  int ->
  unit
(** [guarded_populate cf t arr runtime input_end buf base] runs the populate
    function of [cf], after the bounds check of a field whose offset depends on
    the data. *)

val field_validators :
  field_idx:int ->
  validator_off:int ->
  populate:(slots -> 'a -> 'b -> 'c -> int -> unit) ->
  check:(slots -> bool) option ->
  act:(slots -> unit) option ->
  (slots -> 'a -> 'b -> 'c -> int -> unit)
  * (slots -> 'a -> 'b -> 'c -> int -> unit)
(** [field_validators ~field_idx ~validator_off ~populate ~check ~act] is the
    pair [(check_only, full)]: populate the field's slot and test its
    constraint, then for [full] also run its action. *)

val add_field : ('a -> 'f, 'r) record -> ('a, 'r) field -> ('f, 'r) record
(** [add_field r f] appends [f] to [r]. *)

val compile_where_clause :
  ?used:(int, unit) Hashtbl.t ->
  (string, int) Hashtbl.t ->
  (string option * 'a) list ->
  bool Types.expr option ->
  (slots -> bool) option
(** [compile_where_clause ?used param_slots field_readers where] compiles a
    record's [where] clause onto the scratch, noting in [used] the slots it
    reads. *)

val build_validators :
  ?used:(int, 'a) Hashtbl.t ->
  ('b * step) list ->
  ('c * ('d -> 'e -> 'f -> 'g -> 'h -> unit)) list ->
  (slots -> bool) option ->
  Types.field list ->
  (string, int) Hashtbl.t ->
  int ->
  (slots -> runtime -> Input_end.t -> bytes -> int -> unit)
  * ('d -> 'e -> 'f -> 'g -> 'h -> unit)
  * (?env_slots:int array -> bytes -> int -> unit)
  * (slots -> unit)
  * (step array * bool)
(** [build_validators ?used validators checkers where fields param_slots
     n_total] is
    [(validate_arr, populate, validate, clear, (steps, needs_arr))]: the full
    pass over a scratch, the check-only pass, a validator that owns its scratch,
    the function that zeroes a scratch before a pass, and the steps of the pass
    with whether any of them needs a scratch. Only the slots in [used] are
    filled when it is given. *)

val collect_param_handles :
  string -> Types.field list -> bool Types.expr option -> Param.packed list
(** [collect_param_handles name fields where] lists the parameters a codec
    refers to, once each. Raises [Invalid_argument] when two distinct parameters
    share a name. *)

val fill_param_slots :
  (string, int) Hashtbl.t -> int -> Param.packed list -> unit
(** [fill_param_slots tbl base handles] maps each parameter to its scratch slot,
    from [base] on. *)

val validate_runtime_of :
  scratch:(unit -> slots) ->
  clear:(slots -> unit) ->
  param_base:int ->
  param_handles:Param.packed list ->
  validate_arr:(slots -> Types.eval_ctx -> 'a -> 'b -> 'c -> unit) ->
  Types.eval_ctx ->
  'a ->
  'b ->
  'c ->
  unit
(** [validate_runtime_of] runs [validate_arr] on a scratch cleared by [clear]
    and seeded with the context's parameters, then writes the output parameters
    back. *)

val seal :
  struct_fields:Types.field list ->
  param_handles:Param.packed list ->
  ('r, 'r) record ->
  'r compiled
(** [seal ~struct_fields ~param_handles r] is the compiled form of the finished
    record [r], whose fields are [struct_fields] and parameters [param_handles].
*)

val typ_where_conds : 'a Types.typ -> bool Types.expr list
(** [typ_where_conds t] is the conditions of the [where] wrappers of [t],
    outermost first. *)

val check_decode_bounds :
  wire_size_info -> int -> runtime -> Input_end.t -> bytes -> int -> unit
(** [check_decode_bounds size min_size runtime input_end buf off] checks that
    the record at [off] lies inside [buf]: [min_size] bytes, then the span
    [size] computes from the data when it is variable. Raises [Parse_error] on a
    short buffer. *)

val min_bounds_error : int -> bytes -> int -> 'a
(** [min_bounds_error min_size buf off] raises the [Parse_error] of a record of
    [min_size] bytes that does not fit [buf] at [off]. *)

val run_plain_pass : pass -> Input_end.t -> bytes -> int -> unit
(** [run_plain_pass pass input_end buf off] runs the check pass of a codec
    without parameters over the record at [off], whose frame the caller has
    checked. Raises [Parse_error] when a check fails. *)

val encode_too_short : string -> int -> bytes -> int -> 'a
(** [encode_too_short name n buf off] raises the [Invalid_argument] of encoding
    codec [name], of [n] bytes, at [off] in a [buf] too short for it, or at a
    negative [off]. *)

val encode_rejected : string -> Types.parse_error -> 'a
(** [encode_rejected name e] raises the [Invalid_argument] of an encode of codec
    [name] whose result fails its own checks with [e]. *)
