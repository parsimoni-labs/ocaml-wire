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
  (string, int) Hashtbl.t ->
  (string option * 'a) list ->
  bool Types.expr option ->
  (slots -> bool) option
(** [compile_where_clause param_slots field_readers where] compiles a record's
    [where] clause onto the scratch. *)

val build_validators :
  ('a * (slots -> Types.eval_ctx -> Input_end.t -> bytes -> int -> unit)) list ->
  ('c * ('d -> 'e -> 'f -> 'g -> 'h -> unit)) list ->
  (slots -> bool) option ->
  Types.field list ->
  (string, int) Hashtbl.t ->
  int ->
  (slots -> Types.eval_ctx -> Input_end.t -> bytes -> int -> unit)
  * ('d -> 'e -> 'f -> 'g -> 'h -> unit)
  * (?env_slots:int array -> bytes -> int -> unit)
(** [build_validators validators checkers where fields param_slots n_total] is
    [(validate_arr, populate, validate)]: the full pass over a scratch, the
    check-only pass, and a validator that owns its scratch. *)

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
  n_total:int ->
  param_base:int ->
  param_handles:Param.packed list ->
  validate_arr:(slots -> Types.eval_ctx -> 'a -> 'b -> 'c -> unit) ->
  Types.eval_ctx ->
  'a ->
  'b ->
  'c ->
  unit
(** [validate_runtime_of] runs [validate_arr] on a cleared scratch seeded with
    the context's parameters, then writes the output parameters back. *)

val seal : ('r, 'r) record -> 'r t
(** [seal r] is the codec of the finished record [r]. *)
