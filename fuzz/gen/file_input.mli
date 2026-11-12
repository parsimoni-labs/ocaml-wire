(** AFL file inputs: the control header every AFL input starts with, and the
    persistent-loop suites that read one input per execution. *)

val truncate_bytes : max_len:int -> bytes -> bytes
(** [truncate_bytes ~max_len bs] caps [bs] at [max_len] bytes. *)

type afl_input = { selector : int; mode : int; payload : bytes }
(** Parsed AFL file input. {!field-selector} chooses the schema/target,
    {!field-mode} chooses a target-specific contract branch, and
    {!field-payload} is the protocol data. *)

val afl_payload : bytes -> bytes
(** [afl_payload bs] strips the two-byte control header from [bs]. *)

val parse_afl_input : ?max_len:int -> bytes -> afl_input
(** [parse_afl_input bs] parses the control header and truncates the payload to
    [max_len] bytes. *)

val afl_contract_cases :
  ?max_len:int ->
  string ->
  (string * 'a) list ->
  check:(string -> 'a -> afl_input -> unit) ->
  Alcobar.test_case list
(** [afl_contract_cases label items ~check] selects one of [items] per input and
    hands it to [check] with the parsed input. *)

val afl_cases : ?max_len:int -> string -> Alcobar.test_case list
(** [afl_cases label] checks decode / validate crash-safety of a registry codec
    picked per AFL input. *)

val afl_env_cases : ?max_len:int -> string -> Alcobar.test_case list
(** [afl_env_cases label] is {!afl_cases} restricted to registry codecs that
    bind a {!Wire.Param.env}. *)
