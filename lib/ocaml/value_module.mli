(** The values of a codec of any shape: a record type per record of the codec, a
    decoder that reads each field through its accessor after the check, an
    encoder and a size over the record's plan, and the function that finds the
    codec's error where the check fails. {!Writer} writes them into the module
    of a codec the walk covers when its record holds more than named scalars at
    constant offsets. Internal to {!Wire_ocaml}. *)

val helpers : uses:(string -> bool) -> string
(** [helpers ~uses] is the functions the values raise through that [uses] names,
    written once before the views. *)

val ml_record : Format.formatter -> View_module.t -> View_module.record -> unit
(** [ml_record ppf t r] writes, in the module of the record [r] a field holds,
    its record type [t], [of_view], [encode_at], [size] and [error_at]. *)

val ml_root : Format.formatter -> View_module.t -> View_module.record -> unit
(** [ml_root ppf t r] writes the same for the codec's own record [r], then
    [min_size], [is_valid], [decode], [decode_exn], [validate], [validate_exn]
    and [encode]. *)

val mli_record :
  Format.formatter ->
  target:[ `OCaml | `OxCaml ] ->
  View_module.t ->
  View_module.record ->
  unit
(** [mli_record ppf ~target t r] states the record type and [size] of [r]. *)

val mli_root :
  Format.formatter ->
  target:[ `OCaml | `OxCaml ] ->
  View_module.t ->
  View_module.record ->
  unit
(** [mli_root ppf ~target t r] states the members {!ml_root} writes. *)
