(** The [agree] differential: the EverParse validators of a package against the
    OCaml codecs they are projected from, on one corpus. *)

val with_agree :
  name:string ->
  package:string ->
  Wire_3d.packed list ->
  (([ `Fuzz of int | `Lines of string list ] -> Unix.process_status * string) ->
  'a) ->
  'a
(** [with_agree ~name ~package codecs f] builds the validators of [codecs] and
    the [agree] harness once, then is [f run], where [run corpus] runs [agree]
    on [corpus] and is its exit status and output. *)

val differential_ok :
  name:string ->
  package:string ->
  corpus:[ `Fuzz of int | `Lines of string list ] ->
  Wire_3d.packed list ->
  unit
(** [differential_ok ~name ~package ~corpus codecs] fails unless the validators
    of [codecs] give each input of [corpus] the verdict the OCaml codec gives
    it. *)

val corpus_line : string -> 'a Wire.Codec.t -> string -> bool -> string
(** [corpus_line name c hex accepted] is the corpus line of the input [hex] for
    the codec [name], after checking that [c] gives it [accepted]: [c] decodes
    it and spans all of it. *)
