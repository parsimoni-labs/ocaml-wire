(** How a single field is read and written in place: the staged readers and
    writers of {!Codec.get} and {!Codec.View.set}. Internal to {!Codec}. *)

val build_staged_reader :
  'a Types.typ ->
  Codec_repr.field_access ->
  Types.eval_ctx ->
  Input_end.t ->
  bytes ->
  int ->
  'a
(** [build_staged_reader t access runtime input_end buf off] is the field of
    type [t] that [access] places in the record at [off]. It checks that the
    bytes it takes lie in the input and raises an end-of-input [Parse_error]
    where they do not, as it raises the errors of the value's own checks. Raises
    [Invalid_argument] at staging for a type and a place with no reader. *)

val build_staged_writer :
  'a Types.typ ->
  Codec_repr.field_access ->
  Types.eval_ctx ->
  bytes ->
  int ->
  'a ->
  unit
(** [build_staged_writer t access runtime buf off v] writes [v] in the field of
    type [t] that [access] places in the record at [off], leaving the buffer
    untouched when it raises. *)

val build_immediate_staged_reader :
  'a Types.typ -> Codec_repr.field_access -> (bytes -> int -> 'a) option
(** [build_immediate_staged_reader t access] is a reader that needs no
    evaluation context, with no bounds check of its own, or [None] where the
    layout may consult one. *)

val build_immediate_get :
  name:string ->
  'a Types.typ ->
  Codec_repr.field_access ->
  'a Codec_leaf.get option
(** [build_immediate_get ~name t access] is the reader {!Codec.get} makes for
    the field [name] of type [t] at a constant place: one closure that checks
    the field's bytes lie in the buffer, loads them and answers [Ok v], or the
    error of a closed enum's value it does not list. [None] for a field whose
    read can fail otherwise, such as a map's, or whose place is not constant. *)

val build_immediate_staged_writer :
  'a Types.typ -> Codec_repr.field_access -> (bytes -> int -> 'a -> unit) option
(** [build_immediate_staged_writer t access] is the writer twin of
    {!build_immediate_staged_reader}. *)

val immediate_extent :
  'a Types.typ -> Codec_repr.field_access -> (int * int) option
(** [immediate_extent t access] is the offset and the width of a field of type
    [t] at a constant offset in its record, or [None] for any other place. *)
