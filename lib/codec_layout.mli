(** The layout pass of a variable-size record without parameters.

    The layout of such a record lists how each field adds to its extent: fixed
    widths, byte spans, the rest of the buffer, gated fields and sub-codecs,
    sized by fields at fixed offsets before them. One walk over it measures the
    record and checks the spans and sub-codecs it holds, and either accepts,
    with the record's end, or leaves the record to the general passes, which
    answer as they always did. *)

type items
(** A record's layout, compiled for the walks. *)

val layout_items :
  (string * Codec_repr.packed_scalar) list ->
  Codec_repr.layout list option ->
  items option
(** [layout_items prefix layout] compiles the layout of a record, given in
    reverse field order, reading sizes and gates from the fixed-offset fields
    [prefix] names. [None] when the record has no layout or a size or gate reads
    anything else. *)

val build_layout :
  min_size:int -> items option -> (Input_end.t -> bytes -> int -> int) option
(** [build_layout ~min_size items] is the layout pass: [walk input_end buf off]
    is where the record at [off] ends when every span and sub-codec in it fits
    before [input_end] and passes its checks, else [-1]. *)

val layout_extent :
  min_size:int ->
  items ->
  (Types.eval_ctx -> Input_end.t -> bytes -> int -> int) ->
  Types.eval_ctx ->
  Input_end.t ->
  bytes ->
  int ->
  int
(** [layout_extent ~min_size items compute ctx input_end buf off] is where the
    record at [off] ends: the extent walk's answer when it gives one, else
    [compute]'s, value or error. Both measure the same fields the same way. *)
