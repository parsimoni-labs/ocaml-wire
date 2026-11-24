(** The rows of the flow classifier that parse with Wire.Codec or with the
    generated record modules, each handing every packet's key to {!Flow_table};
    [Flow_rows_parse] and [Flow_rows_keys] are the same rows over {!Flow_sum}
    and {!Flow_keys}. [buf] is a pcap capture held whole, [offs] and [lens] the
    offset and captured length of each frame in it. *)

val flow_codec : bytes -> int array -> int array -> unit
(** [flow_codec buf offs lens] decodes each frame with [Codec.decode], bounded
    by the pcap record around it. *)

val flow_view : bytes -> int array -> int array -> unit
(** [flow_view buf offs lens] reads each frame with the record modules of the
    fixed headers, the layers linked here. *)

val replay : int array -> unit
(** [replay keys] hands the keys of {!Flow_keys.take} to the sink, with no
    parse. *)
