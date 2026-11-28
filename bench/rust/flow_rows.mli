(** The rows of the flow classifier that parse with Wire.Codec or with the
    generated modules, each handing every packet's key to {!Flow_table};
    [Flow_rows_parse] and [Flow_rows_keys] are the same rows over {!Flow_sum}
    and {!Flow_keys}. [buf] is a pcap capture held whole, [offs] and [lens] the
    offset and captured length of each frame in it. *)

val flow_codec : bytes -> int array -> int array -> unit
(** [flow_codec buf offs lens] decodes each frame with [Codec.decode], bounded
    by the pcap record around it. *)

val flow_view : bytes -> int array -> int array -> unit
(** [flow_view buf offs lens] reads each frame with the record modules of the
    fixed headers, the layers linked here. *)

val flow_check : bytes -> int array -> int array -> unit
(** [flow_check buf offs lens] checks each frame with the [check] of the module
    of [Packet_stack.capture] Wire_ocaml writes, then reads it through its
    views: an accessor per field, and per sub-record one that enters it. *)

val flow_fold : bytes -> int array -> int array -> unit
(** [flow_fold buf offs lens] folds over the frames of the index with the fold
    of [Packet_stack.capture] Wire_ocaml writes, its [f] given the fields of
    each frame's key. *)

val replay : int array -> unit
(** [replay keys] hands the keys of {!Flow_keys.take} to the sink, with no
    parse. *)
