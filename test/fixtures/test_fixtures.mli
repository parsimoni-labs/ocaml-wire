(** Shared codec fixtures used by the test suites. *)

open Wire

type inner = { tag : int; value : int }

val f_inner_tag : int Field.t
(** [f_inner_tag] is the field reference for [inner.tag]; reused by codecs that
    depend on the parsed tag value. *)

val f_inner_value : int Field.t
(** [f_inner_value] is the field reference for [inner.value]. *)

val inner_codec : inner Codec.t
(** [inner_codec] is the tag-prefixed inner record used as the building block
    for [outer_codec], [repeat_codec], and many test cases. *)

type outer = { header : int; inner : inner; trailer : int }

val outer_codec : outer Codec.t
(** [outer_codec] embeds [inner_codec] between a header and trailer byte;
    exercises the [codec] field combinator. *)

type l2 = { l2_x : int }
type l1 = { l1_inner : l2; l1_y : int }
type l0 = { l0_inner : l1; l0_z : int }

val l2_codec : l2 Codec.t
(** [l2_codec] is the innermost level of a 3-deep codec nesting. *)

val l1_codec : l1 Codec.t
(** [l1_codec] is the middle level, embedding [l2_codec]. *)

val l0_codec : l0 Codec.t
(** [l0_codec] is the outermost level, embedding [l1_codec]; exercises two-level
    nesting. *)

type opt_record = { opt_hdr : int; opt_payload : int option; opt_trail : int }

val opt_codec : present:bool -> opt_record Codec.t
(** [opt_codec ~present] is a record with an optional middle field; [present]
    selects whether the payload is included. *)

val opt_codec_present : opt_record Codec.t
(** [opt_codec_present] is [opt_codec ~present:true]. *)

val opt_codec_absent : opt_record Codec.t
(** [opt_codec_absent] is [opt_codec ~present:false]. *)

type container = { cnt_length : int; cnt_items : inner list }

val f_cnt_length : int Field.t
(** [f_cnt_length] is the field reference for [container.cnt_length], used to
    size the trailing [repeat] body. *)

val repeat_codec : container Codec.t
(** [repeat_codec] is a length-prefixed list of [inner] elements. *)

type packet = { pkt_id : int; pkt_data : int }

val packet_codec : packet Codec.t
(** [packet_codec] is a fixed-shape packet record reused by TM-frame style
    composition tests. *)

type multi_record = { x : int; y : int }

val multi_record_codec : multi_record Codec.t
(** [multi_record_codec] is a two-[uint16be]-field record reused by ASCII
    rendering and codec encode/decode tests. *)
