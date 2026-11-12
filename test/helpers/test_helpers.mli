(** Shared codec fixtures used by the test suites. *)

open Wire

type inner = { tag : UInt8.t; value : UInt16.t }

val f_inner_tag : UInt8.t Field.t
(** [f_inner_tag] is the field reference for the inner tag; reused by codecs
    that depend on the parsed tag value. *)

val f_inner_value : UInt16.t Field.t
(** [f_inner_value] is the field reference for the inner value. *)

val inner_codec : inner Codec.t
(** [inner_codec] is the tag-prefixed inner record used as the building block
    for {!outer_codec}, {!repeat_codec}, and many test cases. *)

type outer = { header : UInt8.t; inner : inner; trailer : UInt8.t }

val outer_codec : outer Codec.t
(** [outer_codec] embeds {!inner_codec} between a header and trailer byte;
    exercises the [codec] field combinator. *)

type l2 = { x : UInt8.t }
type l1 = { inner : l2; y : UInt16.t }
type l0 = { inner : l1; z : UInt8.t }

val l2_codec : l2 Codec.t
(** [l2_codec] is the innermost level of a 3-deep codec nesting. *)

val l1_codec : l1 Codec.t
(** [l1_codec] is the middle level, embedding {!l2_codec}. *)

val l0_codec : l0 Codec.t
(** [l0_codec] is the outermost level, embedding {!l1_codec}; exercises
    two-level nesting. *)

type opt_record = { hdr : UInt8.t; payload : UInt16.t option; trail : UInt8.t }

val opt_codec : present:bool -> opt_record Codec.t
(** [opt_codec ~present] is a record with an optional middle field; [present]
    selects whether the payload is included. *)

val opt_codec_present : opt_record Codec.t
(** [opt_codec_present] is [opt_codec ~present:true]. *)

val opt_codec_absent : opt_record Codec.t
(** [opt_codec_absent] is [opt_codec ~present:false]. *)

type container = { length : UInt8.t; items : inner list }

val f_cnt_length : UInt8.t Field.t
(** [f_cnt_length] is the field reference for the container length, used to size
    the trailing [repeat] body. *)

val repeat_codec : container Codec.t
(** [repeat_codec] is a length-prefixed list of {!type:inner} elements. *)

type packet = { id : UInt8.t; data : UInt16.t }

val packet_codec : packet Codec.t
(** [packet_codec] is a fixed-shape packet record reused by TM-frame style
    composition tests. *)

type multi_record = { x : UInt16.t; y : UInt16.t }

val multi_record_codec : multi_record Codec.t
(** [multi_record_codec] is a two-[uint16be]-field record reused by ASCII
    rendering and codec encode/decode tests. *)

(** {1 Decode assertions} *)

val decode_ok : ('a, parse_error) result -> 'a
(** [decode_ok r] returns the decoded value, failing the test on [Error]. *)

val expect_constraint_fail : ('a, parse_error) result -> unit
(** [expect_constraint_fail r] asserts [r] is [Error] with a [Constraint_failed]
    kind, failing the test otherwise. *)

val expect_kind : (error_kind -> bool) -> ('a, parse_error) result -> unit
(** [expect_kind pred r] asserts [r] is [Error] whose [kind] satisfies [pred].
*)

val roundtrip : string -> 'a typ -> 'a Alcotest.testable -> 'a -> unit
(** [roundtrip name typ testable v] checks that decoding the encoding of [v]
    yields [v] again, failing the test on a parse error. *)

(** {1 Codec test fixtures} *)

val contains : sub:string -> string -> bool
(** [contains ~sub s] is [true] iff [sub] occurs in [s]. *)

val render_3d : 'r Codec.t -> string
(** [render_3d codec] is the 3D rendering of [codec], for substring assertions.
*)

val encode_record : 'r Codec.t -> 'r -> (string, 'e) result
(** [encode_record codec v] is [Ok] of the encoding of [v] with [codec]. *)

val decode_record : 'r Codec.t -> string -> ('r, parse_error) result
(** [decode_record codec s] decodes [s] with [codec], reporting an
    [Unexpected_eof] when [s] is shorter than the codec's wire size. *)

type simple_record = { a : UInt8.t; b : UInt16.t; c : UInt32.t }

val simple_record_codec : simple_record Codec.t
(** [simple_record_codec] is a three-field [uint8], [uint16], [uint32] record.
*)

type meta_record = { x : UInt8.t }

val projection_limit : (UInt8.t, Param.input) Param.t
(** [projection_limit] is the input parameter bounding {!projection_codec}. *)

val projection_outx : (UInt8.t, Param.output) Param.t
(** [projection_outx] is the output parameter {!projection_codec} assigns. *)

val projection_f_x : UInt8.t Field.t
(** [projection_f_x] is the field {!projection_codec}'s checks refer to. *)

val projection_codec : meta_record Codec.t
(** [projection_codec] is a one-field record carrying a where clause, a
    constraint and an action over parameters. *)

val skip_unless_gc_counters : unit -> unit
(** [skip_unless_gc_counters ()] skips the current test unless the runtime
    counts [Gc.minor_words], i.e. under the native and bytecode backends. *)

val raises_invalid : (unit -> 'a) -> bool
(** [raises_invalid f] is [true] iff [f ()] raises [Invalid_argument]. *)

val printable_byte : int expr -> bool expr
(** [printable_byte b] holds when [b] is a printable ASCII byte. *)

module Bs = Bytesrw.Bytes.Slice
