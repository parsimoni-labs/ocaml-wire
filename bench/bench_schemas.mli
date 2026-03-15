(** Benchmark schemas covering all Wire API constructs. *)

type 'a schema = private {
  name : string;
  codec : 'a Wire.Codec.t;
  struct_ : Wire.struct_;
  size : int;
  default : 'a;
  make_data : int -> bytes array;
  decode : bytes -> int -> 'a;
  encode : 'a -> bytes -> int -> unit;
}
(** Type-erased schema for benchmark iteration. *)

type any_schema = Any : 'a schema -> any_schema

val all_schemas : any_schema list
(** [all_schemas] is the list of all benchmark schemas. *)

val all_structs : Wire.struct_ list
(** [all_structs] is the list of all benchmark structs. *)

(** {2 CLCW} *)

type clcw

val clcw_codec : clcw Wire.Codec.t
val clcw_size : int
val clcw_default : clcw
val clcw_data : int -> bytes array
val cw_report : (int, clcw) Wire.Codec.field

(** {2 SpacePacket} *)

type space_packet

val space_packet_codec : space_packet Wire.Codec.t
val space_packet_data : int -> bytes array
