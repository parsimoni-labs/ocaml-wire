(** Schema definitions for differential testing. *)

type simple_header = { version : int; length : int; flags : int }
(** A simple packet header with version, length, and flags fields. *)

val simple_header_codec : simple_header Wire.Codec.t
(** Record codec for encoding/decoding simple headers. *)

val simple_header_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D code generation. *)

val simple_header_module : Wire.Everparse.Raw.module_
(** Module definition for 3D code generation. *)

type constrained_packet = { pkt_type : int; pkt_length : int }
(** A packet with type and length fields, where length must be >= 4. *)

val constrained_packet_codec : constrained_packet Wire.Codec.t
(** Record codec for encoding/decoding constrained packets. *)

val constrained_packet_module : Wire.Everparse.Raw.module_
(** Module definition for 3D code generation. *)
