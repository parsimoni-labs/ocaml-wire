(** One bounded Ethernet / IPv4 / TCP or UDP stack. Options and payloads are
    zero-copy slices. IPv4 fragments expose no transport header. *)

open Wire

type tcp = {
  src_port : UInt16.t;
  dst_port : UInt16.t;
  seq : UInt32.t;
  ack : UInt32.t;
  data_offset : int;
  reserved : int;
  flags : int;
  window : UInt16.t;
  checksum : UInt16.t;
  urgent : UInt16.t;
  options : Bytesrw.Bytes.Slice.t;
}

type udp = {
  src_port : UInt16.t;
  dst_port : UInt16.t;
  length : UInt16.t;
  checksum : UInt16.t;
  payload : Bytesrw.Bytes.Slice.t;
}

type ipv4 = {
  version : int;
  ihl : int;
  dscp : int;
  ecn : int;
  total_length : UInt16.t;
  identification : UInt16.t;
  flags : int;
  fragment_offset : int;
  ttl : UInt8.t;
  protocol : UInt8.t;
  checksum : UInt16.t;
  src : UInt32.t;
  dst : UInt32.t;
  options : Bytesrw.Bytes.Slice.t;
  tcp : tcp option;
  udp : udp option;
  opaque : Bytesrw.Bytes.Slice.t option;
}

type t = {
  dst : Bytesrw.Bytes.Slice.t;
  src : Bytesrw.Bytes.Slice.t;
  ether_type : UInt16.t;
  ipv4 : ipv4 option;
}

val tcp : tcp Codec.t
(** TCP prefix and options, sized by its data offset. *)

val udp : udp Codec.t
(** UDP header and payload, sized by its length. *)

val ipv4 : ipv4 Codec.t
(** IPv4 header, options and a payload region bounded by total_length. *)

val codec : t Codec.t
(** Ethernet II with IPv4 selected by EtherType; other EtherTypes stop after the
    Ethernet header. Unknown IPv4 protocols and fragments retain an opaque
    payload. Ethernet padding after the IPv4 total length is accepted. *)

val pp : Format.formatter -> t -> unit
(** Display the selected network and transport layers. *)
