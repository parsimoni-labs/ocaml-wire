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

(** {1 Captures}

    A capture keeps the first bytes of each packet, and a header-only one such
    as a MAWI trace leaves the IPv4 total length, the IPv6 payload length and
    the UDP length running past them. {!capture} bounds every layer by the bytes
    captured: each length field is read, and must cover its header where it
    counts one, but bounds no region. *)

type udp_header = {
  src_port : UInt16.t;
  dst_port : UInt16.t;
  length : UInt16.t;
  checksum : UInt16.t;
}

val udp_header : udp_header Codec.t
(** The UDP header, with a length of at least 8. *)

type captured_ipv4 = {
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
  udp : udp_header option;
}

val captured_ipv4 : captured_ipv4 Codec.t
(** The IPv4 header and options, then the TCP or UDP header of a packet that is
    not a fragment. The total length covers the header and bounds nothing. *)

type ipv6 = {
  version : int;
  traffic_class : int;
  flow_label : int;
  payload_length : UInt16.t;
  next_header : UInt8.t;
  hop_limit : UInt8.t;
  src0 : UInt32.t;
  src1 : UInt32.t;
  src2 : UInt32.t;
  src3 : UInt32.t;
  dst0 : UInt32.t;
  dst1 : UInt32.t;
  dst2 : UInt32.t;
  dst3 : UInt32.t;
  tcp : tcp option;
  udp : udp_header option;
}

val ipv6 : ipv6 Codec.t
(** The fixed IPv6 header (RFC 8200), each address as four 32-bit words from the
    most significant, then the TCP or UDP header its next header names. An
    extension header ends the walk: no transport header is read. *)

type capture = {
  dst : Bytesrw.Bytes.Slice.t;
  src : Bytesrw.Bytes.Slice.t;
  ether_type : UInt16.t;
  ipv4 : captured_ipv4 option;
  ipv6 : ipv6 option;
}

val capture : capture Codec.t
(** Ethernet II with IPv4 or IPv6 selected by EtherType, each layer bounded by
    the bytes captured. Bytes after the transport header are accepted. *)
