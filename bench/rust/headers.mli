(** Ethernet, IPv4, TCP and UDP headers, without options or payload, as the
    cross-language benchmark reads them. Each codec checks what a packet parser
    checks of a header: an IPv4 version of 4, header lengths that fit the fixed
    header, lengths that cover the header. *)

open Wire

type eth = string * string * UInt16.t

val eth : eth Codec.t
(** [eth] is an Ethernet II header: two MAC addresses and the EtherType. *)

val eth_type : (UInt16.t, eth) Codec.field
(** [eth_type] is the EtherType of {!val-eth}. *)

type ipv4 =
  int
  * int
  * int
  * int
  * UInt16.t
  * UInt16.t
  * int
  * int
  * UInt8.t
  * UInt8.t
  * UInt16.t
  * UInt32.t
  * UInt32.t

val ipv4 : ipv4 Codec.t
(** [ipv4] is an IPv4 header without options (RFC 791). *)

val ipv4_ihl : (int, ipv4) Codec.field
(** [ipv4_ihl] is the header length of {!val-ipv4}, in 32-bit words. *)

val ipv4_flags : (int, ipv4) Codec.field
(** [ipv4_flags] are the flags of {!val-ipv4}: reserved, don't fragment, more
    fragments. *)

val ipv4_fragment_offset : (int, ipv4) Codec.field
(** [ipv4_fragment_offset] is the fragment offset of {!val-ipv4}. *)

val ipv4_total_length : (UInt16.t, ipv4) Codec.field
(** [ipv4_total_length] is the total length of {!val-ipv4}. *)

val ipv4_ttl : (UInt8.t, ipv4) Codec.field
(** [ipv4_ttl] is the time to live of {!val-ipv4}. *)

val ipv4_protocol : (UInt8.t, ipv4) Codec.field
(** [ipv4_protocol] is the next protocol of {!val-ipv4}. *)

val ipv4_src : (UInt32.t, ipv4) Codec.field
(** [ipv4_src] is the source address of {!val-ipv4}. *)

val ipv4_dst : (UInt32.t, ipv4) Codec.field
(** [ipv4_dst] is the destination address of {!val-ipv4}. *)

type tcp =
  UInt16.t
  * UInt16.t
  * UInt32.t
  * UInt32.t
  * int
  * int
  * int
  * UInt16.t
  * UInt16.t
  * UInt16.t

val tcp : tcp Codec.t
(** [tcp] is a TCP header without options (RFC 9293). *)

val tcp_src_port : (UInt16.t, tcp) Codec.field
(** [tcp_src_port] is the source port of {!val-tcp}. *)

val tcp_dst_port : (UInt16.t, tcp) Codec.field
(** [tcp_dst_port] is the destination port of {!val-tcp}. *)

val tcp_seq : (UInt32.t, tcp) Codec.field
(** [tcp_seq] is the sequence number of {!val-tcp}. *)

val tcp_ack : (UInt32.t, tcp) Codec.field
(** [tcp_ack] is the acknowledgment number of {!val-tcp}. *)

val tcp_data_offset : (int, tcp) Codec.field
(** [tcp_data_offset] is the header length in words of {!val-tcp}. *)

val tcp_flags : (int, tcp) Codec.field
(** [tcp_flags] is the nine control bits of {!val-tcp}, NS to FIN. *)

val tcp_window : (UInt16.t, tcp) Codec.field
(** [tcp_window] is the window of {!val-tcp}. *)

type udp = UInt16.t * UInt16.t * UInt16.t * UInt16.t

val udp : udp Codec.t
(** [udp] is a UDP header (RFC 768). *)

val udp_src_port : (UInt16.t, udp) Codec.field
(** [udp_src_port] is the source port of {!val-udp}. *)

val udp_dst_port : (UInt16.t, udp) Codec.field
(** [udp_dst_port] is the destination port of {!val-udp}. *)

val udp_length : (UInt16.t, udp) Codec.field
(** [udp_length] is the length of {!val-udp}. *)
