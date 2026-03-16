(** TCP/IP protocol header codecs using Wire's zero-copy API.

    Ethernet (14B) -> IPv4 (20B) -> TCP (20B) or UDP (8B). All payload fields
    use [byte_slice] for zero-copy sub-slicing. *)

(** {2 Ethernet II} *)

type ethernet

val ethernet_codec : ethernet Wire.Codec.t
(** Codec for the 14-byte Ethernet II frame header including a zero-copy payload
    slice. *)

val ethernet_struct : Wire.struct_
(** Wire struct descriptor for EverParse code generation. *)

val ethernet_size : int
(** Wire size of an Ethernet II frame in bytes. *)

val ethernet_payload_size : int
(** Fixed payload size carried inside an Ethernet frame in bytes. *)

val f_eth_ethertype : (int, ethernet) Wire.Codec.field
(** Zero-copy field accessor for the Ethernet EtherType field. *)

val f_eth_payload : (Bytesrw.Bytes.Slice.t, ethernet) Wire.Codec.field
(** Zero-copy field accessor for the Ethernet payload byte slice. *)

(** {2 IPv4} *)

type ipv4

val ipv4_codec : ipv4 Wire.Codec.t
(** Codec for the 20-byte IPv4 header including a zero-copy payload slice. *)

val ipv4_struct : Wire.struct_
(** Wire struct descriptor for EverParse code generation. *)

val ipv4_size : int
(** Wire size of an IPv4 header in bytes. *)

val ipv4_payload_size : int
(** Fixed payload size carried inside an IPv4 packet in bytes. *)

val f_ip_protocol : (int, ipv4) Wire.Codec.field
(** Zero-copy field accessor for the IPv4 Protocol field. *)

val f_ip_src : (int, ipv4) Wire.Codec.field
(** Zero-copy field accessor for the IPv4 source address field. *)

val f_ip_dst : (int, ipv4) Wire.Codec.field
(** Zero-copy field accessor for the IPv4 destination address field. *)

val f_ip_payload : (Bytesrw.Bytes.Slice.t, ipv4) Wire.Codec.field
(** Zero-copy field accessor for the IPv4 payload byte slice. *)

(** {2 TCP} *)

type tcp

val tcp_codec : tcp Wire.Codec.t
(** Codec for the 20-byte TCP header. *)

val tcp_struct : Wire.struct_
(** Wire struct descriptor for EverParse code generation. *)

val tcp_size : int
(** Wire size of a TCP header in bytes. *)

val f_tcp_src_port : (int, tcp) Wire.Codec.field
(** Zero-copy field accessor for the TCP source port field. *)

val f_tcp_dst_port : (int, tcp) Wire.Codec.field
(** Zero-copy field accessor for the TCP destination port field. *)

val f_tcp_syn : (bool, tcp) Wire.Codec.field
(** Zero-copy field accessor for the TCP SYN flag. *)

val f_tcp_ack : (bool, tcp) Wire.Codec.field
(** Zero-copy field accessor for the TCP ACK flag. *)

(** {2 UDP} *)

type udp

val udp_codec : udp Wire.Codec.t
(** Codec for the 8-byte UDP header. *)

val udp_struct : Wire.struct_
(** Wire struct descriptor for EverParse code generation. *)

val udp_size : int
(** Wire size of a UDP header in bytes. *)

val f_udp_src_port : (int, udp) Wire.Codec.field
(** Zero-copy field accessor for the UDP source port field. *)

val f_udp_dst_port : (int, udp) Wire.Codec.field
(** Zero-copy field accessor for the UDP destination port field. *)

val f_udp_length : (int, udp) Wire.Codec.field
(** Zero-copy field accessor for the UDP length field. *)

(** {2 Utilities} *)

val pp_ipv4_addr : Format.formatter -> int -> unit
(** Pretty-printer for a packed 32-bit IPv4 address in dotted-decimal notation.
*)

val ipv4_addr : int -> int -> int -> int -> int
(** [ipv4_addr a b c d] packs four octets into a single 32-bit IPv4 address. *)

(** {2 Data generators} *)

val tcp_frame_data : int -> bytes array
(** [tcp_frame_data n] generates [n] synthetic Ethernet/IPv4/TCP byte buffers.
*)

val udp_frame_data : int -> bytes array
(** [udp_frame_data n] generates [n] synthetic Ethernet/IPv4/UDP byte buffers.
*)

(** {2 Schema registry} *)

type 'a schema = {
  name : string;
  codec : 'a Wire.Codec.t;
  struct_ : Wire.struct_;
  size : int;
  decode : bytes -> int -> 'a;
}

type any_schema = Any : 'a schema -> any_schema

val all_schemas : any_schema list
(** List of all protocol schemas (Ethernet, IPv4, TCP, UDP) as existential
    wrappers. *)

val all_structs : Wire.struct_ list
(** List of Wire struct descriptors for all protocols, used for EverParse code
    generation. *)
