(** TCP/IP protocol header codecs using Wire's zero-copy [byte_slice] API.

    Demonstrates nested protocol parsing: Ethernet (14B) -> IPv4 (20B) -> TCP
    (20B) or UDP (8B)

    All payload fields use [byte_slice] for zero-copy sub-slicing. Individual
    header fields are accessed via [Codec.get] without allocating intermediate
    records. *)

open Wire
module Slice = Bytesrw.Bytes.Slice

(* -- Ethernet II frame header: 14 bytes + payload -- *)

let ethernet_payload_size = 40

type ethernet = {
  eth_dst : Slice.t;
  eth_src : Slice.t;
  eth_ethertype : int;
  eth_payload : Slice.t;
}

let f_eth_ethertype = Field.v "EtherType" uint16be

let f_eth_payload =
  Field.v "Payload" (byte_slice ~size:(int ethernet_payload_size))

let bf_eth_ethertype = Codec.(f_eth_ethertype $ fun e -> e.eth_ethertype)
let bf_eth_payload = Codec.(f_eth_payload $ fun e -> e.eth_payload)

let ethernet_codec =
  Codec.v "Ethernet"
    (fun dst src etype payload ->
      {
        eth_dst = dst;
        eth_src = src;
        eth_ethertype = etype;
        eth_payload = payload;
      })
    Codec.
      [
        (Field.v "DstMAC" (byte_slice ~size:(int 6)) $ fun e -> e.eth_dst);
        (Field.v "SrcMAC" (byte_slice ~size:(int 6)) $ fun e -> e.eth_src);
        bf_eth_ethertype;
        bf_eth_payload;
      ]

let ethernet_struct = Everparse.struct_of_codec ethernet_codec
let ethernet_size = Codec.wire_size ethernet_codec

(* -- IPv4 header: 20 bytes fixed (no options) + payload -- *)

let ipv4_payload_size = 20

type ipv4 = {
  ip_version : int;
  ip_ihl : int;
  ip_dscp : int;
  ip_ecn : int;
  ip_total_length : int;
  ip_identification : int;
  ip_flags : int;
  ip_fragment_offset : int;
  ip_ttl : int;
  ip_protocol : int;
  ip_checksum : int;
  ip_src : int;
  ip_dst : int;
  ip_payload : Slice.t;
}

let f_ip_protocol = Field.v "Protocol" uint8
let f_ip_src = Field.v "SrcAddr" uint32be
let f_ip_dst = Field.v "DstAddr" uint32be
let f_ip_payload = Field.v "Payload" (byte_slice ~size:(int ipv4_payload_size))
let bf_ip_protocol = Codec.(f_ip_protocol $ fun p -> p.ip_protocol)
let bf_ip_src = Codec.(f_ip_src $ fun p -> p.ip_src)
let bf_ip_dst = Codec.(f_ip_dst $ fun p -> p.ip_dst)
let bf_ip_payload = Codec.(f_ip_payload $ fun p -> p.ip_payload)

let ipv4_codec =
  Codec.v "IPv4"
    (fun version ihl dscp ecn total_length identification flags fragment_offset
         ttl protocol checksum src dst payload ->
      {
        ip_version = version;
        ip_ihl = ihl;
        ip_dscp = dscp;
        ip_ecn = ecn;
        ip_total_length = total_length;
        ip_identification = identification;
        ip_flags = flags;
        ip_fragment_offset = fragment_offset;
        ip_ttl = ttl;
        ip_protocol = protocol;
        ip_checksum = checksum;
        ip_src = src;
        ip_dst = dst;
        ip_payload = payload;
      })
    Codec.
      [
        (Field.v "Version" (bits ~width:4 U8) $ fun p -> p.ip_version);
        (Field.v "IHL" (bits ~width:4 U8) $ fun p -> p.ip_ihl);
        (Field.v "DSCP" (bits ~width:6 U8) $ fun p -> p.ip_dscp);
        (Field.v "ECN" (bits ~width:2 U8) $ fun p -> p.ip_ecn);
        (Field.v "TotalLength" uint16be $ fun p -> p.ip_total_length);
        (Field.v "Identification" uint16be $ fun p -> p.ip_identification);
        (Field.v "Flags" (bits ~width:3 U16be) $ fun p -> p.ip_flags);
        ( Field.v "FragmentOffset" (bits ~width:13 U16be) $ fun p ->
          p.ip_fragment_offset );
        (Field.v "TTL" uint8 $ fun p -> p.ip_ttl);
        bf_ip_protocol;
        (Field.v "HeaderChecksum" uint16be $ fun p -> p.ip_checksum);
        bf_ip_src;
        bf_ip_dst;
        bf_ip_payload;
      ]

let ipv4_struct = Everparse.struct_of_codec ipv4_codec
let ipv4_size = Codec.wire_size ipv4_codec

(* -- TCP header: 20 bytes fixed (no options) -- *)

type tcp = {
  tcp_src_port : int;
  tcp_dst_port : int;
  tcp_seq : int;
  tcp_ack_num : int;
  tcp_data_offset : int;
  tcp_reserved : int;
  tcp_ns : bool;
  tcp_cwr : bool;
  tcp_ece : bool;
  tcp_urg : bool;
  tcp_ack : bool;
  tcp_psh : bool;
  tcp_rst : bool;
  tcp_syn : bool;
  tcp_fin : bool;
  tcp_window : int;
  tcp_checksum : int;
  tcp_urgent_ptr : int;
}

let f_tcp_src_port = Field.v "SrcPort" uint16be
let f_tcp_dst_port = Field.v "DstPort" uint16be
let f_tcp_syn = Field.v "SYN" (bit (bits ~width:1 U16be))
let f_tcp_ack = Field.v "ACK" (bit (bits ~width:1 U16be))

let tcp_of_fields src_port dst_port seq ack_num data_offset reserved ns cwr ece
    urg ack psh rst syn fin window checksum urgent_ptr =
  {
    tcp_src_port = src_port;
    tcp_dst_port = dst_port;
    tcp_seq = seq;
    tcp_ack_num = ack_num;
    tcp_data_offset = data_offset;
    tcp_reserved = reserved;
    tcp_ns = ns;
    tcp_cwr = cwr;
    tcp_ece = ece;
    tcp_urg = urg;
    tcp_ack = ack;
    tcp_psh = psh;
    tcp_rst = rst;
    tcp_syn = syn;
    tcp_fin = fin;
    tcp_window = window;
    tcp_checksum = checksum;
    tcp_urgent_ptr = urgent_ptr;
  }

let bf_tcp_src_port = Codec.(f_tcp_src_port $ fun t -> t.tcp_src_port)
let bf_tcp_dst_port = Codec.(f_tcp_dst_port $ fun t -> t.tcp_dst_port)
let bf_tcp_syn = Codec.(f_tcp_syn $ fun t -> t.tcp_syn)
let bf_tcp_ack = Codec.(f_tcp_ack $ fun t -> t.tcp_ack)

let tcp_codec =
  Codec.v "TCP" tcp_of_fields
    Codec.
      [
        bf_tcp_src_port;
        bf_tcp_dst_port;
        (Field.v "SeqNum" uint32be $ fun t -> t.tcp_seq);
        (Field.v "AckNum" uint32be $ fun t -> t.tcp_ack_num);
        (Field.v "DataOffset" (bits ~width:4 U16be) $ fun t -> t.tcp_data_offset);
        (Field.v "Reserved" (bits ~width:3 U16be) $ fun t -> t.tcp_reserved);
        (Field.v "NS" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_ns);
        (Field.v "CWR" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_cwr);
        (Field.v "ECE" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_ece);
        (Field.v "URG" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_urg);
        bf_tcp_ack;
        (Field.v "PSH" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_psh);
        (Field.v "RST" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_rst);
        bf_tcp_syn;
        (Field.v "FIN" (bit (bits ~width:1 U16be)) $ fun t -> t.tcp_fin);
        (Field.v "Window" uint16be $ fun t -> t.tcp_window);
        (Field.v "Checksum" uint16be $ fun t -> t.tcp_checksum);
        (Field.v "UrgentPtr" uint16be $ fun t -> t.tcp_urgent_ptr);
      ]

let tcp_struct = Everparse.struct_of_codec tcp_codec
let tcp_size = Codec.wire_size tcp_codec

(* -- UDP header: 8 bytes -- *)

type udp = {
  udp_src_port : int;
  udp_dst_port : int;
  udp_length : int;
  udp_checksum : int;
}

let f_udp_src_port = Field.v "SrcPort" uint16be
let f_udp_dst_port = Field.v "DstPort" uint16be
let f_udp_length = Field.v "Length" uint16be
let f_udp_checksum = Field.v "Checksum" uint16be

let udp_codec =
  Codec.v "UDP"
    (fun src_port dst_port length checksum ->
      {
        udp_src_port = src_port;
        udp_dst_port = dst_port;
        udp_length = length;
        udp_checksum = checksum;
      })
    Codec.
      [
        (f_udp_src_port $ fun u -> u.udp_src_port);
        (f_udp_dst_port $ fun u -> u.udp_dst_port);
        (f_udp_length $ fun u -> u.udp_length);
        (f_udp_checksum $ fun u -> u.udp_checksum);
      ]

let udp_struct = Everparse.struct_of_codec udp_codec
let udp_size = Codec.wire_size udp_codec

(* -- Utilities -- *)

let pp_ipv4_addr ppf addr =
  Fmt.pf ppf "%d.%d.%d.%d"
    ((addr lsr 24) land 0xFF)
    ((addr lsr 16) land 0xFF)
    ((addr lsr 8) land 0xFF)
    (addr land 0xFF)

let ipv4_addr a b c d = (a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d

(* -- Data generators -- *)

let tcp_frame_data n =
  Array.init n (fun i ->
      let b = Bytes.create ethernet_size in
      (* Ethernet: dst MAC *)
      Bytes.set_uint8 b 0 0x00;
      Bytes.set_uint8 b 1 0x11;
      Bytes.set_uint8 b 2 0x22;
      Bytes.set_uint8 b 3 0x33;
      Bytes.set_uint8 b 4 0x44;
      Bytes.set_uint8 b 5 0x55;
      (* Ethernet: src MAC *)
      Bytes.set_uint8 b 6 0xAA;
      Bytes.set_uint8 b 7 0xBB;
      Bytes.set_uint8 b 8 0xCC;
      Bytes.set_uint8 b 9 0xDD;
      Bytes.set_uint8 b 10 0xEE;
      Bytes.set_uint8 b 11 0xFF;
      (* EtherType: IPv4 *)
      Bytes.set_uint16_be b 12 0x0800;
      (* IPv4 at offset 14 *)
      let ip = 14 in
      Bytes.set_uint8 b ip 0x45;
      Bytes.set_uint8 b (ip + 1) 0x00;
      Bytes.set_uint16_be b (ip + 2) 40;
      Bytes.set_uint16_be b (ip + 4) (i mod 65536);
      Bytes.set_uint16_be b (ip + 6) 0x4000;
      Bytes.set_uint8 b (ip + 8) 64;
      Bytes.set_uint8 b (ip + 9) 6;
      Bytes.set_uint16_be b (ip + 10) 0;
      Bytes.set_int32_be b (ip + 12) (Int32.of_int (ipv4_addr 192 168 1 100));
      Bytes.set_int32_be b (ip + 16) (Int32.of_int (ipv4_addr 10 0 0 1));
      (* TCP at offset 34 *)
      let tcp = 34 in
      Bytes.set_uint16_be b tcp (12345 + (i mod 1000));
      Bytes.set_uint16_be b (tcp + 2) 443;
      Bytes.set_int32_be b (tcp + 4) (Int32.of_int (i * 1000));
      Bytes.set_int32_be b (tcp + 8) 0l;
      Bytes.set_uint16_be b (tcp + 12) 0x5002;
      Bytes.set_uint16_be b (tcp + 14) 65535;
      Bytes.set_uint16_be b (tcp + 16) 0;
      Bytes.set_uint16_be b (tcp + 18) 0;
      b)

let udp_frame_data n =
  Array.init n (fun i ->
      let b = Bytes.create ethernet_size in
      Bytes.set_uint8 b 0 0x00;
      Bytes.set_uint8 b 1 0x11;
      Bytes.set_uint8 b 2 0x22;
      Bytes.set_uint8 b 3 0x33;
      Bytes.set_uint8 b 4 0x44;
      Bytes.set_uint8 b 5 0x55;
      Bytes.set_uint8 b 6 0xAA;
      Bytes.set_uint8 b 7 0xBB;
      Bytes.set_uint8 b 8 0xCC;
      Bytes.set_uint8 b 9 0xDD;
      Bytes.set_uint8 b 10 0xEE;
      Bytes.set_uint8 b 11 0xFF;
      Bytes.set_uint16_be b 12 0x0800;
      let ip = 14 in
      Bytes.set_uint8 b ip 0x45;
      Bytes.set_uint8 b (ip + 1) 0x00;
      Bytes.set_uint16_be b (ip + 2) 28;
      Bytes.set_uint16_be b (ip + 4) (i mod 65536);
      Bytes.set_uint16_be b (ip + 6) 0x4000;
      Bytes.set_uint8 b (ip + 8) 64;
      Bytes.set_uint8 b (ip + 9) 17;
      Bytes.set_uint16_be b (ip + 10) 0;
      Bytes.set_int32_be b (ip + 12) (Int32.of_int (ipv4_addr 192 168 1 100));
      Bytes.set_int32_be b (ip + 16) (Int32.of_int (ipv4_addr 10 0 0 1));
      let udp = 34 in
      Bytes.set_uint16_be b udp (5353 + (i mod 100));
      Bytes.set_uint16_be b (udp + 2) 53;
      Bytes.set_uint16_be b (udp + 4) 8;
      Bytes.set_uint16_be b (udp + 6) 0;
      b)

(* -- Schema registry (for EverParse benchmark infrastructure) -- *)

type 'a schema = {
  name : string;
  codec : 'a Codec.t;
  struct_ : Wire.Everparse.Raw.struct_;
  size : int;
  decode : bytes -> int -> ('a, Wire.parse_error) result;
}

let schema name codec struct_ size =
  { name; codec; struct_; size; decode = Codec.decode codec }

type any_schema = Any : 'a schema -> any_schema

let all_schemas =
  [
    Any (schema "Ethernet" ethernet_codec ethernet_struct ethernet_size);
    Any (schema "IPv4" ipv4_codec ipv4_struct ipv4_size);
    Any (schema "TCP" tcp_codec tcp_struct tcp_size);
    Any (schema "UDP" udp_codec udp_struct udp_size);
  ]

let all_structs = List.map (fun (Any s) -> s.struct_) all_schemas
