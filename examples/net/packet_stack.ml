open Wire
(** Ethernet / IPv4 / TCP or UDP, with every layer bounded by its parent.
    Fragmented IPv4 packets deliberately have no transport header. *)

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

let data_offset =
  Field.v "data_offset" (bits ~width:4 U16be) ~self_constraint:(fun x ->
      Expr.(x >= int 5))

let tcp =
  Codec.v "StackTCP"
    (fun src_port dst_port seq ack data_offset reserved flags window checksum
         urgent options ->
      {
        src_port;
        dst_port;
        seq;
        ack;
        data_offset;
        reserved;
        flags;
        window;
        checksum;
        urgent;
        options;
      })
    Codec.
      [
        (Field.v "src_port" uint16be $ fun r -> r.src_port);
        (Field.v "dst_port" uint16be $ fun r -> r.dst_port);
        (Field.v "seq" uint32be $ fun r -> r.seq);
        (Field.v "ack" uint32be $ fun r -> r.ack);
        (data_offset $ fun r -> r.data_offset);
        (Field.v "reserved" (bits ~width:3 U16be) $ fun r -> r.reserved);
        (Field.v "flags" (bits ~width:9 U16be) $ fun r -> r.flags);
        (Field.v "window" uint16be $ fun r -> r.window);
        (Field.v "checksum" uint16be $ fun r -> r.checksum);
        (Field.v "urgent" uint16be $ fun r -> r.urgent);
        ( Field.v "options"
            (byte_slice ~size:Expr.((Field.ref data_offset lsl int 2) - int 20))
        $ fun r -> r.options );
      ]

type udp = {
  src_port : UInt16.t;
  dst_port : UInt16.t;
  length : UInt16.t;
  checksum : UInt16.t;
  payload : Bytesrw.Bytes.Slice.t;
}

let udp_length =
  Field.v "length" uint16be ~self_constraint:(fun x -> Expr.(x >= int 8))

let udp =
  Codec.v "StackUDP"
    (fun src_port dst_port length checksum payload ->
      { src_port; dst_port; length; checksum; payload })
    Codec.
      [
        (Field.v "src_port" uint16be $ fun r -> r.src_port);
        (Field.v "dst_port" uint16be $ fun r -> r.dst_port);
        (udp_length $ fun r -> r.length);
        (Field.v "checksum" uint16be $ fun r -> r.checksum);
        ( Field.v "payload"
            (byte_slice ~size:Expr.(Field.ref udp_length - int 8))
        $ fun r -> r.payload );
      ]

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

let ihl =
  Field.v "ihl" (bits ~width:4 U8) ~self_constraint:(fun x -> Expr.(x >= int 5))

let total_length =
  Field.v "total_length" uint16be ~self_constraint:(fun x ->
      Expr.(x >= Field.ref ihl lsl int 2))

let flags = Field.v "flags" (bits ~width:3 U16be)
let fragment_offset = Field.v "fragment_offset" (bits ~width:13 U16be)
let protocol = Field.v "protocol" uint8
let payload_size = Expr.(Field.ref total_length - (Field.ref ihl lsl int 2))

let unfragmented =
  Expr.(Field.ref flags land int 1 = int 0 && Field.ref fragment_offset = int 0)

let is_tcp = Expr.(unfragmented && Field.ref protocol = int 6)
let is_udp = Expr.(unfragmented && Field.ref protocol = int 17)

let ipv4_of_fields version ihl dscp ecn total_length identification flags
    fragment_offset ttl protocol checksum src dst options tcp udp opaque =
  {
    version;
    ihl;
    dscp;
    ecn;
    total_length;
    identification;
    flags;
    fragment_offset;
    ttl;
    protocol;
    checksum;
    src;
    dst;
    options;
    tcp;
    udp;
    opaque;
  }

let ipv4 =
  Codec.v "StackIPv4" ipv4_of_fields
    Codec.
      [
        ( Field.v "version" (bits ~width:4 U8) ~self_constraint:(fun x ->
              Expr.(x = int 4))
        $ fun r -> r.version );
        (ihl $ fun r -> r.ihl);
        (Field.v "dscp" (bits ~width:6 U8) $ fun r -> r.dscp);
        (Field.v "ecn" (bits ~width:2 U8) $ fun r -> r.ecn);
        (total_length $ fun r -> r.total_length);
        (Field.v "identification" uint16be $ fun r -> r.identification);
        (flags $ fun r -> r.flags);
        (fragment_offset $ fun r -> r.fragment_offset);
        (Field.v "ttl" uint8 $ fun r -> r.ttl);
        (protocol $ fun r -> r.protocol);
        (Field.v "checksum" uint16be $ fun r -> r.checksum);
        (Field.v "src" uint32be $ fun r -> r.src);
        (Field.v "dst" uint32be $ fun r -> r.dst);
        ( Field.v "options"
            (byte_slice ~size:Expr.((Field.ref ihl lsl int 2) - int 20))
        $ fun r -> r.options );
        ( Field.optional "tcp" ~present:is_tcp
            (nested_at_most ~size:payload_size (codec tcp))
        $ fun r -> r.tcp );
        ( Field.optional "udp" ~present:is_udp
            (nested_at_most ~size:payload_size (codec udp))
        $ fun r -> r.udp );
        ( Field.optional "opaque"
            ~present:Expr.(not (is_tcp || is_udp))
            (byte_slice ~size:payload_size)
        $ fun r -> r.opaque );
      ]

type t = {
  dst : Bytesrw.Bytes.Slice.t;
  src : Bytesrw.Bytes.Slice.t;
  ether_type : UInt16.t;
  ipv4 : ipv4 option;
}

let ether_type = Field.v "ether_type" uint16be

let codec =
  Codec.v "EthernetStack"
    (fun dst src ether_type ipv4 -> { dst; src; ether_type; ipv4 })
    Codec.
      [
        (Field.v "dst" (byte_slice ~size:(int 6)) $ fun r -> r.dst);
        (Field.v "src" (byte_slice ~size:(int 6)) $ fun r -> r.src);
        (ether_type $ fun r -> r.ether_type);
        ( Field.optional "ipv4"
            ~present:Expr.(Field.ref ether_type = int 0x0800)
            (codec ipv4)
        $ fun r -> r.ipv4 );
      ]

let pp ppf (v : t) =
  Fmt.pf ppf "Ethernet type=0x%04x" (UInt16.to_int v.ether_type);
  Option.iter
    (fun (ip : ipv4) ->
      Fmt.pf ppf " IPv4 protocol=%d length=%d" (UInt8.to_int ip.protocol)
        (UInt16.to_int ip.total_length);
      Option.iter
        (fun (tcp : tcp) ->
          Fmt.pf ppf " TCP %d -> %d"
            (UInt16.to_int tcp.src_port)
            (UInt16.to_int tcp.dst_port))
        ip.tcp;
      Option.iter
        (fun (udp : udp) ->
          Fmt.pf ppf " UDP %d -> %d"
            (UInt16.to_int udp.src_port)
            (UInt16.to_int udp.dst_port))
        ip.udp)
    v.ipv4

(* {1 Captures}

   A capture keeps the first bytes of each packet: MAWI's keep the headers and
   drop the payloads, so the IPv4 total length, the IPv6 payload length and the
   UDP length usually run past the bytes captured. [capture] reads the same
   headers bounded by the capture instead: each length field is read, and must
   cover its header where it counts one, but bounds no region. *)

type udp_header = {
  src_port : UInt16.t;
  dst_port : UInt16.t;
  length : UInt16.t;
  checksum : UInt16.t;
}

let udp_header =
  Codec.v "StackUDPHeader"
    (fun src_port dst_port length checksum ->
      { src_port; dst_port; length; checksum })
    Codec.
      [
        (Field.v "src_port" uint16be $ fun (r : udp_header) -> r.src_port);
        (Field.v "dst_port" uint16be $ fun (r : udp_header) -> r.dst_port);
        (udp_length $ fun (r : udp_header) -> r.length);
        (Field.v "checksum" uint16be $ fun (r : udp_header) -> r.checksum);
      ]

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

let captured_ipv4 =
  Codec.v "CapturedIPv4"
    (fun version ihl dscp ecn total_length identification flags fragment_offset
         ttl protocol checksum src dst options tcp udp ->
      {
        version;
        ihl;
        dscp;
        ecn;
        total_length;
        identification;
        flags;
        fragment_offset;
        ttl;
        protocol;
        checksum;
        src;
        dst;
        options;
        tcp;
        udp;
      })
    Codec.
      [
        ( Field.v "version" (bits ~width:4 U8) ~self_constraint:(fun x ->
              Expr.(x = int 4))
        $ fun (r : captured_ipv4) -> r.version );
        (ihl $ fun (r : captured_ipv4) -> r.ihl);
        (Field.v "dscp" (bits ~width:6 U8) $ fun (r : captured_ipv4) -> r.dscp);
        (Field.v "ecn" (bits ~width:2 U8) $ fun (r : captured_ipv4) -> r.ecn);
        (total_length $ fun (r : captured_ipv4) -> r.total_length);
        ( Field.v "identification" uint16be $ fun (r : captured_ipv4) ->
          r.identification );
        (flags $ fun (r : captured_ipv4) -> r.flags);
        (fragment_offset $ fun (r : captured_ipv4) -> r.fragment_offset);
        (Field.v "ttl" uint8 $ fun (r : captured_ipv4) -> r.ttl);
        (protocol $ fun (r : captured_ipv4) -> r.protocol);
        (Field.v "checksum" uint16be $ fun (r : captured_ipv4) -> r.checksum);
        (Field.v "src" uint32be $ fun (r : captured_ipv4) -> r.src);
        (Field.v "dst" uint32be $ fun (r : captured_ipv4) -> r.dst);
        ( Field.v "options"
            (byte_slice ~size:Expr.((Field.ref ihl lsl int 2) - int 20))
        $ fun (r : captured_ipv4) -> r.options );
        ( Field.optional "tcp" ~present:is_tcp (Wire.codec tcp)
        $ fun (r : captured_ipv4) -> r.tcp );
        ( Field.optional "udp" ~present:is_udp (Wire.codec udp_header)
        $ fun (r : captured_ipv4) -> r.udp );
      ]

(* The fixed header of nox-ip's [Ip.V6.Header.codec] (RFC 8200), with each
   address as four 32-bit words. *)
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

let next_header = Field.v "next_header" uint8

let ipv6_of_fields version traffic_class flow_label payload_length next_header
    hop_limit src0 src1 src2 src3 dst0 dst1 dst2 dst3 tcp udp =
  {
    version;
    traffic_class;
    flow_label;
    payload_length;
    next_header;
    hop_limit;
    src0;
    src1;
    src2;
    src3;
    dst0;
    dst1;
    dst2;
    dst3;
    tcp;
    udp;
  }

let word name (of_ipv6 : ipv6 -> UInt32.t) =
  Codec.(Field.v name uint32be $ of_ipv6)

let ipv6 =
  Codec.v "StackIPv6" ipv6_of_fields
    Codec.
      [
        ( Field.v "version" (bits ~width:4 U32be) ~self_constraint:(fun x ->
              Expr.(x = int 6))
        $ fun r -> r.version );
        ( Field.v "traffic_class" (bits ~width:8 U32be) $ fun r ->
          r.traffic_class );
        (Field.v "flow_label" (bits ~width:20 U32be) $ fun r -> r.flow_label);
        (Field.v "payload_length" uint16be $ fun r -> r.payload_length);
        (next_header $ fun r -> r.next_header);
        (Field.v "hop_limit" uint8 $ fun r -> r.hop_limit);
        word "src0" (fun r -> r.src0);
        word "src1" (fun r -> r.src1);
        word "src2" (fun r -> r.src2);
        word "src3" (fun r -> r.src3);
        word "dst0" (fun r -> r.dst0);
        word "dst1" (fun r -> r.dst1);
        word "dst2" (fun r -> r.dst2);
        word "dst3" (fun r -> r.dst3);
        ( Field.optional "tcp"
            ~present:Expr.(Field.ref next_header = int 6)
            (Wire.codec tcp)
        $ fun r -> r.tcp );
        ( Field.optional "udp"
            ~present:Expr.(Field.ref next_header = int 17)
            (Wire.codec udp_header)
        $ fun r -> r.udp );
      ]

type capture = {
  dst : Bytesrw.Bytes.Slice.t;
  src : Bytesrw.Bytes.Slice.t;
  ether_type : UInt16.t;
  ipv4 : captured_ipv4 option;
  ipv6 : ipv6 option;
}

let capture =
  Codec.v "CapturedStack"
    (fun dst src ether_type ipv4 ipv6 -> { dst; src; ether_type; ipv4; ipv6 })
    Codec.
      [
        (Field.v "dst" (byte_slice ~size:(int 6)) $ fun (r : capture) -> r.dst);
        (Field.v "src" (byte_slice ~size:(int 6)) $ fun (r : capture) -> r.src);
        (ether_type $ fun (r : capture) -> r.ether_type);
        ( Field.optional "ipv4"
            ~present:Expr.(Field.ref ether_type = int 0x0800)
            (Wire.codec captured_ipv4)
        $ fun (r : capture) -> r.ipv4 );
        ( Field.optional "ipv6"
            ~present:Expr.(Field.ref ether_type = int 0x86dd)
            (Wire.codec ipv6)
        $ fun (r : capture) -> r.ipv6 );
      ]
