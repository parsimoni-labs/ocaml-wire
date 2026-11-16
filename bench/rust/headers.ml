open Wire

type eth = string * string * UInt16.t

let eth_type = Codec.(Field.v "ether_type" uint16be $ fun (_, _, x) -> x)

let eth =
  Codec.v "Ethernet"
    (fun dst src ty -> (dst, src, ty))
    Codec.
      [
        (Field.v "dst" (byte_array ~size:(int 6)) $ fun (x, _, _) -> x);
        (Field.v "src" (byte_array ~size:(int 6)) $ fun (_, x, _) -> x);
        eth_type;
      ]

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

let ipv4_ihl =
  Codec.(
    Field.v "ihl" (bits ~width:4 U8) ~self_constraint:(fun self ->
        Expr.(self >= int 5))
    $ fun (_, x, _, _, _, _, _, _, _, _, _, _, _) -> x)

let ipv4_flags =
  Codec.(
    Field.v "flags" (bits ~width:3 U16be)
    $ fun (_, _, _, _, _, _, x, _, _, _, _, _, _) -> x)

let ipv4_fragment_offset =
  Codec.(
    Field.v "fragment_offset" (bits ~width:13 U16be)
    $ fun (_, _, _, _, _, _, _, x, _, _, _, _, _) -> x)

let ipv4_total_length =
  Codec.(
    Field.v "total_length" uint16be ~self_constraint:(fun self ->
        Expr.(self >= int 20))
    $ fun (_, _, _, _, x, _, _, _, _, _, _, _, _) -> x)

let ipv4_ttl =
  Codec.(Field.v "ttl" uint8 $ fun (_, _, _, _, _, _, _, _, x, _, _, _, _) -> x)

let ipv4_protocol =
  Codec.(
    Field.v "protocol" uint8 $ fun (_, _, _, _, _, _, _, _, _, x, _, _, _) -> x)

let ipv4_src =
  Codec.(
    Field.v "src_addr" uint32be $ fun (_, _, _, _, _, _, _, _, _, _, _, x, _) ->
    x)

let ipv4_dst =
  Codec.(
    Field.v "dst_addr" uint32be $ fun (_, _, _, _, _, _, _, _, _, _, _, _, x) ->
    x)

let ipv4 =
  Codec.v "IPv4"
    (fun v ihl dscp ecn len id flags frag ttl proto sum src dst ->
      (v, ihl, dscp, ecn, len, id, flags, frag, ttl, proto, sum, src, dst))
    Codec.
      [
        ( Field.v "version" (bits ~width:4 U8) ~self_constraint:(fun self ->
              Expr.(self = int 4))
        $ fun (x, _, _, _, _, _, _, _, _, _, _, _, _) -> x );
        ipv4_ihl;
        ( Field.v "dscp" (bits ~width:6 U8)
        $ fun (_, _, x, _, _, _, _, _, _, _, _, _, _) -> x );
        ( Field.v "ecn" (bits ~width:2 U8)
        $ fun (_, _, _, x, _, _, _, _, _, _, _, _, _) -> x );
        ipv4_total_length;
        ( Field.v "identification" uint16be
        $ fun (_, _, _, _, _, x, _, _, _, _, _, _, _) -> x );
        ipv4_flags;
        ipv4_fragment_offset;
        ipv4_ttl;
        ipv4_protocol;
        ( Field.v "header_checksum" uint16be
        $ fun (_, _, _, _, _, _, _, _, _, _, x, _, _) -> x );
        ipv4_src;
        ipv4_dst;
      ]

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

let tcp_src_port =
  Codec.(Field.v "src_port" uint16be $ fun (x, _, _, _, _, _, _, _, _, _) -> x)

let tcp_dst_port =
  Codec.(Field.v "dst_port" uint16be $ fun (_, x, _, _, _, _, _, _, _, _) -> x)

let tcp_seq =
  Codec.(Field.v "seq" uint32be $ fun (_, _, x, _, _, _, _, _, _, _) -> x)

let tcp_ack =
  Codec.(Field.v "ack" uint32be $ fun (_, _, _, x, _, _, _, _, _, _) -> x)

let tcp_data_offset =
  Codec.(
    Field.v "data_offset" (bits ~width:4 U16be) ~self_constraint:(fun self ->
        Expr.(self >= int 5))
    $ fun (_, _, _, _, x, _, _, _, _, _) -> x)

let tcp_flags =
  Codec.(
    Field.v "flags" (bits ~width:9 U16be)
    $ fun (_, _, _, _, _, _, x, _, _, _) -> x)

let tcp_window =
  Codec.(Field.v "window" uint16be $ fun (_, _, _, _, _, _, _, x, _, _) -> x)

let tcp =
  Codec.v "TCP"
    (fun sp dp seq ack off res flags win sum urg ->
      (sp, dp, seq, ack, off, res, flags, win, sum, urg))
    Codec.
      [
        tcp_src_port;
        tcp_dst_port;
        tcp_seq;
        tcp_ack;
        tcp_data_offset;
        ( Field.v "reserved" (bits ~width:3 U16be)
        $ fun (_, _, _, _, _, x, _, _, _, _) -> x );
        tcp_flags;
        tcp_window;
        (Field.v "checksum" uint16be $ fun (_, _, _, _, _, _, _, _, x, _) -> x);
        (Field.v "urgent_ptr" uint16be $ fun (_, _, _, _, _, _, _, _, _, x) -> x);
      ]

type udp = UInt16.t * UInt16.t * UInt16.t * UInt16.t

let udp_src_port = Codec.(Field.v "src_port" uint16be $ fun (x, _, _, _) -> x)
let udp_dst_port = Codec.(Field.v "dst_port" uint16be $ fun (_, x, _, _) -> x)

let udp_length =
  Codec.(
    Field.v "length" uint16be ~self_constraint:(fun self ->
        Expr.(self >= int 8))
    $ fun (_, _, x, _) -> x)

let udp =
  Codec.v "UDP"
    (fun sp dp len sum -> (sp, dp, len, sum))
    Codec.
      [
        udp_src_port;
        udp_dst_port;
        udp_length;
        (Field.v "checksum" uint16be $ fun (_, _, _, x) -> x);
      ]
