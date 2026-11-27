let fields =
  [
    "ipv4_present";
    "ipv4_protocol";
    "ipv4_src";
    "ipv4_dst";
    "ipv4_tcp_present";
    "ipv4_tcp_dst_port";
    "ipv4_udp_present";
    "ipv4_udp_dst_port";
    "ipv6_present";
    "ipv6_next_header";
    "ipv6_dst3";
  ]

type t = string list

let invalid seen = "invalid" :: seen

let valid seen ~ipv4_present ~ipv4_protocol ~ipv4_src ~ipv4_dst
    ~ipv4_tcp_present ~ipv4_tcp_dst_port ~ipv4_udp_present ~ipv4_udp_dst_port
    ~ipv6_present ~ipv6_next_header ~ipv6_dst3 =
  Fmt.str "%b %Ld %Ld %Ld %b %Ld %b %Ld %b %Ld %Ld" ipv4_present ipv4_protocol
    ipv4_src ipv4_dst ipv4_tcp_present ipv4_tcp_dst_port ipv4_udp_present
    ipv4_udp_dst_port ipv6_present ipv6_next_header ipv6_dst3
  :: seen

let pp = Fmt.(list ~sep:cut string)
