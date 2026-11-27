(* The checksum visitor the fold [checksum] of g_selected calls by name, over
   the outputs of [Packet_stack.codec] the checksum reads. It folds them as
   [int64], as the Rust rows fold [u64]. *)

type t = int64

external ( + ) : int64 -> int64 -> int64 = "%int64_add"
external ( lxor ) : int64 -> int64 -> int64 = "%int64_xor"
external ( lor ) : int64 -> int64 -> int64 = "%int64_or"
external ( lsl ) : int64 -> int -> int64 = "%int64_lsl"

let[@inline always] invalid (base : t) = base

let[@inline always] valid base ~ether_type ~ipv4_present ~ipv4_total_length
    ~ipv4_ttl ~ipv4_protocol ~ipv4_src ~ipv4_dst ~ipv4_tcp_present
    ~ipv4_tcp_src_port ~ipv4_tcp_dst_port ~ipv4_tcp_seq ~ipv4_tcp_ack
    ~ipv4_tcp_data_offset ~ipv4_tcp_flags ~ipv4_tcp_window ~ipv4_udp_present
    ~ipv4_udp_src_port ~ipv4_udp_dst_port ~ipv4_udp_length =
  let contribution =
    if not ipv4_present then ether_type
    else
      let ip0 = ipv4_src lxor ipv4_dst in
      let ip1 = ipv4_ttl + ipv4_protocol in
      let ip2 = ether_type + ipv4_total_length in
      let transport =
        if ipv4_tcp_present then
          let a = (ipv4_tcp_src_port lsl 16) lor ipv4_tcp_dst_port in
          let b = ipv4_tcp_seq lxor ipv4_tcp_ack in
          let c = ipv4_tcp_window + ipv4_tcp_flags in
          a + b + (c + ipv4_tcp_data_offset)
        else if ipv4_udp_present then
          ((ipv4_udp_src_port lsl 16) lor ipv4_udp_dst_port) + ipv4_udp_length
        else 0L
      in
      ip0 + ip1 + (ip2 + transport)
  in
  base + contribution
