(* The visitor of the fold [parse] of g_flow, on OxCaml: it computes each key
   on the [int64] values the scanner hands out, with [Flow_sum]'s key
   functions, and folds the keys in its accumulator, an unboxed pair of the
   number of keys and their sum, which stays in registers. *)

type t = #(int * int)

let i x = x
let ip6_bytes l = Int64.add 40L l
let[@inline always] invalid (base : t) = base

let[@inline always] ports ~src ~dst = Int64.logor (Int64.shift_left src 16) dst

let[@inline always] valid base ~ipv4_present ~ipv4_total_length ~ipv4_protocol
    ~ipv4_src ~ipv4_dst ~ipv4_tcp_present ~ipv4_tcp_src_port ~ipv4_tcp_dst_port
    ~ipv4_udp_present ~ipv4_udp_src_port ~ipv4_udp_dst_port ~ipv6_present
    ~ipv6_payload_length ~ipv6_next_header ~ipv6_src0 ~ipv6_src1 ~ipv6_src2
    ~ipv6_src3 ~ipv6_dst0 ~ipv6_dst1 ~ipv6_dst2 ~ipv6_dst3 ~ipv6_tcp_present
    ~ipv6_tcp_src_port ~ipv6_tcp_dst_port ~ipv6_udp_present ~ipv6_udp_src_port
    ~ipv6_udp_dst_port =
  if ipv4_present then
    let #(keys, sum) = base in
    #( keys + 1,
       sum
       + Int64.to_int
           (Flow_sum.key4 ~src:(i ipv4_src) ~dst:(i ipv4_dst)
              ~proto:(i ipv4_protocol)
              ~ports:
                (if ipv4_tcp_present then
                   ports ~src:ipv4_tcp_src_port ~dst:ipv4_tcp_dst_port
                 else if ipv4_udp_present then
                   ports ~src:ipv4_udp_src_port ~dst:ipv4_udp_dst_port
                 else 0L)
              ~bytes:(i ipv4_total_length)) )
  else if ipv6_present then
    let #(keys, sum) = base in
    #( keys + 1,
       sum
       + Int64.to_int
           (Flow_sum.key6 ~s0:(i ipv6_src0) ~s1:(i ipv6_src1) ~s2:(i ipv6_src2)
              ~s3:(i ipv6_src3) ~d0:(i ipv6_dst0) ~d1:(i ipv6_dst1)
              ~d2:(i ipv6_dst2) ~d3:(i ipv6_dst3) ~proto:(i ipv6_next_header)
              ~ports:
                (if ipv6_tcp_present then
                   ports ~src:ipv6_tcp_src_port ~dst:ipv6_tcp_dst_port
                 else if ipv6_udp_present then
                   ports ~src:ipv6_udp_src_port ~dst:ipv6_udp_dst_port
                 else 0L)
              ~bytes:(ip6_bytes ipv6_payload_length)) )
  else base

let[@inline always] init () = #(0, 0)
let[@inline always] finish #(keys, sum) = Flow_sum.add_pass ~keys ~sum
