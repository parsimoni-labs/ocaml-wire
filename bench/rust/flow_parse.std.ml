(* The visitor of the fold [parse] of g_flow, on OCaml: it hands the key of
   each packet to [Flow_sum] as [int]s, since an [int64] handed to a call
   would be boxed and the pair the OxCaml visitor folds allocated for each
   key. [i] and [ip6_bytes] are defined beside [valid], not in it: OCaml
   without flambda inlines no function whose body defines one, and an
   out-of-line [valid] boxes each [int64] it is passed. *)

type t = int

let i = Int64.to_int
let ip6_bytes l = 40 + Int64.to_int l
let[@inline always] invalid (base : t) = base

let[@inline always] ports ~src ~dst =
  (Int64.to_int src lsl 16) lor Int64.to_int dst

let[@inline always] valid base ~ipv4_present ~ipv4_total_length ~ipv4_protocol
    ~ipv4_src ~ipv4_dst ~ipv4_tcp_present ~ipv4_tcp_src_port ~ipv4_tcp_dst_port
    ~ipv4_udp_present ~ipv4_udp_src_port ~ipv4_udp_dst_port ~ipv6_present
    ~ipv6_payload_length ~ipv6_next_header ~ipv6_src0 ~ipv6_src1 ~ipv6_src2
    ~ipv6_src3 ~ipv6_dst0 ~ipv6_dst1 ~ipv6_dst2 ~ipv6_dst3 ~ipv6_tcp_present
    ~ipv6_tcp_src_port ~ipv6_tcp_dst_port ~ipv6_udp_present ~ipv6_udp_src_port
    ~ipv6_udp_dst_port =
  if ipv4_present then (
    Flow_sum.add4 ~src:(i ipv4_src) ~dst:(i ipv4_dst) ~proto:(i ipv4_protocol)
      ~ports:
        (if ipv4_tcp_present then
           ports ~src:ipv4_tcp_src_port ~dst:ipv4_tcp_dst_port
         else if ipv4_udp_present then
           ports ~src:ipv4_udp_src_port ~dst:ipv4_udp_dst_port
         else 0)
      ~bytes:(i ipv4_total_length);
    base)
  else if ipv6_present then (
    Flow_sum.add6 ~s0:(i ipv6_src0) ~s1:(i ipv6_src1) ~s2:(i ipv6_src2)
      ~s3:(i ipv6_src3) ~d0:(i ipv6_dst0) ~d1:(i ipv6_dst1) ~d2:(i ipv6_dst2)
      ~d3:(i ipv6_dst3) ~proto:(i ipv6_next_header)
      ~ports:
        (if ipv6_tcp_present then
           ports ~src:ipv6_tcp_src_port ~dst:ipv6_tcp_dst_port
         else if ipv6_udp_present then
           ports ~src:ipv6_udp_src_port ~dst:ipv6_udp_dst_port
         else 0)
      ~bytes:(ip6_bytes ipv6_payload_length);
    base)
  else base

let init () = 0
let finish (_ : t) = ()
