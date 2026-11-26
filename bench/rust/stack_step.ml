(* The checksum visitor the fold [checksum] of g_stack calls by name, over
   every output of [Packet_stack.codec]: the checksum of {!Selected_step} over
   the outputs it reads. *)

type t = Selected_step.t

let invalid = Selected_step.invalid

let[@inline always] valid base ~ether_type ~ipv4_present ~ipv4_version:_
    ~ipv4_ihl:_ ~ipv4_dscp:_ ~ipv4_ecn:_ ~ipv4_total_length
    ~ipv4_identification:_ ~ipv4_flags:_ ~ipv4_fragment_offset:_ ~ipv4_ttl
    ~ipv4_protocol ~ipv4_checksum:_ ~ipv4_src ~ipv4_dst ~ipv4_tcp_present
    ~ipv4_tcp_src_port ~ipv4_tcp_dst_port ~ipv4_tcp_seq ~ipv4_tcp_ack
    ~ipv4_tcp_data_offset ~ipv4_tcp_reserved:_ ~ipv4_tcp_flags ~ipv4_tcp_window
    ~ipv4_tcp_checksum:_ ~ipv4_tcp_urgent:_ ~ipv4_udp_present ~ipv4_udp_src_port
    ~ipv4_udp_dst_port ~ipv4_udp_length ~ipv4_udp_checksum:_
    ~ipv4_opaque_present:_ =
  Selected_step.valid base ~ether_type ~ipv4_present ~ipv4_total_length
    ~ipv4_ttl ~ipv4_protocol ~ipv4_src ~ipv4_dst ~ipv4_tcp_present
    ~ipv4_tcp_src_port ~ipv4_tcp_dst_port ~ipv4_tcp_seq ~ipv4_tcp_ack
    ~ipv4_tcp_data_offset ~ipv4_tcp_flags ~ipv4_tcp_window ~ipv4_udp_present
    ~ipv4_udp_src_port ~ipv4_udp_dst_port ~ipv4_udp_length
