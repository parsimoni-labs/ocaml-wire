(** Demonstrates zero-copy TCP/IP header parsing with Wire.

    Three nested protocol layers accessed via [sub] and [get] — no intermediate
    record allocation, no buffer copies. *)

open Wire
open Net

let () =
  let buf = (tcp_frame_data 1).(0) in

  Fmt.pr "TCP/IP Zero-Copy Header Parsing with Wire\n";
  Fmt.pr "==========================================\n\n";
  Fmt.pr "Frame: %d bytes in a single buffer\n\n" (Bytes.length buf);

  (* Layer 1: Ethernet — read ethertype, navigate to IPv4 sub-region *)
  let etype = Codec.get ethernet_codec f_eth_ethertype buf 0 in
  let ip_off = Codec.sub ethernet_codec f_eth_payload buf 0 in
  Fmt.pr "Ethernet: ethertype=0x%04X payload_off=%d\n" etype ip_off;

  (* Layer 2: IPv4 — read fields, navigate to TCP sub-region *)
  let protocol = Codec.get ipv4_codec f_ip_protocol buf ip_off in
  let src = Codec.get ipv4_codec f_ip_src buf ip_off in
  let dst = Codec.get ipv4_codec f_ip_dst buf ip_off in
  let tcp_off = Codec.sub ipv4_codec f_ip_payload buf ip_off in
  Fmt.pr "IPv4:     protocol=%d src=%a dst=%a payload_off=%d\n" protocol
    pp_ipv4_addr src pp_ipv4_addr dst tcp_off;

  (* Layer 3: TCP — read fields *)
  let src_port = Codec.get tcp_codec f_tcp_src_port buf tcp_off in
  let dst_port = Codec.get tcp_codec f_tcp_dst_port buf tcp_off in
  let syn = Codec.get tcp_codec f_tcp_syn buf tcp_off in
  let ack = Codec.get tcp_codec f_tcp_ack buf tcp_off in
  Fmt.pr "TCP:      %d -> %d SYN=%b ACK=%b\n\n" src_port dst_port syn ack;

  (* In-place mutation: change destination port *)
  Codec.set tcp_codec f_tcp_dst_port buf tcp_off 8080;
  Fmt.pr "After set: TCP dst_port=%d\n"
    (Codec.get tcp_codec f_tcp_dst_port buf tcp_off);

  Fmt.pr
    "\n\
     3 protocol layers parsed from a single %dB buffer, zero allocation for \
     int fields.\n"
    (Bytes.length buf)
