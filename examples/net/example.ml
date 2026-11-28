(** Demonstrates zero-copy TCP/IP header parsing with Wire.

    Three nested protocol layers checked once each and read in place through
    views: no intermediate record allocation, no buffer copies. *)

open Wire
open Net
module Slice = Bytesrw.Bytes.Slice
module View = Codec.View

(* [view c b ~off] is the view of the record of [c] at [off] in [b]. *)
let view c b ~off =
  let k = View.check c b ~off in
  if View.is_ok k then View.get k
  else
    Fmt.failwith "%a" Error.pp
      (Result.get_error (Codec.validate c (b : _ Buf.t :> bytes) ~off))

let () =
  let buf = (tcp_frame_data 1).(0) in
  let (Buf.Any b) = Buf.v buf in

  Fmt.pr "TCP/IP Zero-Copy Header Parsing with Wire\n";
  Fmt.pr "==========================================\n\n";
  Fmt.pr "Frame: %d bytes in a single buffer\n\n" (Bytes.length buf);

  (* Layer 1: Ethernet -- read ethertype, navigate to IPv4 sub-region *)
  let eth = view ethernet_codec b ~off:0 in
  let read c f v = View.read b v (View.field c f) in
  let etype = read ethernet_codec bf_eth_ethertype eth in
  let ip_off = Slice.first (read ethernet_codec bf_eth_payload eth) in
  Fmt.pr "Ethernet: ethertype=0x%04X payload_off=%d\n" (UInt16.to_int etype)
    ip_off;

  (* Layer 2: IPv4 -- read fields, navigate to TCP sub-region *)
  let ip = view ipv4_codec b ~off:ip_off in
  let protocol = read ipv4_codec bf_ip_protocol ip in
  let src = read ipv4_codec bf_ip_src ip in
  let dst = read ipv4_codec bf_ip_dst ip in
  let tcp_off = Slice.first (read ipv4_codec bf_ip_payload ip) in
  Fmt.pr "IPv4:     protocol=%d src=%a dst=%a payload_off=%d\n"
    (UInt8.to_int protocol) pp_ipv4_addr src pp_ipv4_addr dst tcp_off;

  (* Layer 3: TCP -- read fields *)
  let tcp = view tcp_codec b ~off:tcp_off in
  let src_port = read tcp_codec bf_tcp_src_port tcp in
  let dst_port = read tcp_codec bf_tcp_dst_port tcp in
  let syn = read tcp_codec bf_tcp_syn tcp in
  let ack = read tcp_codec bf_tcp_ack tcp in
  Fmt.pr "TCP:      %d -> %d SYN=%b ACK=%b\n\n" (UInt16.to_int src_port)
    (UInt16.to_int dst_port) syn ack;

  (* In-place mutation: change destination port *)
  View.set b tcp (View.field tcp_codec bf_tcp_dst_port) (UInt16.v 8080);
  Fmt.pr "After set: TCP dst_port=%d\n"
    (UInt16.to_int (read tcp_codec bf_tcp_dst_port tcp));

  Fmt.pr
    "\n\
     3 protocol layers parsed from a single %dB buffer, zero allocation for \
     int fields.\n"
    (Bytes.length buf)
