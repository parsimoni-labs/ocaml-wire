(** Protocol gateway benchmark: parse + rewrite + re-encode across 4 layers.

    Simulates a CCSDS-over-UDP protocol translator:
    - Traverse Eth -> IPv4 -> UDP -> Space Packet (4 layers)
    - Rewrite APID in the Space Packet
    - Rewrite UDP destination port
    - Update UDP checksum

    Compares Wire zero-copy sub+get+set vs hand-written Bytes access. *)

module C = Wire.Codec

let n_frames = 10_000_000

let time label f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let dt = Unix.gettimeofday () -. t0 in
  let ns_per = dt *. 1e9 /. float n_frames in
  let mpps = float n_frames /. dt /. 1e6 in
  Fmt.pr "  %-50s %6.1f ns/frm  %5.1f Mfrm/s\n" label ns_per mpps

let () =
  let frames = Net.udp_frame_data n_frames in
  let frame_size = Bytes.length frames.(0) in
  Fmt.pr "Protocol gateway (%d frames, %dB each)\n\n" n_frames frame_size;

  (* Wire: zero-copy traversal + rewrite *)
  time "wire: sub*3 + get APID + set APID,port,csum" (fun () ->
      Array.iter
        (fun buf ->
          (* Traverse 4 layers *)
          let ip = C.sub Net.ethernet_codec Net.f_eth_payload buf 0 in
          let udp = C.sub Net.ipv4_codec Net.f_ip_payload buf ip in
          let sp = udp + Net.udp_size in
          (* Read + rewrite APID *)
          let _apid = C.get Space.packet_codec Space.f_sp_apid buf sp in
          C.set Space.packet_codec Space.f_sp_apid buf sp 42;
          (* Rewrite UDP dst port *)
          C.set Net.udp_codec Net.f_udp_dst_port buf udp 5555;
          (* Zero out UDP checksum (common for UDP-over-IPv4) *)
          C.set Net.udp_codec Net.f_udp_checksum buf udp 0)
        frames);

  (* Hand-written: hardcoded offsets for Eth(14)+IPv4(20)+UDP(8)+SpacePacket *)
  let eth_hdr = 14 in
  let ip_hdr = 20 in
  let udp_off = eth_hdr + ip_hdr in
  let sp_off = udp_off + 8 in
  time "hand: hardcoded offsets + Bytes get/set" (fun () ->
      Array.iter
        (fun buf ->
          (* Read + rewrite APID *)
          let w0 = Bytes.get_uint16_be buf sp_off in
          let _apid = w0 land 0x7FF in
          Bytes.set_uint16_be buf sp_off (w0 land 0xF800 lor 42);
          (* Rewrite UDP dst port *)
          Bytes.set_uint16_be buf (udp_off + 2) 5555;
          (* Zero UDP checksum *)
          Bytes.set_uint16_be buf (udp_off + 6) 0)
        frames)
