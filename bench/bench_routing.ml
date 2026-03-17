(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router:
    - Pre-allocate a contiguous buffer of N variable-size Space Packets
    - For each packet: read 6-byte primary header, extract APID (11-bit
      bitfield), look up routing table, extract variable-length payload via
      DataLength, pass payload pointer to handler (no copy)
    - Measure packets/sec and MB/s of payload delivered

    Compares: pure C (shift/mask) vs Wire OCaml (Codec.get) vs hand OCaml. *)

module C = Wire.Codec

(* Realistic APID distribution: HK (small packets), science (large),
   diagnostic (medium), idle (minimal). *)
let apid_of_index i =
  let r = i mod 100 in
  if r < 40 then i mod 256 (* 40% HK: APIDs 0-255 *)
  else if r < 75 then 256 + (i mod 768) (* 35% science: 256-1023 *)
  else if r < 95 then 1024 + (i mod 512) (* 20% diagnostic: 1024-1535 *)
  else 0x7FF (* 5% idle *)

(* Packet size distribution: HK=32B, science=256B, diag=64B, idle=8B payload *)
let payload_size_of_apid apid =
  if apid < 256 then 32
  else if apid < 1024 then 256
  else if apid < 1536 then 64
  else 8

(* Generate a contiguous buffer of n variable-size Space Packets. Returns
   (buf, n_packets, total_payload_bytes). *)
let generate_stream n =
  let total = ref 0 in
  let hdr = Space.packet_size in
  for i = 0 to n - 1 do
    let apid = apid_of_index i in
    total := !total + hdr + payload_size_of_apid apid
  done;
  let buf = Bytes.create !total in
  let off = ref 0 in
  let payload_total = ref 0 in
  for i = 0 to n - 1 do
    let apid = apid_of_index i in
    let plen = payload_size_of_apid apid in
    let w0 = (1 lsl 11) lor apid in
    Bytes.set_uint16_be buf !off w0;
    let w1 = (3 lsl 14) lor (i mod 16384) in
    Bytes.set_uint16_be buf (!off + 2) w1;
    Bytes.set_uint16_be buf (!off + 4) (plen - 1);
    off := !off + hdr + plen;
    payload_total := !payload_total + plen
  done;
  (buf, !total, !payload_total)

let routing_table =
  Array.init 2048 (fun apid ->
      if apid < 256 then 0
      else if apid < 1024 then 1
      else if apid < 1536 then 2
      else 3)

let handler_counts = Array.make 4 0

let[@inline] dispatch handler_id =
  handler_counts.(handler_id) <- handler_counts.(handler_id) + 1

let () =
  let n = 10_000_000 in
  let buf, total_bytes, payload_bytes = generate_stream n in
  Fmt.pr "APID demux (%d packets, %d MB stream, %d MB payload)\n\n" n
    (total_bytes / 1_000_000)
    (payload_bytes / 1_000_000);

  let time label f =
    Gc.compact ();
    Array.fill handler_counts 0 4 0;
    let t0 = Unix.gettimeofday () in
    f ();
    let dt = Unix.gettimeofday () -. t0 in
    let ns_per = dt *. 1e9 /. float n in
    let mpps = float n /. dt /. 1e6 in
    let mbps = float payload_bytes /. dt /. 1e6 in
    Fmt.pr "  %-50s %5.1f ns/pkt  %5.1f Mpkt/s  %6.0f MB/s\n" label ns_per mpps
      mbps
  in

  let hdr = Space.packet_size in

  (* Pure C: shift/mask in tight loop *)
  let c_ns = C_scenarios.routing buf n in
  let c_dt = float c_ns /. 1e9 in
  Fmt.pr "  %-50s %5.1f ns/pkt  %5.1f Mpkt/s  %6.0f MB/s\n"
    "C: shift/mask + dispatch (tight loop)"
    (float c_ns /. float n)
    (float n /. c_dt /. 1e6)
    (float payload_bytes /. c_dt /. 1e6);

  (* Wire (staged): partial-apply get outside loop *)
  let get_apid = C.get Space.packet_codec Space.f_sp_apid in
  let get_seq = C.get Space.packet_codec Space.f_sp_seq_count in
  let get_dlen = C.get Space.packet_codec Space.f_sp_data_len in
  time "wire (staged): get APID + SeqCount + DataLen" (fun () ->
      let off = ref 0 in
      for _ = 1 to n do
        let o = !off in
        let apid = get_apid buf o in
        let _seq = get_seq buf o in
        let dlen = get_dlen buf o in
        dispatch routing_table.(apid);
        off := o + hdr + dlen + 1
      done);

  (* Wire (naive): full 4-arg get inside loop *)
  time "wire (naive): Codec.get codec field buf off" (fun () ->
      let off = ref 0 in
      for _ = 1 to n do
        let o = !off in
        let apid = C.get Space.packet_codec Space.f_sp_apid buf o in
        let _seq = C.get Space.packet_codec Space.f_sp_seq_count buf o in
        let dlen = C.get Space.packet_codec Space.f_sp_data_len buf o in
        dispatch routing_table.(apid);
        off := o + hdr + dlen + 1
      done);

  (* Hand-written OCaml: hardcoded offsets + shift/mask *)
  time "hand: Bytes.get_uint16_be + shift/mask + dispatch" (fun () ->
      let off = ref 0 in
      for _ = 1 to n do
        let o = !off in
        let w0 = Bytes.get_uint16_be buf o in
        let apid = w0 land 0x7FF in
        let _seq = Bytes.get_uint16_be buf (o + 2) land 0x3FFF in
        let dlen = Bytes.get_uint16_be buf (o + 4) in
        dispatch routing_table.(apid);
        off := o + hdr + dlen + 1
      done);

  Fmt.pr "\n  routed: hk=%d sci=%d diag=%d idle=%d\n" handler_counts.(0)
    handler_counts.(1) handler_counts.(2) handler_counts.(3)
