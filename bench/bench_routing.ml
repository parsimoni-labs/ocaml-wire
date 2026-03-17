(** Telemetry routing benchmark: parse CCSDS Space Packets and dispatch by APID.

    Simulates a ground station TM router:
    - Read APID (11-bit bitfield) to select handler
    - Read SeqCount to detect gaps
    - Read DataLength for payload extraction
    - Dispatch to handler based on APID

    Compares Wire zero-copy get vs hand-written Bytes access. *)

module C = Wire.Codec

let n_packets = 10_000_000

let time label f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let dt = Unix.gettimeofday () -. t0 in
  let ns_per = dt *. 1e9 /. float n_packets in
  let mpps = float n_packets /. dt /. 1e6 in
  Fmt.pr "  %-50s %6.1f ns/pkt  %5.1f Mpkt/s\n" label ns_per mpps

(* APID dispatch table — 4 handlers covering the 11-bit space *)
let handler_hk = ref 0
let handler_sci = ref 0
let handler_diag = ref 0
let handler_other = ref 0

let[@inline] dispatch_apid apid =
  if apid < 256 then incr handler_hk
  else if apid < 1024 then incr handler_sci
  else if apid < 1536 then incr handler_diag
  else incr handler_other

let () =
  let packets = Space.packet_data n_packets in
  Fmt.pr "Telemetry routing (%d packets, %dB each)\n\n" n_packets
    Space.packet_size;

  (* Wire: zero-copy field access *)
  time "wire: get APID + SeqCount + DataLen + dispatch" (fun () ->
      Array.iter
        (fun buf ->
          let apid = C.get Space.packet_codec Space.f_sp_apid buf 0 in
          let _seq = C.get Space.packet_codec Space.f_sp_seq_count buf 0 in
          let _dlen = C.get Space.packet_codec Space.f_sp_data_len buf 0 in
          dispatch_apid apid)
        packets);

  handler_hk := 0;
  handler_sci := 0;
  handler_diag := 0;
  handler_other := 0;

  (* Hand-written: hardcoded offsets *)
  time "hand: Bytes.get_uint16_be + mask/shift + dispatch" (fun () ->
      Array.iter
        (fun buf ->
          let w0 = Bytes.get_uint16_be buf 0 in
          let apid = w0 land 0x7FF in
          let w1 = Bytes.get_uint16_be buf 2 in
          let _seq = w1 land 0x3FFF in
          let _dlen = Bytes.get_uint16_be buf 4 in
          dispatch_apid apid)
        packets);

  Fmt.pr "\n  dispatched: hk=%d sci=%d diag=%d other=%d\n" !handler_hk
    !handler_sci !handler_diag !handler_other
