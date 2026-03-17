(** CLCW monitoring benchmark: bitfield-heavy read pattern.

    Simulates real-time CLCW monitoring:
    - Check Lockout, Wait, Retransmit flags (1-bit bitfields)
    - Read ReportValue for FARM-B counter (8-bit bitfield)
    - Flag anomalies

    All 4 reads are from a single 32-bit word (bf_uint32be), exercising the
    bitfield closure dispatch path. *)

module C = Wire.Codec

let n_words = 10_000_000

let time label f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let dt = Unix.gettimeofday () -. t0 in
  let ns_per = dt *. 1e9 /. float n_words in
  let mcps = float n_words /. dt /. 1e6 in
  Fmt.pr "  %-50s %6.1f ns/word  %5.1f Mcheck/s\n" label ns_per mcps

let () =
  let words = Space.clcw_data n_words in
  Fmt.pr "CLCW monitoring (%d words, %dB each)\n\n" n_words Space.clcw_size;

  let anomalies = ref 0 in

  (* Wire: bitfield accessors *)
  time "wire: get lockout+wait+retransmit+report" (fun () ->
      Array.iter
        (fun buf ->
          let lockout = C.get Space.clcw_codec Space.cw_lockout buf 0 in
          let wait = C.get Space.clcw_codec Space.cw_wait buf 0 in
          let retransmit = C.get Space.clcw_codec Space.cw_retransmit buf 0 in
          let report = C.get Space.clcw_codec Space.cw_report buf 0 in
          if lockout <> 0 || wait <> 0 || retransmit <> 0 || report > 200 then
            incr anomalies)
        words);

  let wire_anomalies = !anomalies in
  anomalies := 0;

  (* Hand-written: single 32-bit read + mask/shift *)
  time "hand: get_int32_be + mask/shift x4" (fun () ->
      Array.iter
        (fun buf ->
          let w = Bytes.get_int32_be buf 0 |> Int32.to_int in
          let lockout = (w lsr 8) land 1 in
          let wait = (w lsr 7) land 1 in
          let retransmit = (w lsr 6) land 1 in
          let report = w land 0xFF in
          if lockout <> 0 || wait <> 0 || retransmit <> 0 || report > 200 then
            incr anomalies)
        words);

  let hand_anomalies = !anomalies in
  Fmt.pr "\n  anomalies: wire=%d hand=%d\n" wire_anomalies hand_anomalies
