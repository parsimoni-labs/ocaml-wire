(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words at frame rate:
    - Read Lockout, Wait, Retransmit flags (1-bit bitfields in bf_uint32be)
    - Read ReportValue for FARM-B counter (8-bit bitfield)
    - Compare with expected sequence number, flag anomalies

    Compares: pure C (shift/mask) vs Wire OCaml (staged Codec.get). *)

module C = Wire.Codec

let n_words = 10_000_000
let n_rounds = 10

let () =
  let words = Space.clcw_data n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each)\n\n" n_words Space.clcw_size;

  (* C baseline — run n_rounds to match Wire *)
  let c_total_ns = ref 0 in
  for _ = 1 to n_rounds do
    c_total_ns := !c_total_ns + C_scenarios.clcw words n_words
  done;
  let c_anomalies = C_scenarios.clcw_anomalies () in
  let total_ops = n_words * n_rounds in
  let c_ns = !c_total_ns in
  let c_dt = float c_ns /. 1e9 in
  let c_ns_per = float c_ns /. float total_ops in

  (* Wire: load + extract (read word once, extract 4 fields) *)
  let load_word =
    Wire.Staged.unstage (C.load Space.clcw_codec Space.cw_lockout)
  in
  let x_lockout =
    Wire.Staged.unstage (C.extract Space.clcw_codec Space.cw_lockout)
  in
  let x_wait = Wire.Staged.unstage (C.extract Space.clcw_codec Space.cw_wait) in
  let x_retransmit =
    Wire.Staged.unstage (C.extract Space.clcw_codec Space.cw_retransmit)
  in
  let x_report =
    Wire.Staged.unstage (C.extract Space.clcw_codec Space.cw_report)
  in
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  let w_anomalies = ref 0 in
  let expected_seq = ref 0 in
  for _ = 1 to n_rounds do
    w_anomalies := 0;
    expected_seq := 0;
    Array.iter
      (fun buf ->
        let word = load_word buf 0 in
        let lockout = x_lockout word in
        let wait = x_wait word in
        let retransmit = x_retransmit word in
        let report = x_report word in
        if
          lockout <> 0 || wait <> 0 || retransmit <> 0
          || report <> !expected_seq land 0xFF
        then incr w_anomalies;
        expected_seq := report)
      words
  done;
  let w_dt = Unix.gettimeofday () -. t0 in
  let total_ops = n_words * n_rounds in
  let w_ns_per = w_dt *. 1e9 /. float total_ops in
  let ratio = w_ns_per /. c_ns_per in

  Fmt.pr "  %-24s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)\n"
    "C (baseline)" c_ns_per
    (float total_ops /. c_dt /. 1e6)
    c_anomalies;
  Fmt.pr "  %-24s %6.1f ns/word  %5.1f Mcheck/s  (%d anomalies)  (%.1fx)\n"
    "Wire (load + extract)" w_ns_per
    (float total_ops /. w_dt /. 1e6)
    !w_anomalies ratio;
  if c_anomalies <> !w_anomalies then
    Fmt.pr "\n  MISMATCH! C: %d anomalies, Wire: %d anomalies\n" c_anomalies
      !w_anomalies
  else Fmt.pr "\n  %d anomalies detected (C and Wire agree)\n" c_anomalies
