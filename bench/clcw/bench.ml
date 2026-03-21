(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words using Wire's staged Codec.get
    — all field access is generated from the Wire DSL. The C baseline does the
    same work with hand-written bitfield extraction. *)

open Bench_lib
module C = Wire.Codec

external c_clcw_poll : bytes -> int -> int -> int = "c_clcw_poll"
external c_clcw_poll_result : bytes -> int -> int = "c_clcw_poll_result"

let n_words = 10_000_000
let word_size = Wire.Codec.wire_size Space.clcw_codec
let cf_report = Space.bf_cw_report
let cf_lockout = Space.bf_cw_lockout
let cf_wait = Space.bf_cw_wait
let cf_retransmit = Space.bf_cw_retransmit

let generate_stream n =
  let buf = Bytes.create (n * word_size) in
  let set_report = Wire.Staged.unstage (C.set Space.clcw_codec cf_report) in
  for i = 0 to n - 1 do
    let off = i * word_size in
    Wire.Codec.encode Space.clcw_codec Space.clcw_default buf off;
    set_report buf off (i mod 256)
  done;
  buf

let () =
  Memtrace.trace_if_requested ~context:"clcw" ();
  let buf = generate_stream n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each, contiguous buffer)\n\n" n_words
    word_size;

  let bf_lockout = C.bitfield Space.clcw_codec cf_lockout in
  let bf_wait = C.bitfield Space.clcw_codec cf_wait in
  let bf_retransmit = C.bitfield Space.clcw_codec cf_retransmit in
  let bf_report = C.bitfield Space.clcw_codec cf_report in
  let load = Wire.Staged.unstage (C.load_word bf_lockout) in

  let anomalies = ref 0 in
  let expected_seq = ref 0 in

  let fn, cycling_reset =
    cycling ~data:buf ~n_items:n_words ~size:word_size (fun buf off ->
        let w = load buf off in
        let lockout = C.extract bf_lockout w in
        let wait = C.extract bf_wait w in
        let retransmit = C.extract bf_retransmit w in
        let report = C.extract bf_report w in
        if
          lockout <> 0 || wait <> 0 || retransmit <> 0
          || report <> !expected_seq land 0xFF
        then incr anomalies;
        expected_seq := report)
  in
  let reset () =
    cycling_reset ();
    anomalies := 0;
    expected_seq := 0
  in

  let poll_all () =
    for i = 0 to n_words - 1 do
      let off = i * word_size in
      let w = load buf off in
      let lockout = C.extract bf_lockout w in
      let wait = C.extract bf_wait w in
      let retransmit = C.extract bf_retransmit w in
      let report = C.extract bf_report w in
      if
        lockout <> 0 || wait <> 0 || retransmit <> 0
        || report <> !expected_seq land 0xFF
      then incr anomalies;
      expected_seq := report
    done
  in

  let verify () =
    reset ();
    poll_all ();
    let ocaml_anomalies = !anomalies in
    let c_anomalies = c_clcw_poll_result buf 0 in
    if ocaml_anomalies <> c_anomalies then
      Fmt.failwith "CLCW result mismatch: OCaml=%d C=%d" ocaml_anomalies
        c_anomalies;
    reset ()
  in

  let t =
    v "Wire (staged Codec.get)" ~size:word_size ~reset fn
    |> with_c c_clcw_poll buf |> with_verify verify
  in

  run_table ~title:"CLCW polling" ~n:(n_words * 10) ~unit:"word" [ t ];

  reset ();
  poll_all ();
  let c_result = c_clcw_poll_result buf 0 in
  Fmt.pr "\n  OCaml anomalies: %d\n  C anomalies:     %d\n" !anomalies c_result
