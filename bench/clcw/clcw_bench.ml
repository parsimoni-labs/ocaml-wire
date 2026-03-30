(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words using Wire's staged Codec.get
    — all field access is generated from the Wire DSL. The C tier uses
    EverParse-generated validators for field extraction. *)

open Bench_lib
module C = Wire.Codec

external c_clcw_poll : bytes -> int -> int -> int = "c_clcw_poll"
external c_clcw_poll_result : bytes -> int -> int = "c_clcw_poll_result"

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

type state = {
  buf : bytes;
  n_words : int;
  anomalies : int ref;
  expected_seq : int ref;
  process_word : bytes -> int -> unit;
  step_once : unit -> unit;
  reset_cycle : unit -> unit;
}

let state n_words =
  let buf = generate_stream n_words in
  let bf_lockout = C.bitfield Space.clcw_codec cf_lockout in
  let bf_wait = C.bitfield Space.clcw_codec cf_wait in
  let bf_retransmit = C.bitfield Space.clcw_codec cf_retransmit in
  let bf_report = C.bitfield Space.clcw_codec cf_report in
  let load = Wire.Staged.unstage (C.load_word bf_lockout) in
  let anomalies = ref 0 in
  let expected_seq = ref 0 in
  let process_word buf off =
    let w = load buf off in
    let lockout = C.extract bf_lockout w in
    let wait = C.extract bf_wait w in
    let retransmit = C.extract bf_retransmit w in
    let report = C.extract bf_report w in
    let expected_report = !expected_seq land 0xFF in
    if lockout <> 0 || wait <> 0 || retransmit <> 0 || report <> expected_report
    then incr anomalies;
    expected_seq := (report + 1) land 0xFF
  in
  let step_once, reset_cycle =
    cycling ~data:buf ~n_items:n_words ~size:word_size process_word
  in
  {
    buf;
    n_words;
    anomalies;
    expected_seq;
    process_word;
    step_once;
    reset_cycle;
  }

let reset state =
  state.reset_cycle ();
  state.anomalies := 0;
  state.expected_seq := 0

let step state () = state.step_once ()

let run_all state =
  for i = 0 to state.n_words - 1 do
    let off = i * word_size in
    state.process_word state.buf off
  done;
  !(state.anomalies)

let benchmark ~n_words =
  let st = state n_words in
  let ocaml_result () = run_all st in
  let c_result () = c_clcw_poll_result st.buf 0 in
  let ffi_index = ref 0 in
  let ffi_anomalies = ref 0 in
  let ffi_expected_seq = ref 0 in
  let ffi_reset () =
    ffi_index := 0;
    ffi_anomalies := 0;
    ffi_expected_seq := 0
  in
  let ffi_fn _buf =
    let off = !ffi_index mod st.n_words * word_size in
    let r = C_stubs.clcw_parse st.buf off in
    let expected_report = !ffi_expected_seq land 0xFF in
    if
      r.lockout <> 0 || r.wait <> 0 || r.retransmit <> 0
      || r.reportvalue <> expected_report
    then incr ffi_anomalies;
    ffi_expected_seq := (r.reportvalue + 1) land 0xFF;
    incr ffi_index
  in
  let ffi_result () =
    ffi_reset ();
    for _ = 0 to st.n_words - 1 do
      ffi_fn Bytes.empty
    done;
    !ffi_anomalies
  in
  let reset () =
    reset st;
    ffi_reset ()
  in
  let t =
    v "Wire OCaml" ~size:word_size ~reset (step st)
    |> with_c c_clcw_poll st.buf
    |> with_ffi ffi_fn Bytes.empty
    |> with_expect ~equal:Int.equal ~pp:Fmt.int ~ffi:ffi_result ~c:c_result
         ocaml_result
  in
  (t, st)

let check ~n_words =
  let t, _ = benchmark ~n_words in
  Bench_lib.check t

let main () =
  Memtrace.trace_if_requested ~context:"clcw" ();
  let n_words = 1_000_000 in
  let t, st = benchmark ~n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each, contiguous buffer)\n\n" n_words
    word_size;
  run_table ~title:"CLCW polling" ~n:(n_words * 10) ~unit:"word" [ t ];
  reset st;
  let ocaml_anomalies = run_all st in
  let c_anomalies = c_clcw_poll_result st.buf 0 in
  Fmt.pr "\n  OCaml anomalies: %d\n  C anomalies:     %d\n" ocaml_anomalies
    c_anomalies
