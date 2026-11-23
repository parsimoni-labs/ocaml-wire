(* CLCW polling: the word stream and the anomaly check, every field taken
   from one word load with Wire's [Codec.load_word] and [Codec.extract]. *)

module C = Wire.Codec

let word_size = Wire.Codec.wire_size Space.clcw_codec
let cf_report = Space.bf_cw_report
let cf_lockout = Space.bf_cw_lockout
let cf_wait = Space.bf_cw_wait
let cf_retransmit = Space.bf_cw_retransmit

(* The anomalies the check counts, each kind on words of its own: wait on
   every 61st word, retransmit on every 97th, lockout on every 211th, and a
   report value that skips one on every 127th, so that each test of the check
   meets words that fail it. *)
let generate_stream n =
  let buf = Bytes.create (n * word_size) in
  let set f = Wire.Staged.unstage (C.set Space.clcw_codec f) in
  let set_report = set cf_report and set_lockout = set cf_lockout in
  let set_wait = set cf_wait and set_retransmit = set cf_retransmit in
  let report = ref 0 in
  for i = 0 to n - 1 do
    let off = i * word_size in
    Wire.Codec.encode Space.clcw_codec Space.clcw_default buf off;
    if i mod 61 = 60 then set_wait buf off 1;
    if i mod 97 = 96 then set_retransmit buf off 1;
    if i mod 211 = 210 then set_lockout buf off 1;
    if i mod 127 = 126 then incr report;
    set_report buf off (!report land 0xFF);
    incr report
  done;
  buf

let detect_anomaly ~lockout ~wait ~retransmit ~report anomalies expected_seq =
  let expected_report = !expected_seq land 0xFF in
  if lockout <> 0 || wait <> 0 || retransmit <> 0 || report <> expected_report
  then incr anomalies;
  expected_seq := (report + 1) land 0xFF

type state = {
  buf : bytes;
  n_words : int;
  anomalies : int ref;
  expected_seq : int ref;
  process_word : bytes -> int -> unit;
  step_once : unit -> unit;
  reset_cycle : unit -> unit;
}

let state buf =
  let n_words = Bytes.length buf / word_size in
  let bf_lockout = C.bitfield Space.clcw_codec cf_lockout in
  let bf_wait = C.bitfield Space.clcw_codec cf_wait in
  let bf_retransmit = C.bitfield Space.clcw_codec cf_retransmit in
  let bf_report = C.bitfield Space.clcw_codec cf_report in
  let load = Wire.Staged.unstage (C.load_word bf_lockout) in
  let anomalies = ref 0 in
  let expected_seq = ref 0 in
  let process_word buf off =
    let w = load buf off in
    detect_anomaly ~lockout:(C.extract bf_lockout w) ~wait:(C.extract bf_wait w)
      ~retransmit:(C.extract bf_retransmit w)
      ~report:(C.extract bf_report w) anomalies expected_seq
  in
  let step_once, reset_cycle =
    Bench_lib.cycling ~data:buf ~n_items:n_words ~size:word_size process_word
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

(* The word {!Space.clcw_default} encodes, COP-1 in effect on virtual channel
   7, every flag clear, with the four fields a FARM reports. *)
let write_word =
  Wire.Staged.unstage
    (C.writer Space.clcw_codec Space.clcw_default
       [ cf_lockout; cf_wait; cf_retransmit; cf_report ])

let report ~out n =
  for i = 0 to n - 1 do
    let off = i * word_size in
    write_word out off
      (((i land 0xFFF) + 1) lsr 12)
      (((i land 0x3F) + 1) lsr 6)
      ((i lsr 13) land 1)
      (i land 0xFF)
  done
