(* CLCW polling: the word stream and the anomaly check, each word checked by
   Wire's [Codec.View.check] and its fields read from the view with
   [Codec.View.read]. *)

module C = Wire.Codec
module V = C.View

let word_size = Wire.Codec.min_size Space.clcw_codec
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
  let report = ref 0 in
  let flag b = if b then 1 else 0 in
  for i = 0 to n - 1 do
    let off = i * word_size in
    if i mod 127 = 126 then incr report;
    C.encode Space.clcw_codec
      {
        Space.clcw_default with
        Space.wait = flag (i mod 61 = 60);
        retransmit = flag (i mod 97 = 96);
        lockout = flag (i mod 211 = 210);
        report = !report land 0xFF;
      }
      buf ~off;
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
  let w_lockout = V.field Space.clcw_codec cf_lockout in
  let w_wait = V.field Space.clcw_codec cf_wait in
  let w_retransmit = V.field Space.clcw_codec cf_retransmit in
  let w_report = V.field Space.clcw_codec cf_report in
  let anomalies = ref 0 in
  let expected_seq = ref 0 in
  let process_word buf off =
    let (Wire.Buf.Any b) = Wire.Buf.v buf in
    let k = V.check Space.clcw_codec b ~off in
    if V.is_ok k then
      let v = V.get k in
      detect_anomaly ~lockout:(V.read b v w_lockout) ~wait:(V.read b v w_wait)
        ~retransmit:(V.read b v w_retransmit) ~report:(V.read b v w_report)
        anomalies expected_seq
    else incr anomalies
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
let report ~out n =
  for i = 0 to n - 1 do
    let off = i * word_size in
    C.encode Space.clcw_codec
      {
        Space.clcw_default with
        Space.lockout = ((i land 0xFFF) + 1) lsr 12;
        wait = ((i land 0x3F) + 1) lsr 6;
        retransmit = (i lsr 13) land 1;
        report = i land 0xFF;
      }
      out ~off
  done
