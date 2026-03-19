(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words using Wire's staged Codec.get
    — all field access is generated from the Wire DSL. *)

open Bench_lib
module C = Wire.Codec

let n_words = 10_000_000
let word_size = Wire.Codec.wire_size Space.clcw_codec

let generate_stream n =
  let buf = Bytes.create (n * word_size) in
  let set_report =
    Wire.Staged.unstage (C.set Space.clcw_codec Space.cw_report)
  in
  for i = 0 to n - 1 do
    let off = i * word_size in
    Wire.Codec.encode Space.clcw_codec Space.clcw_default buf off;
    set_report buf off (i mod 256)
  done;
  buf

let () =
  let buf = generate_stream n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each, contiguous buffer)\n\n" n_words
    word_size;

  let get_lockout =
    Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_lockout)
  in
  let get_wait = Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_wait) in
  let get_retransmit =
    Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_retransmit)
  in
  let get_report =
    Wire.Staged.unstage (C.get Space.clcw_codec Space.cw_report)
  in

  let anomalies = ref 0 in
  let expected_seq = ref 0 in

  (* OCaml tier: poll one CLCW word, cycling through the buffer *)
  let fn, cycling_reset =
    cycling ~data:buf ~n_items:n_words ~size:word_size (fun buf off ->
        let lockout = get_lockout buf off in
        let wait = get_wait buf off in
        let retransmit = get_retransmit buf off in
        let report = get_report buf off in
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

  let single_word = Bytes.sub buf 0 word_size in
  let t =
    ( v "Wire (staged Codec.get)" ~size:word_size ~reset fn |> fun t ->
      match C_tier.clcw_loop with Some f -> with_c f buf t | None -> t )
    |> fun t ->
    match C_tier.clcw_check with
    | Some f -> with_ffi f single_word t
    | None -> t
  in

  run_table ~title:"CLCW polling" ~n:(n_words * 10) ~unit:"word" [ t ];
  Fmt.pr "\n  %d anomalies\n" !anomalies
