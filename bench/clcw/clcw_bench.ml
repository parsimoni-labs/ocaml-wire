(** CLCW polling loop benchmark.

    Simulates a COP-1 receiver polling CLCW words using Wire's staged Codec.get
    -- all field access is generated from the Wire DSL. The C tier uses
    EverParse-generated validators for field extraction. *)

open Bench_lib
open Clcw_workload

external c_clcw_poll : bytes -> int -> int -> int = "c_clcw_poll"
external c_clcw_poll_result : bytes -> int -> int = "c_clcw_poll_result"

let benchmark ~n_words =
  let buf = generate_stream n_words in
  let st = state buf in
  let ocaml_result () = run_all st in
  let c_result () = c_clcw_poll_result buf 0 in
  let t =
    v "Wire OCaml" ~size:word_size ~reset:(fun () -> reset st) (step st)
    |> with_c c_clcw_poll buf
    |> with_expect ~equal:Int.equal ~pp:Fmt.int ~c:c_result ocaml_result
  in
  (t, st, buf)

let check ~n_words =
  let t, _, _ = benchmark ~n_words in
  Bench_lib.check t

let main () =
  Memtrace.trace_if_requested ~context:"clcw" ();
  let n_words =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000
  in
  let t, st, buf = benchmark ~n_words in
  Fmt.pr "CLCW polling loop (%d words, %dB each, contiguous buffer)\n\n" n_words
    word_size;
  run_table ~title:"CLCW polling" ~n:(n_words * 10) ~unit:"word" [ t ];
  reset st;
  let ocaml_anomalies = run_all st in
  let c_anomalies = c_clcw_poll_result buf 0 in
  Fmt.pr "\n  OCaml anomalies: %d\n  C anomalies:     %d\n" ocaml_anomalies
    c_anomalies
