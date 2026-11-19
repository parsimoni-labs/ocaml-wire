(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router using Wire's
    staged Codec.get -- all field access is generated from the Wire DSL. The C
    baseline uses EverParse-generated validators for field extraction. *)

open Bench_lib
open Routing_workload

external c_apid_route : bytes -> int -> int -> int = "c_apid_route"

external c_apid_route_counts : bytes -> int -> int -> int * int * int * int
  = "c_apid_route_counts"

let benchmark ~n_pkts =
  let buf, total_bytes, payload_bytes = generate_stream n_pkts in
  let st = state () in
  let step = step ~buf ~total_bytes st in
  let ocaml_result () = run_packets ~buf ~total_bytes st n_pkts in
  let c_result () = c_apid_route_counts buf 0 n_pkts in
  let t =
    v "Wire OCaml" ~size:hdr ~reset:(fun () -> reset st) step
    |> with_c c_apid_route buf
    |> with_expect ~equal:( = ) ~pp:pp_counts ~c:c_result ocaml_result
  in
  (t, payload_bytes, st, buf)

let check ~n_pkts =
  let t, _, _, _ = benchmark ~n_pkts in
  Bench_lib.check t

let main () =
  Memtrace.trace_if_requested ~context:"routing" ();
  let n_pkts =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 10_000_000
  in
  let t, payload_bytes, st, buf = benchmark ~n_pkts in
  Fmt.pr "APID demux (%d packets, %d MB stream, %d MB payload)\n\n" n_pkts
    (Bytes.length buf / 1_000_000)
    (payload_bytes / 1_000_000);
  run_table ~title:"APID routing" ~n:n_pkts ~unit:"pkt" [ t ];
  reset st;
  let ocaml_counts =
    run_packets ~buf ~total_bytes:(Bytes.length buf) st n_pkts
  in
  let c_counts = c_apid_route_counts buf 0 n_pkts in
  Fmt.pr "\n  OCaml: %a\n" pp_counts ocaml_counts;
  Fmt.pr "  C:     %a\n" pp_counts c_counts
