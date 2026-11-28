(** TM frame reassembly benchmark.

    Simulates a downlink TM frame processor, each header checked by Wire's
    [Codec.View.check] and its fields read with [Codec.View.read] -- all field
    access is generated from the Wire DSL. The C baseline uses
    EverParse-generated validators for field extraction. *)

open Bench_lib
open Gateway_workload

external c_tm_reassemble : bytes -> int -> int -> int = "c_tm_reassemble"

external c_tm_reassemble_checksum : bytes -> int -> int
  = "c_tm_reassemble_checksum"

let pp_checksum ppf h = Fmt.pf ppf "%u" h

let benchmark ~n_frames =
  let buf, total_pkts = generate_frames n_frames in
  let st = state buf in
  let ocaml_result () = checksum st in
  let c_result () = c_tm_reassemble_checksum buf 0 in
  let t =
    v "Wire OCaml" ~size:cadu_size ~reset:(fun () -> reset st) (step st)
    |> with_c c_tm_reassemble buf
    |> with_expect ~equal:Int.equal ~pp:pp_checksum ~c:c_result ocaml_result
  in
  (t, st, buf, total_pkts)

let check ~n_frames =
  let t, _, _, _ = benchmark ~n_frames in
  Bench_lib.check t

let main () =
  let n_frames =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000
  in
  let t, st, buf, total_pkts = benchmark ~n_frames in
  Bench_lib.check t;
  let ocaml_checksum = checksum st in
  let c_checksum = c_tm_reassemble_checksum buf 0 in
  Fmt.pr
    "TM frame reassembly (%d frames, %d-byte CADUs, %d embedded packets)\n\n"
    n_frames cadu_size total_pkts;
  Memtrace.trace_if_requested ~context:"gateway" ();
  run_table ~prechecked:true ~title:"TM frame reassembly" ~n:n_frames
    ~unit:"frm" [ t ];
  Fmt.pr "\n  OCaml checksum: 0x%x\n  C checksum:     0x%x\n" ocaml_checksum
    c_checksum
