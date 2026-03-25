(** Wire Codec Benchmark: field-level read/write performance.

    Compares three tiers for accessing fields in binary protocol headers, all
    derived from the same Wire DSL definition: 1. EverParse C -- verified C
    validator in a tight C loop 2. OCaml->C FFI -- calling EverParse from OCaml
    3. Pure OCaml -- Wire.Codec.get/set (zero-copy field access) *)

open Bench_lib
open Demo_bench_cases

let c_loop_of_id = function
  | Minimal -> C_stubs.minimal_loop
  | All_ints -> C_stubs.allints_loop
  | Large_mixed -> C_stubs.largemixed_loop
  | Bitfield8 -> C_stubs.bitfield8_loop
  | Bitfield16 -> C_stubs.bitfield16_loop
  | Bitfield32 -> C_stubs.bitfield32_loop
  | Bool_fields -> C_stubs.boolfields_loop
  | Clcw_report -> C_stubs.clcwreport_loop
  | Space_packet_apid -> C_stubs.spacepacketapid_loop
  | Ipv4_src -> C_stubs.ipv4_loop
  | Tcp_dst_port -> C_stubs.tcp_loop
  | Tcp_syn -> C_stubs.tcpsyn_loop
  | Mapped_priority -> C_stubs.mapped_loop
  | Cases_type -> C_stubs.casesdemo_loop
  | Enum_status -> C_stubs.enumdemo_loop
  | Constrained_data -> C_stubs.constrained_loop

let ffi_parse_of_id = function
  | Minimal -> fun b -> ignore (C_stubs.minimal_parse b)
  | All_ints -> fun b -> ignore (C_stubs.allints_parse b)
  | Large_mixed -> fun b -> ignore (C_stubs.largemixed_parse b)
  | Bitfield8 -> fun b -> ignore (C_stubs.bitfield8_parse b)
  | Bitfield16 -> fun b -> ignore (C_stubs.bitfield16_parse b)
  | Bitfield32 -> fun b -> ignore (C_stubs.bitfield32_parse b)
  | Bool_fields -> fun b -> ignore (C_stubs.boolfields_parse b)
  | Clcw_report -> fun b -> ignore (C_stubs.clcwreport_parse b)
  | Space_packet_apid -> fun b -> ignore (C_stubs.spacepacketapid_parse b)
  | Ipv4_src -> fun b -> ignore (C_stubs.ipv4_parse b)
  | Tcp_dst_port -> fun b -> ignore (C_stubs.tcp_parse b)
  | Tcp_syn -> fun b -> ignore (C_stubs.tcpsyn_parse b)
  | Mapped_priority -> fun b -> ignore (C_stubs.mapped_parse b)
  | Cases_type -> fun b -> ignore (C_stubs.casesdemo_parse b)
  | Enum_status -> fun b -> ignore (C_stubs.enumdemo_parse b)
  | Constrained_data -> fun b -> ignore (C_stubs.constrained_parse b)

let read_row (Read_case case) =
  let ocaml_fn, ocaml_reset =
    cycling ~data:case.dataset.packed ~n_items:case.dataset.n_items
      ~size:case.size (fun buf off -> ignore (case.get buf off))
  in
  let ffi_index = ref 0 in
  let ffi_reset () = ffi_index := 0 in
  let ffi_parse = ffi_parse_of_id case.id in
  let ffi_fn _buf =
    let item = case.dataset.items.(!ffi_index mod case.dataset.n_items) in
    ffi_parse item;
    incr ffi_index
  in
  let reset () =
    ocaml_reset ();
    ffi_reset ()
  in
  v case.label ~size:case.size ~reset ocaml_fn
  |> with_c (c_loop_of_id case.id) case.dataset.packed
  |> with_ffi ffi_fn Bytes.empty
  |> with_verify (Demo_bench_diff.verify_of_id case.id)

let write_row case = v case.label ~size:0 case.run |> with_verify case.verify

let () =
  Memtrace.trace_if_requested ~context:"demo" ();
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 10_000_000
  in

  Fmt.pr "Wire Codec Benchmark\n";
  Fmt.pr "====================\n";
  Fmt.pr "All three tiers validate the same record sequence.\n";
  Fmt.pr "  C        = EverParse validate+extract selected field in C\n";
  Fmt.pr "  FFI      = same EverParse projection, called from OCaml\n";
  Fmt.pr "  OCaml    = Wire Codec.get on the same field\n";

  run_table ~title:"Read: projected field access (ns/op)" ~n
    (List.map read_row read_benchmark_cases);

  run_table ~title:"Write: in-place field mutation (ns/op)" ~n
    (List.map write_row write_benchmark_cases);

  Fmt.pr "\n"
