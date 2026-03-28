(** Wire Codec Benchmark: field-level read/write performance.

    Compares three tiers for accessing fields in binary protocol headers, all
    derived from the same Wire DSL definition: 1. EverParse C -- verified C
    validator in a tight C loop 2. OCaml->C FFI -- calling EverParse from OCaml
    3. Pure OCaml -- Wire.Codec.get/set (zero-copy field access) *)

open Bench_lib
open Demo_bench_cases

let read_row (C rc) =
  let (Read_case case) = rc in
  let stubs =
    C_stubs.stubs_of_name (Wire.Everparse.Raw.struct_name case.struct_)
  in
  let ocaml_fn, ocaml_reset =
    cycling ~data:case.dataset.packed ~n_items:case.dataset.n_items
      ~size:case.size (fun buf off -> ignore (case.get buf off))
  in
  let ffi_index = ref 0 in
  let ffi_reset () = ffi_index := 0 in
  let ffi_fn _buf =
    let item = case.dataset.items.(!ffi_index mod case.dataset.n_items) in
    stubs.ffi_parse item 0;
    incr ffi_index
  in
  let reset () =
    ocaml_reset ();
    ffi_reset ()
  in
  v case.label ~size:case.size ~reset ocaml_fn
  |> with_c stubs.loop case.dataset.packed
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
