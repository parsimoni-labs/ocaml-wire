(** Performance comparison: EverParse C vs OCaml Wire.Codec vs OCaml->C FFI.

    All three paths use the same schema definitions from bench_schemas.ml: 1.
    EverParse C -- generated verified C validator, timed in pure C loop 2. OCaml
    Codec -- pure OCaml Wire.Codec.decode 3. OCaml->C FFI -- OCaml calling
    EverParse C validator via generated stubs

    Usage: BUILD_EVERPARSE=1 dune exec bench/bench_perf.exe [-- ITERATIONS]

    Default: 1_000_000 iterations. *)

open Bench_schemas

(* ── Timing ── *)

let time_ns f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9

(* ── OCaml Codec benchmark (decode = full parse) ── *)

let bench_ocaml_decode (type a) (s : a schema) n =
  let data = s.make_data 1 in
  let buf = data.(0) in
  let ns =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (s.decode buf 0)
        done)
  in
  ns /. float_of_int n

(* ── Zero-copy get benchmark (single field, no record alloc) ── *)

let bench_zero_copy_get (fld : (int, _) Wire.Codec.field) (s : _ schema) n =
  let data = s.make_data 1 in
  let buf = data.(0) in
  let slice =
    Bytesrw.Bytes.Slice.make buf ~first:0 ~length:(Bytes.length buf)
  in
  let ns =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (Wire.Codec.get fld slice)
        done)
  in
  ns /. float_of_int n

(* ── EverParse C FFI benchmark (validate via OCaml->C call) ── *)

let bench_ffi_check (check : bytes -> bool) (s : _ schema) n =
  let data = s.make_data 1 in
  let buf = data.(0) in
  let ns =
    time_ns (fun () ->
        for _ = 1 to n do
          ignore (check buf)
        done)
  in
  ns /. float_of_int n

(* ── Pure C EverParse loop benchmark (validate in tight C loop) ── *)

let bench_c_loop (loop : bytes -> int -> int -> int) (s : _ schema) n =
  let data = s.make_data 1 in
  let buf = data.(0) in
  let ns = loop buf 0 n in
  float_of_int ns /. float_of_int n

(* ── Output formatting ── *)

let header () =
  Printf.printf
    "Wire Codec Performance: EverParse C vs OCaml vs C FFI\n\
     ======================================================\n\
     All schemas defined once in OCaml Wire DSL.\n\n";
  Printf.printf "%-14s %4s  %-14s %10s %10s\n" "Schema" "Size" "Method"
    "Validate" "vs C";
  Printf.printf "%-14s %4s  %-14s %10s %10s\n" "" "" "" "ns/op" "";
  Printf.printf "%s\n" (String.make 58 '-')

let row ?(schema = "") ?(size = 0) method_ time c_baseline =
  let ratio =
    if c_baseline > 0.1 then Printf.sprintf "%.1fx" (time /. c_baseline)
    else "-"
  in
  if schema <> "" then
    Printf.printf "%-14s %3dB  %-14s %8.1f %10s\n" schema size method_ time
      ratio
  else Printf.printf "%-14s %4s  %-14s %8.1f %10s\n" "" "" method_ time ratio

let bench_one (Any s) n ~c_loop_fn ~ffi_check_fn ~zero_copy_time =
  let c_time = bench_c_loop c_loop_fn s (n * 10) in
  let ocaml_time = bench_ocaml_decode s n in
  let ffi_time = bench_ffi_check ffi_check_fn s n in

  row ~schema:s.name ~size:s.size "EverParse C" c_time c_time;
  row "OCaml Codec" ocaml_time c_time;
  row "OCaml->C FFI" ffi_time c_time;
  (match zero_copy_time with
  | Some zc_time -> row "Zero-copy get" zc_time c_time
  | None -> ());
  Printf.printf "\n"

(* ── Main ── *)

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1_000_000
  in

  header ();

  List.iter
    (fun (Any s as any) ->
      let lower = String.lowercase_ascii s.name in
      let c_loop_fn = Bench_ep_dispatch.get_loop lower in
      let ffi_check_fn = Bench_ep_dispatch.get_check lower in
      let zero_copy_time =
        if s.name = "CLCW" then Some (bench_zero_copy_get cw_report s n)
        else None
      in
      bench_one any n ~c_loop_fn ~ffi_check_fn ~zero_copy_time)
    all_schemas;

  (* FFI call overhead breakdown *)
  let buf = Bytes.create 4 in
  let noop_time =
    let ns =
      time_ns (fun () ->
          for _ = 1 to n do
            ignore (Bench_ep_stubs.noop buf)
          done)
    in
    ns /. float_of_int n
  in
  let noop_safe_time =
    let ns =
      time_ns (fun () ->
          for _ = 1 to n do
            ignore (Bench_ep_stubs.noop_safe buf)
          done)
    in
    ns /. float_of_int n
  in
  Printf.printf "FFI call overhead:\n";
  Printf.printf "  noop [@@noalloc]   %6.1f ns/call\n" noop_time;
  Printf.printf "  noop CAMLparam     %6.1f ns/call\n" noop_safe_time;
  Printf.printf "  CLCW FFI check     %6.1f ns/call\n"
    (let (Any s) = List.find (fun (Any s) -> s.name = "CLCW") all_schemas in
     bench_ffi_check (Bench_ep_dispatch.get_check "clcw") s n);
  Printf.printf "\n";

  Printf.printf
    "Legend:\n\
    \  EverParse C   = generated verified C validator, timed in pure C loop\n\
    \  OCaml Codec   = Wire.Codec.decode, pure OCaml (full parse)\n\
    \  OCaml->C FFI  = OCaml calling EverParse C validator via generated stubs\n\
    \  Zero-copy get = Wire.Codec.get, single field read, no record alloc\n"
