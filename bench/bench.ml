(** Wire Codec Benchmark: field-level read/write performance.

    Compares three approaches for accessing fields in binary protocol headers:
    - EverParse C: generated verified C validator (tight C loop)
    - OCaml->C FFI: calling the C validator from OCaml
    - Pure OCaml: Wire.Codec.get/set (zero-copy field access)

    Usage: BUILD_EVERPARSE=1 dune exec bench/bench.exe [-- ITERATIONS] *)

open Wire

(* ── Timing and allocation ── *)

let time_ns n f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. float_of_int n

let alloc_words n f =
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  let after = (Gc.quick_stat ()).minor_words in
  (after -. before) /. float_of_int n

(* ── Table formatting ── *)

let table_header title cols =
  Fmt.pr "\n%s\n%s\n\n" title (String.make (String.length title) '=');
  let widths =
    List.map (fun (name, w) -> (name, max w (String.length name))) cols
  in
  List.iter (fun (name, w) -> Fmt.pr "  %-*s" w name) widths;
  Fmt.pr "\n";
  List.iter (fun (_, w) -> Fmt.pr "  %s" (String.make w '-')) widths;
  Fmt.pr "\n";
  widths

let table_row widths cells =
  List.iter2 (fun (_, w) cell -> Fmt.pr "  %-*s" w cell) widths cells;
  Fmt.pr "\n"

(* ── Benchmark helpers ── *)

let bench_c_loop loop buf off n =
  let total = n * 10 in
  let ns = loop buf off total in
  float_of_int ns /. float_of_int total

let bench_ffi n check buf =
  time_ns n (fun () ->
      for _ = 1 to n do
        ignore (check buf)
      done)

let bench_op n f =
  time_ns n (fun () ->
      for _ = 1 to n do
        f ()
      done)

let ns_fmt t = Fmt.str "%.1f" t
let alloc_fmt w = if w < 0.5 then "0w" else Fmt.str "%.0fw" w

let ratio_fmt num denom =
  if denom > 0.1 then Fmt.str "%.1fx" (denom /. num) else "-"

(* ── Test data ── *)

let minimal_buf = (Demo.minimal_data 1).(0)
let bf8_buf = (Demo.bf8_data 1).(0)
let bf16_buf = (Demo.bf16_data 1).(0)
let bool_buf = (Demo.bool_fields_data 1).(0)
let bf32_buf = (Demo.bf32_data 1).(0)
let ints_buf = (Demo.all_ints_data 1).(0)
let mixed_buf = (Demo.large_mixed_data 1).(0)
let clcw_buf = (Space.clcw_data 1).(0)
let tcp_buf = (Net.tcp_frame_data 1).(0)

(* Sub-offsets for nested protocol access *)
let ip_off = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0
let tcp_off = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip_off

(* Isolated buffers for flat EverParse validation *)
let ipv4_only_buf = Bytes.sub tcp_buf 14 Net.ipv4_size
let tcp_only_buf = Bytes.sub tcp_buf 34 Net.tcp_size

(* ── Main ── *)

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 10_000_000
  in

  Fmt.pr "Wire Codec Benchmark\n";
  Fmt.pr "====================\n";
  Fmt.pr "EverParse C validation vs OCaml->C FFI vs pure OCaml field access.\n";
  Fmt.pr "All schemas defined once in the Wire DSL.\n";

  (* ── Read: field access ── *)
  let widths =
    table_header "Read: field access (ns/op)"
      [
        ("Field", 40);
        ("Size", 5);
        ("EverParse C", 11);
        ("OCaml->C FFI", 12);
        ("OCaml get", 9);
        ("alloc", 5);
        ("vs C", 5);
      ]
  in

  let read_row label size c_ns ffi_ns get_ns alloc =
    table_row widths
      [
        label;
        Fmt.str "%dB" size;
        (match c_ns with Some t -> ns_fmt t | None -> "-");
        (match ffi_ns with Some t -> ns_fmt t | None -> "-");
        ns_fmt get_ns;
        alloc_fmt alloc;
        (match c_ns with Some c -> ratio_fmt get_ns c | None -> "-");
      ]
  in

  (* Helper for flat field benchmarks with EverParse comparison *)
  let bench_flat label ~size ~c_loop_fn ~check_fn ~buf ~get_fn =
    let c = bench_c_loop c_loop_fn buf 0 n in
    let ffi = bench_ffi n check_fn buf in
    let get = bench_op n get_fn in
    let alloc = alloc_words n get_fn in
    read_row label size (Some c) (Some ffi) get alloc
  in

  (* Demo: Minimal.value — uint8, 1B struct *)
  bench_flat "Minimal.value (uint8)" ~size:Demo.minimal_size
    ~c_loop_fn:C_stubs.minimal_loop ~check_fn:C_stubs.minimal_check
    ~buf:minimal_buf ~get_fn:(fun () ->
      ignore (Codec.get Demo.minimal_codec Demo.f_minimal_value minimal_buf 0));

  (* Demo: Bitfield8.value — bf5 in bf_uint8, 1B struct *)
  bench_flat "Bitfield8.value (bf5 in bf_uint8)" ~size:Demo.bf8_size
    ~c_loop_fn:C_stubs.bitfield8_loop ~check_fn:C_stubs.bitfield8_check
    ~buf:bf8_buf ~get_fn:(fun () ->
      ignore (Codec.get Demo.bf8_codec Demo.f_bf8_value bf8_buf 0));

  (* Demo: Bitfield16.id — bf11 in bf_uint16be, 2B struct *)
  bench_flat "Bitfield16.id (bf11 in bf_uint16be)" ~size:Demo.bf16_size
    ~c_loop_fn:C_stubs.bitfield16_loop ~check_fn:C_stubs.bitfield16_check
    ~buf:bf16_buf ~get_fn:(fun () ->
      ignore (Codec.get Demo.bf16_codec Demo.f_bf16_id bf16_buf 0));

  (* Demo: BoolFields.active — bool(bf1) in bf_uint8, 2B struct *)
  bench_flat "BoolFields.active (bool bf1 in bf_uint8)"
    ~size:Demo.bool_fields_size ~c_loop_fn:C_stubs.boolfields_loop
    ~check_fn:C_stubs.boolfields_check ~buf:bool_buf ~get_fn:(fun () ->
      ignore (Codec.get Demo.bool_fields_codec Demo.f_bool_active bool_buf 0));

  (* Demo: Bitfield32.pri — bf8 in bf_uint32be, 4B struct *)
  bench_flat "Bitfield32.pri (bf8 in bf_uint32be)" ~size:Demo.bf32_size
    ~c_loop_fn:C_stubs.bitfield32_loop ~check_fn:C_stubs.bitfield32_check
    ~buf:bf32_buf ~get_fn:(fun () ->
      ignore (Codec.get Demo.bf32_codec Demo.f_bf32_pri bf32_buf 0));

  (* Space: CLCW.report — bf8 in bf_uint32be, 4B struct (real protocol) *)
  bench_flat "CLCW.report (bf8 in bf32be)" ~size:Space.clcw_size
    ~c_loop_fn:C_stubs.clcw_loop ~check_fn:C_stubs.clcw_check ~buf:clcw_buf
    ~get_fn:(fun () ->
      ignore (Codec.get Space.clcw_codec Space.cw_report clcw_buf 0));

  (* Net: TCP.dst_port — uint16be, 20B struct *)
  bench_flat "TCP.dst_port (uint16be)" ~size:Net.tcp_size
    ~c_loop_fn:(fun buf _off n -> C_stubs.tcp_loop buf 0 n)
    ~check_fn:C_stubs.tcp_check ~buf:tcp_only_buf
    ~get_fn:(fun () ->
      ignore (Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp_off));

  (* Net: TCP.syn — bool(bf1) in bf_uint16be flags, 20B struct *)
  let tcp_c = bench_c_loop C_stubs.tcp_loop tcp_only_buf 0 n in
  let tcp_ffi = bench_ffi n C_stubs.tcp_check tcp_only_buf in
  let tcp_syn_get =
    bench_op n (fun () ->
        ignore (Codec.get Net.tcp_codec Net.f_tcp_syn tcp_buf tcp_off))
  in
  let tcp_syn_alloc =
    alloc_words n (fun () ->
        ignore (Codec.get Net.tcp_codec Net.f_tcp_syn tcp_buf tcp_off))
  in
  read_row "TCP.syn (bool bf1 in bf16be)" Net.tcp_size (Some tcp_c)
    (Some tcp_ffi) tcp_syn_get tcp_syn_alloc;

  (* Demo: AllInts.u64be — uint64be (boxed), 21B struct *)
  bench_flat "AllInts.u64be (uint64be, boxed)" ~size:Demo.all_ints_size
    ~c_loop_fn:C_stubs.allints_loop ~check_fn:C_stubs.allints_check
    ~buf:ints_buf ~get_fn:(fun () ->
      ignore (Codec.get Demo.all_ints_codec Demo.f_ints_u64be ints_buf 0));

  (* Demo: LargeMixed.timestamp — uint64be, 26B struct, last of 10 fields *)
  bench_flat "LargeMixed.timestamp (uint64be, 26B)" ~size:Demo.large_mixed_size
    ~c_loop_fn:C_stubs.largemixed_loop ~check_fn:C_stubs.largemixed_check
    ~buf:mixed_buf ~get_fn:(fun () ->
      ignore
        (Codec.get Demo.large_mixed_codec Demo.f_mixed_timestamp mixed_buf 0));

  (* Net: IPv4.src — uint32be (unboxed), 40B struct *)
  bench_flat "IPv4.src (uint32be, unboxed)" ~size:Net.ipv4_size
    ~c_loop_fn:(fun buf _off n -> C_stubs.ipv4_loop buf 0 n)
    ~check_fn:C_stubs.ipv4_check ~buf:ipv4_only_buf
    ~get_fn:(fun () ->
      ignore (Codec.get Net.ipv4_codec Net.f_ip_src tcp_buf ip_off));

  Fmt.pr "\n";

  (* Nested: Eth -> IPv4 -> TCP.dst_port (3-layer traversal) *)
  let nested_dst_get =
    bench_op n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        ignore (Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp))
  in
  let nested_dst_alloc =
    alloc_words n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        ignore (Codec.get Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp))
  in
  read_row "Eth->IPv4->TCP.dst_port (3 layers)" Net.ethernet_size None None
    nested_dst_get nested_dst_alloc;

  (* Nested: Eth -> IPv4 -> TCP.syn (3-layer traversal, bitfield) *)
  let nested_syn_get =
    bench_op n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        ignore (Codec.get Net.tcp_codec Net.f_tcp_syn tcp_buf tcp))
  in
  let nested_syn_alloc =
    alloc_words n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        ignore (Codec.get Net.tcp_codec Net.f_tcp_syn tcp_buf tcp))
  in
  read_row "Eth->IPv4->TCP.syn (3 layers)" Net.ethernet_size None None
    nested_syn_get nested_syn_alloc;

  (* ── Write: in-place field mutation ── *)
  let widths =
    table_header "Write: in-place field mutation (ns/op)"
      [ ("Field", 40); ("OCaml set", 9); ("alloc", 5) ]
  in

  let write_row label set_ns alloc =
    table_row widths [ label; ns_fmt set_ns; alloc_fmt alloc ]
  in

  (* Minimal.value *)
  let v =
    bench_op n (fun () ->
        Codec.set Demo.minimal_codec Demo.f_minimal_value minimal_buf 0 42)
  in
  let a =
    alloc_words n (fun () ->
        Codec.set Demo.minimal_codec Demo.f_minimal_value minimal_buf 0 42)
  in
  write_row "Minimal.value (uint8)" v a;

  (* Bitfield8.value *)
  let v =
    bench_op n (fun () ->
        Codec.set Demo.bf8_codec Demo.f_bf8_value bf8_buf 0 19)
  in
  let a =
    alloc_words n (fun () ->
        Codec.set Demo.bf8_codec Demo.f_bf8_value bf8_buf 0 19)
  in
  write_row "Bitfield8.value (bf5, read-mod-write)" v a;

  (* BoolFields.active *)
  let v =
    bench_op n (fun () ->
        Codec.set Demo.bool_fields_codec Demo.f_bool_active bool_buf 0 true)
  in
  let a =
    alloc_words n (fun () ->
        Codec.set Demo.bool_fields_codec Demo.f_bool_active bool_buf 0 true)
  in
  write_row "BoolFields.active (bool bf1, rmw)" v a;

  (* CLCW.report: read-modify-write bitfield *)
  let v =
    bench_op n (fun () ->
        Codec.set Space.clcw_codec Space.cw_report clcw_buf 0 42)
  in
  let a =
    alloc_words n (fun () ->
        Codec.set Space.clcw_codec Space.cw_report clcw_buf 0 42)
  in
  write_row "CLCW.report (bf8, read-mod-write)" v a;

  (* TCP.dst_port: direct uint16be write *)
  let v =
    bench_op n (fun () ->
        Codec.set Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp_off 8080)
  in
  let a =
    alloc_words n (fun () ->
        Codec.set Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp_off 8080)
  in
  write_row "TCP.dst_port (uint16be)" v a;

  (* TCP.syn: read-modify-write bool bitfield *)
  let v =
    bench_op n (fun () ->
        Codec.set Net.tcp_codec Net.f_tcp_syn tcp_buf tcp_off true)
  in
  let a =
    alloc_words n (fun () ->
        Codec.set Net.tcp_codec Net.f_tcp_syn tcp_buf tcp_off true)
  in
  write_row "TCP.syn (bool bf1, read-mod-write)" v a;

  Fmt.pr "\n";

  (* Nested writes: 3-layer traversal + set *)
  let v =
    bench_op n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        Codec.set Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp 8080)
  in
  let a =
    alloc_words n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        Codec.set Net.tcp_codec Net.f_tcp_dst_port tcp_buf tcp 8080)
  in
  write_row "Eth->TCP.dst_port (3 layers)" v a;

  let v =
    bench_op n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        Codec.set Net.tcp_codec Net.f_tcp_syn tcp_buf tcp true)
  in
  let a =
    alloc_words n (fun () ->
        let ip = Codec.sub Net.ethernet_codec Net.f_eth_payload tcp_buf 0 in
        let tcp = Codec.sub Net.ipv4_codec Net.f_ip_payload tcp_buf ip in
        Codec.set Net.tcp_codec Net.f_tcp_syn tcp_buf tcp true)
  in
  write_row "Eth->TCP.syn (3 layers)" v a;

  (* ── FFI overhead ── *)
  let buf4 = Bytes.create 4 in
  let noop_ns = bench_op n (fun () -> ignore (C_stubs.noop buf4)) in
  let noop_safe_ns = bench_op n (fun () -> ignore (C_stubs.noop_safe buf4)) in
  Fmt.pr "\nFFI overhead:\n";
  Fmt.pr "  noop [@@noalloc]  %5.1f ns/call\n" noop_ns;
  Fmt.pr "  noop CAMLparam    %5.1f ns/call\n" noop_safe_ns;
  Fmt.pr "\n"
