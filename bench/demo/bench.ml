(** Wire Codec Benchmark: field-level read/write performance.

    Compares three tiers for accessing fields in binary protocol headers, all
    derived from the same Wire DSL definition: 1. EverParse C -- verified C
    validator in a tight C loop 2. OCaml->C FFI -- calling EverParse from OCaml
    3. Pure OCaml -- Wire.Codec.get/set (zero-copy field access)

    Usage: BUILD_EVERPARSE=1 dune exec bench/demo/bench.exe [-- ITERATIONS] *)

open Wire
open Bench_lib
module Slice = Bytesrw.Bytes.Slice

(* ── Bound fields ── *)

let cf_minimal_value = Codec.(Demo.f_minimal_value $ fun m -> m.Demo.m_value)
let cf_bf8_value = Codec.(Demo.f_bf8_value $ fun b -> b.Demo.bf8_value)
let cf_bf16_id = Codec.(Demo.f_bf16_id $ fun b -> b.Demo.bf16_id)
let cf_bool_active = Codec.(Demo.f_bool_active $ fun b -> b.Demo.bl_active)
let cf_bf32_pri = Codec.(Demo.f_bf32_pri $ fun b -> b.Demo.bf32_pri)
let cf_ints_u64be = Codec.(Demo.f_ints_u64be $ fun a -> a.Demo.ai_u64be)

let cf_mixed_timestamp =
  Codec.(Demo.f_mixed_timestamp $ fun l -> l.Demo.lg_timestamp)

let cf_mp_priority = Codec.(Demo.f_mp_priority $ fun m -> m.Demo.mp_priority)
let cf_cd_type = Codec.(Demo.f_cd_type $ fun c -> c.Demo.cd_type)
let cf_en_status = Codec.(Demo.f_en_status $ fun e -> e.Demo.en_status)
let cf_co_data = Codec.(Demo.f_co_data $ fun c -> c.Demo.co_data)
let cf_cw_report = Space.bf_cw_report
let cf_ip_src = Net.bf_ip_src
let cf_tcp_dst_port = Net.bf_tcp_dst_port
let cf_tcp_syn = Net.bf_tcp_syn
let cf_tcp_src_port = Net.bf_tcp_src_port
let cf_eth_payload = Net.bf_eth_payload
let cf_ip_payload = Net.bf_ip_payload
let n_data = 1024

(* ── Contiguous test data ── *)

let minimal_buf, n_items =
  pack (Demo.minimal_data n_data) ~size:Demo.minimal_size

let bf8_buf, _ = pack (Demo.bf8_data n_data) ~size:Demo.bf8_size
let bf16_buf, _ = pack (Demo.bf16_data n_data) ~size:Demo.bf16_size

let bool_buf, _ =
  pack (Demo.bool_fields_data n_data) ~size:Demo.bool_fields_size

let bf32_buf, _ = pack (Demo.bf32_data n_data) ~size:Demo.bf32_size
let ints_buf, _ = pack (Demo.all_ints_data n_data) ~size:Demo.all_ints_size

let mixed_buf, _ =
  pack (Demo.large_mixed_data n_data) ~size:Demo.large_mixed_size

let clcw_buf, _ = pack (Space.clcw_data n_data) ~size:Space.clcw_size
let mapped_buf = Demo.mapped_data n_data
let cases_buf = Demo.cases_demo_data n_data
let enum_buf = Demo.enum_demo_data n_data
let constrained_buf = Demo.constrained_data n_data

(* Nested: single Ethernet->IPv4->TCP frame *)
let tcp_buf = (Net.tcp_frame_data 1).(0)

let ip_off =
  Slice.first
    ((Staged.unstage (Codec.get Net.ethernet_codec cf_eth_payload)) tcp_buf 0)

let tcp_off =
  Slice.first
    ((Staged.unstage (Codec.get Net.ipv4_codec cf_ip_payload)) tcp_buf ip_off)

let ipv4_only_buf = Bytes.sub tcp_buf 14 Net.ipv4_size
let tcp_only_buf = Bytes.sub tcp_buf 34 Net.tcp_size

(* ── Pre-staged accessors ── *)

let get_minimal = Staged.unstage (Codec.get Demo.minimal_codec cf_minimal_value)
let read_bf8 = Staged.unstage (Codec.get Demo.bf8_codec cf_bf8_value)
let read_bf16 = Staged.unstage (Codec.get Demo.bf16_codec cf_bf16_id)
let read_bool = Staged.unstage (Codec.get Demo.bool_fields_codec cf_bool_active)
let read_bf32 = Staged.unstage (Codec.get Demo.bf32_codec cf_bf32_pri)
let read_u64be = Staged.unstage (Codec.get Demo.all_ints_codec cf_ints_u64be)

let read_mixed =
  Staged.unstage (Codec.get Demo.large_mixed_codec cf_mixed_timestamp)

let read_clcw = Staged.unstage (Codec.get Space.clcw_codec cf_cw_report)
let read_ip_src = Staged.unstage (Codec.get Net.ipv4_codec cf_ip_src)
let read_tcp_port = Staged.unstage (Codec.get Net.tcp_codec cf_tcp_dst_port)
let read_tcp_syn = Staged.unstage (Codec.get Net.tcp_codec cf_tcp_syn)
let read_mapped = Staged.unstage (Codec.get Demo.mapped_codec cf_mp_priority)
let read_cases = Staged.unstage (Codec.get Demo.cases_demo_codec cf_cd_type)
let read_enum = Staged.unstage (Codec.get Demo.enum_demo_codec cf_en_status)

let read_constrained =
  Staged.unstage (Codec.get Demo.constrained_codec cf_co_data)

let read_eth_payload =
  Staged.unstage (Codec.get Net.ethernet_codec cf_eth_payload)

let read_ip_payload = Staged.unstage (Codec.get Net.ipv4_codec cf_ip_payload)
let set_minimal = Staged.unstage (Codec.set Demo.minimal_codec cf_minimal_value)
let set_bf8 = Staged.unstage (Codec.set Demo.bf8_codec cf_bf8_value)
let set_bool = Staged.unstage (Codec.set Demo.bool_fields_codec cf_bool_active)
let set_clcw = Staged.unstage (Codec.set Space.clcw_codec cf_cw_report)
let set_tcp_port = Staged.unstage (Codec.set Net.tcp_codec cf_tcp_dst_port)
let set_tcp_syn = Staged.unstage (Codec.set Net.tcp_codec cf_tcp_syn)
let set_mapped = Staged.unstage (Codec.set Demo.mapped_codec cf_mp_priority)
let set_cases = Staged.unstage (Codec.set Demo.cases_demo_codec cf_cd_type)

(* ── Benchmark helpers ── *)

let rd ~label ~size ~data ?(n_items = n_items) ~c_loop ~ffi_check read_fn =
  let fn, reset =
    cycling ~data ~n_items ~size (fun buf off -> read_fn buf off)
  in
  v label ~size ~reset fn |> with_c c_loop data
  |> with_ffi ffi_check (Bytes.sub data 0 size)

(* ── Main ── *)

let () =
  Memtrace.trace_if_requested ~context:"demo" ();
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 10_000_000
  in

  Fmt.pr "Wire Codec Benchmark\n";
  Fmt.pr "====================\n";
  Fmt.pr "EverParse C vs OCaml->C FFI vs pure OCaml. Same Wire DSL.\n";

  (* ── Read benchmarks ── *)
  run_table ~title:"Read: field access (ns/op)" ~n
    [
      (* Integer types *)
      rd ~label:"Minimal.value (uint8)" ~size:Demo.minimal_size
        ~data:minimal_buf ~c_loop:C_stubs.minimal_loop
        ~ffi_check:C_stubs.minimal_check (fun buf off ->
          ignore (get_minimal buf off));
      rd ~label:"AllInts.u64be (uint64be, boxed)" ~size:Demo.all_ints_size
        ~data:ints_buf ~c_loop:C_stubs.allints_loop
        ~ffi_check:C_stubs.allints_check (fun buf off ->
          ignore (read_u64be buf off));
      rd ~label:"LargeMixed.timestamp (uint64be, 26B)"
        ~size:Demo.large_mixed_size ~data:mixed_buf
        ~c_loop:C_stubs.largemixed_loop ~ffi_check:C_stubs.largemixed_check
        (fun buf off -> ignore (read_mixed buf off));
      (* Bitfields *)
      rd ~label:"Bitfield8.value (bf5 in bf_uint8)" ~size:Demo.bf8_size
        ~data:bf8_buf ~c_loop:C_stubs.bitfield8_loop
        ~ffi_check:C_stubs.bitfield8_check (fun buf off ->
          ignore (read_bf8 buf off));
      rd ~label:"Bitfield16.id (bf11 in bf_uint16be)" ~size:Demo.bf16_size
        ~data:bf16_buf ~c_loop:C_stubs.bitfield16_loop
        ~ffi_check:C_stubs.bitfield16_check (fun buf off ->
          ignore (read_bf16 buf off));
      rd ~label:"Bitfield32.pri (bf8 in bf_uint32be)" ~size:Demo.bf32_size
        ~data:bf32_buf ~c_loop:C_stubs.bitfield32_loop
        ~ffi_check:C_stubs.bitfield32_check (fun buf off ->
          ignore (read_bf32 buf off));
      (* Bool (map) *)
      rd ~label:"BoolFields.active (bool bf1 in bf_uint8)"
        ~size:Demo.bool_fields_size ~data:bool_buf
        ~c_loop:C_stubs.boolfields_loop ~ffi_check:C_stubs.boolfields_check
        (fun buf off -> ignore (read_bool buf off));
      (* Real protocols *)
      rd ~label:"CLCW.report (bf8 in bf32be)" ~size:Space.clcw_size
        ~data:clcw_buf ~c_loop:C_stubs.clcw_loop ~ffi_check:C_stubs.clcw_check
        (fun buf off -> ignore (read_clcw buf off));
      rd ~label:"IPv4.src (uint32be, unboxed)" ~size:Net.ipv4_size
        ~data:ipv4_only_buf ~n_items:1
        ~c_loop:(fun buf _off n -> C_stubs.ipv4_loop buf 0 n)
        ~ffi_check:C_stubs.ipv4_check
        (fun _buf _off -> ignore (read_ip_src tcp_buf ip_off));
      rd ~label:"TCP.dst_port (uint16be)" ~size:Net.tcp_size ~data:tcp_only_buf
        ~n_items:1
        ~c_loop:(fun buf _off n -> C_stubs.tcp_loop buf 0 n)
        ~ffi_check:C_stubs.tcp_check
        (fun _buf _off -> ignore (read_tcp_port tcp_buf tcp_off));
      rd ~label:"TCP.syn (bool bf1 in bf16be)" ~size:Net.tcp_size
        ~data:tcp_only_buf ~n_items:1
        ~c_loop:(fun buf _off n -> C_stubs.tcp_loop buf 0 n)
        ~ffi_check:C_stubs.tcp_check
        (fun _buf _off -> ignore (read_tcp_syn tcp_buf tcp_off));
    ];

  section "Type combinators";

  run_table ~title:"Read: type combinators (ns/op)" ~n
    [
      (* map: user decode function on every Codec.get *)
      rd ~label:"Mapped.priority (map fn, 2B)" ~size:Demo.mapped_size
        ~data:mapped_buf ~c_loop:C_stubs.mapped_loop
        ~ffi_check:C_stubs.mapped_check (fun buf off ->
          ignore (read_mapped buf off));
      (* cases: variant dispatch via array lookup *)
      rd ~label:"CasesDemo.type (cases variant, 1B)" ~size:Demo.cases_demo_size
        ~data:cases_buf ~c_loop:C_stubs.casesdemo_loop
        ~ffi_check:C_stubs.casesdemo_check (fun buf off ->
          ignore (read_cases buf off));
      (* enum + map: C validates enum, OCaml decodes variant *)
      rd ~label:"EnumDemo.status (enum+map variant, 2B)"
        ~size:Demo.enum_demo_size ~data:enum_buf ~c_loop:C_stubs.enumdemo_loop
        ~ffi_check:C_stubs.enumdemo_check (fun buf off ->
          ignore (read_enum buf off));
      (* where: C validates constraint, OCaml skips it *)
      rd ~label:"Constrained.data (where, 2B)" ~size:Demo.constrained_size
        ~data:constrained_buf ~c_loop:C_stubs.constrained_loop
        ~ffi_check:C_stubs.constrained_check (fun buf off ->
          ignore (read_constrained buf off));
    ];

  (* ── Write benchmarks ── *)
  run_table ~title:"Write: in-place field mutation (ns/op)" ~n
    [
      v "Minimal.value (uint8)" ~size:0 (fun () -> set_minimal minimal_buf 0 42);
      v "Bitfield8.value (bf5, read-mod-write)" ~size:0 (fun () ->
          set_bf8 bf8_buf 0 19);
      v "BoolFields.active (bool bf1, rmw)" ~size:0 (fun () ->
          set_bool bool_buf 0 true);
      v "CLCW.report (bf8, read-mod-write)" ~size:0 (fun () ->
          set_clcw clcw_buf 0 42);
      v "TCP.dst_port (uint16be)" ~size:0 (fun () ->
          set_tcp_port tcp_buf tcp_off 8080);
      v "TCP.syn (bool bf1, read-mod-write)" ~size:0 (fun () ->
          set_tcp_syn tcp_buf tcp_off true);
      v "Mapped.priority (map fn encode)" ~size:0 (fun () ->
          set_mapped mapped_buf 0 Demo.High);
      v "CasesDemo.type (cases variant encode)" ~size:0 (fun () ->
          set_cases cases_buf 0 Demo.Telemetry);
      v "Eth->TCP.dst_port (3 layers)" ~size:0 (fun () ->
          let ip = Slice.first (read_eth_payload tcp_buf 0) in
          let tcp = Slice.first (read_ip_payload tcp_buf ip) in
          set_tcp_port tcp_buf tcp 8080);
      v "Eth->TCP.syn (3 layers)" ~size:0 (fun () ->
          let ip = Slice.first (read_eth_payload tcp_buf 0) in
          let tcp = Slice.first (read_ip_payload tcp_buf ip) in
          set_tcp_syn tcp_buf tcp true);
    ];

  (* ── FFI overhead ── *)
  let buf4 = Bytes.create 4 in
  let noop_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (C_stubs.noop buf4)
        done)
  in
  let noop_safe_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (C_stubs.noop_safe buf4)
        done)
  in
  Fmt.pr "\nFFI overhead:\n";
  Fmt.pr "  noop [@@noalloc]  %5.1f ns/call\n" noop_ns;
  Fmt.pr "  noop CAMLparam    %5.1f ns/call\n" noop_safe_ns;
  Fmt.pr "\n"
