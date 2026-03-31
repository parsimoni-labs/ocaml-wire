(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router using Wire's
    staged Codec.get — all field access is generated from the Wire DSL. The C
    baseline uses EverParse-generated validators for field extraction. *)

open Bench_lib
module C = Wire.Codec

external c_apid_route : bytes -> int -> int -> int = "c_apid_route"

external c_apid_route_counts : bytes -> int -> int -> int * int * int * int
  = "c_apid_route_counts"

let cf_sp_apid = Space.bf_sp_apid
let cf_sp_seq_count = Space.bf_sp_seq_count
let cf_sp_data_len = Space.bf_sp_data_len

let apid_of_index i =
  let r = i mod 100 in
  if r < 40 then i mod 256
  else if r < 75 then 256 + (i mod 768)
  else if r < 95 then 1024 + (i mod 512)
  else 0x7FF

let payload_size_of_apid apid =
  if apid < 256 then 32
  else if apid < 1024 then 256
  else if apid < 1536 then 64
  else 8

let generate_stream n =
  let total = ref 0 in
  let hdr = Wire.Codec.wire_size Space.packet_codec in
  for i = 0 to n - 1 do
    let apid = apid_of_index i in
    total := !total + hdr + payload_size_of_apid apid
  done;
  let buf = Bytes.create !total in
  let off = ref 0 in
  let payload_total = ref 0 in
  let set_apid = Wire.Staged.unstage (C.set Space.packet_codec cf_sp_apid) in
  let set_seq =
    Wire.Staged.unstage (C.set Space.packet_codec cf_sp_seq_count)
  in
  let set_dlen =
    Wire.Staged.unstage (C.set Space.packet_codec cf_sp_data_len)
  in
  for i = 0 to n - 1 do
    let apid = apid_of_index i in
    let plen = payload_size_of_apid apid in
    let o = !off in
    set_apid buf o apid;
    set_seq buf o (i mod 16384);
    set_dlen buf o (plen - 1);
    off := o + hdr + plen;
    payload_total := !payload_total + plen
  done;
  (buf, !total, !payload_total)

let n_apids = 2048 (* 11-bit APID field: 0..2047 *)
let n_routes = 4

let routing_table : int iarray =
  let t =
    Iarray.init n_apids (fun apid ->
        if apid < 256 then 0
        else if apid < 1024 then 1
        else if apid < 1536 then 2
        else 3)
  in
  assert (Iarray.length t = n_apids);
  Iarray.iter (fun r -> assert (r >= 0 && r < n_routes)) t;
  t

let pp_counts ppf (hk, sci, diag, idle) =
  Fmt.pf ppf "(hk=%d sci=%d diag=%d idle=%d)" hk sci diag idle

let[@inline] route_of_apid apid = Iarray.unsafe_get routing_table apid
let hdr = Wire.Codec.wire_size Space.packet_codec
let get_apid = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_apid)
let get_dlen = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_data_len)

type state = { off : int ref; handler_counts : int array }

let step ~buf ~total_bytes state () =
  if !(state.off) + hdr > total_bytes then state.off := 0;
  let o = !(state.off) in
  let apid = get_apid buf o in
  let dlen = get_dlen buf o in
  let route = route_of_apid apid in
  Array.unsafe_set state.handler_counts route
    (Array.unsafe_get state.handler_counts route + 1);
  state.off := o + hdr + dlen + 1

let state () = { off = ref 0; handler_counts = Array.make n_routes 0 }

let reset state =
  state.off := 0;
  Array.fill state.handler_counts 0 n_routes 0

let counts state =
  ( state.handler_counts.(0),
    state.handler_counts.(1),
    state.handler_counts.(2),
    state.handler_counts.(3) )

let run_packets ~buf ~total_bytes state n_pkts =
  let step = step ~buf ~total_bytes state in
  for _ = 0 to n_pkts - 1 do
    step ()
  done;
  counts state

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
  let n_pkts = 10_000_000 in
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
