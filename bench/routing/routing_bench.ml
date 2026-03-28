(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router using Wire's
    staged Codec.get — all field access is generated from the Wire DSL. The C
    baseline does the same work with hand-written bitfield extraction. *)

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

let routing_table =
  Array.init 2048 (fun apid ->
      if apid < 256 then 0
      else if apid < 1024 then 1
      else if apid < 1536 then 2
      else 3)

type state = {
  buf : bytes;
  total_bytes : int;
  hdr : int;
  off : int ref;
  handler_counts : int array;
  get_apid : bytes -> int -> int;
  get_seq : bytes -> int -> int;
  get_dlen : bytes -> int -> int;
}

let pp_counts ppf (hk, sci, diag, idle) =
  Fmt.pf ppf "(hk=%d sci=%d diag=%d idle=%d)" hk sci diag idle

let state buf total_bytes =
  let hdr = Wire.Codec.wire_size Space.packet_codec in
  {
    buf;
    total_bytes;
    hdr;
    off = ref 0;
    handler_counts = Array.make 4 0;
    get_apid = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_apid);
    get_seq = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_seq_count);
    get_dlen = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_data_len);
  }

let reset state =
  state.off := 0;
  Array.fill state.handler_counts 0 4 0

let counts state =
  ( state.handler_counts.(0),
    state.handler_counts.(1),
    state.handler_counts.(2),
    state.handler_counts.(3) )

let dispatch state handler_id =
  state.handler_counts.(handler_id) <- state.handler_counts.(handler_id) + 1

let step state () =
  let o = !(state.off) in
  if o + state.hdr > state.total_bytes then state.off := 0;
  let o = !(state.off) in
  let apid = state.get_apid state.buf o in
  let _seq = state.get_seq state.buf o in
  let dlen = state.get_dlen state.buf o in
  dispatch state routing_table.(apid);
  state.off := o + state.hdr + dlen + 1

let run_packets state n_pkts =
  for _ = 0 to n_pkts - 1 do
    step state ()
  done;
  counts state

let benchmark ~n_pkts =
  let buf, total_bytes, payload_bytes = generate_stream n_pkts in
  let st = state buf total_bytes in
  let ocaml_result () = run_packets st n_pkts in
  let c_result () = c_apid_route_counts buf 0 n_pkts in
  let hdr = st.hdr in
  let ffi_off = ref 0 in
  let ffi_reset () = ffi_off := 0 in
  let ffi_fn _buf =
    if !ffi_off + hdr > total_bytes then ffi_off := 0;
    ignore (C_stubs.spacepacket_parse buf !ffi_off);
    ffi_off := !ffi_off + hdr
  in
  let reset () =
    reset st;
    ffi_reset ()
  in
  let t =
    v "Wire OCaml" ~size:hdr ~reset (step st)
    |> with_c c_apid_route buf
    |> with_ffi ffi_fn Bytes.empty
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
  let ocaml_counts = run_packets st n_pkts in
  let c_counts = c_apid_route_counts buf 0 n_pkts in
  Fmt.pr "\n  OCaml: %a\n" pp_counts ocaml_counts;
  Fmt.pr "  C:     %a\n" pp_counts c_counts
