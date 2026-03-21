(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router using Wire's
    staged Codec.get — all field access is generated from the Wire DSL. The C
    baseline does the same work with hand-written bitfield extraction. *)

open Bench_lib
module C = Wire.Codec

external c_apid_route : bytes -> int -> int -> int = "c_apid_route"

external c_apid_route_counts : bytes -> int -> int -> int
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

let handler_counts = Array.make 4 0

let[@inline] dispatch handler_id =
  handler_counts.(handler_id) <- handler_counts.(handler_id) + 1

let () =
  Memtrace.trace_if_requested ~context:"routing" ();
  let n_pkts = 10_000_000 in
  let buf, total_bytes, payload_bytes = generate_stream n_pkts in
  let hdr = Wire.Codec.wire_size Space.packet_codec in
  Fmt.pr "APID demux (%d packets, %d MB stream, %d MB payload)\n\n" n_pkts
    (total_bytes / 1_000_000)
    (payload_bytes / 1_000_000);

  let get_apid = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_apid) in
  let get_seq =
    Wire.Staged.unstage (C.get Space.packet_codec cf_sp_seq_count)
  in
  let get_dlen =
    Wire.Staged.unstage (C.get Space.packet_codec cf_sp_data_len)
  in

  (* OCaml tier: route one packet at a time, cycling through the stream *)
  let off = ref 0 in
  let ocaml_fn () =
    let o = !off in
    if o + hdr > total_bytes then off := 0;
    let o = !off in
    let apid = get_apid buf o in
    let _seq = get_seq buf o in
    let dlen = get_dlen buf o in
    dispatch routing_table.(apid);
    off := o + hdr + dlen + 1
  in
  let reset () =
    off := 0;
    Array.fill handler_counts 0 4 0
  in

  (* Verify OCaml and C produce the same hk handler count *)
  let verify () =
    reset ();
    off := 0;
    for _ = 0 to n_pkts - 1 do
      let o = !off in
      if o + hdr > total_bytes then off := 0;
      let o = !off in
      let apid = get_apid buf o in
      let _seq = get_seq buf o in
      let dlen = get_dlen buf o in
      dispatch routing_table.(apid);
      off := o + hdr + dlen + 1
    done;
    let ocaml_hk = handler_counts.(0) in
    let c_hk = c_apid_route_counts buf 0 n_pkts in
    if ocaml_hk <> c_hk then
      Fmt.failwith "Routing result mismatch: OCaml hk=%d C hk=%d" ocaml_hk c_hk;
    reset ()
  in

  let t =
    v "Wire (staged Codec.get)" ~size:hdr ~reset ocaml_fn
    |> with_c c_apid_route buf |> with_verify verify
  in

  run_table ~title:"APID routing" ~n:n_pkts ~unit:"pkt" [ t ];

  (* Print final results *)
  reset ();
  off := 0;
  for _ = 0 to n_pkts - 1 do
    let o = !off in
    if o + hdr > total_bytes then off := 0;
    let o = !off in
    let apid = get_apid buf o in
    let _seq = get_seq buf o in
    let dlen = get_dlen buf o in
    dispatch routing_table.(apid);
    off := o + hdr + dlen + 1
  done;
  let c_hk = c_apid_route_counts buf 0 n_pkts in
  Fmt.pr "\n  OCaml: hk=%d sci=%d diag=%d idle=%d\n" handler_counts.(0)
    handler_counts.(1) handler_counts.(2) handler_counts.(3);
  Fmt.pr "  C:     hk=%d\n" c_hk
