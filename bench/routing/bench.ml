(** APID demux throughput benchmark.

    Simulates a SpaceWire CCSDS Packet Transfer Protocol router using Wire's
    staged Codec.get — all field access is generated from the Wire DSL. *)

open Bench_lib
module C = Wire.Codec

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
  let set_apid =
    Wire.Staged.unstage (C.set Space.packet_codec Space.f_sp_apid)
  in
  let set_seq =
    Wire.Staged.unstage (C.set Space.packet_codec Space.f_sp_seq_count)
  in
  let set_dlen =
    Wire.Staged.unstage (C.set Space.packet_codec Space.f_sp_data_len)
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
  let n_pkts = 10_000_000 in
  let buf, total_bytes, payload_bytes = generate_stream n_pkts in
  let hdr = Wire.Codec.wire_size Space.packet_codec in
  Fmt.pr "APID demux (%d packets, %d MB stream, %d MB payload)\n\n" n_pkts
    (total_bytes / 1_000_000)
    (payload_bytes / 1_000_000);

  let get_apid =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_apid)
  in
  let get_seq =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_seq_count)
  in
  let get_dlen =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_data_len)
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

  let single_pkt =
    Bytes.sub buf 0 (hdr + payload_size_of_apid (get_apid buf 0) + 1)
  in
  let t =
    ( v "Wire (staged Codec.get)" ~size:hdr ~reset ocaml_fn |> fun t ->
      match C_tier.spacepacket_loop with Some f -> with_c f buf t | None -> t )
    |> fun t ->
    match C_tier.spacepacket_check with
    | Some f -> with_ffi f single_pkt t
    | None -> t
  in

  run_table ~title:"APID routing" ~n:n_pkts ~unit:"pkt" [ t ];

  Fmt.pr "\n  routed: hk=%d sci=%d diag=%d idle=%d\n" handler_counts.(0)
    handler_counts.(1) handler_counts.(2) handler_counts.(3)
