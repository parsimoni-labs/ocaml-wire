(** TM frame reassembly benchmark.

    Simulates a downlink TM frame processor using Wire's staged Codec.get — all
    field access is generated from the Wire DSL. *)

open Bench_lib
module C = Wire.Codec

let cadu_size = 1115
let tm_hdr = Wire.Codec.wire_size Space.tm_frame_codec
let data_field_size = cadu_size - tm_hdr
let sp_hdr = Wire.Codec.wire_size Space.packet_codec

let generate_frames n =
  let buf = Bytes.create (n * cadu_size) in
  let total_pkts = ref 0 in
  let remaining_in_frame = ref data_field_size in
  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in
  (* Use Wire Codec.set for frame headers *)
  let set_vcid =
    Wire.Staged.unstage (C.set Space.tm_frame_codec Space.f_tf_vcid)
  in
  let set_fhp =
    Wire.Staged.unstage (C.set Space.tm_frame_codec Space.f_tf_first_hdr)
  in
  let set_apid =
    Wire.Staged.unstage (C.set Space.packet_codec Space.f_sp_apid)
  in
  let set_seq =
    Wire.Staged.unstage (C.set Space.packet_codec Space.f_sp_seq_count)
  in
  let set_dlen =
    Wire.Staged.unstage (C.set Space.packet_codec Space.f_sp_data_len)
  in
  for frame = 0 to n - 1 do
    let base = frame * cadu_size in
    Wire.Codec.encode Space.tm_frame_codec Space.tm_frame_default buf base;
    set_vcid buf base (frame mod 8);
    let first_hdr_ptr =
      if !remaining_in_frame >= pkt_size then 0 else !remaining_in_frame
    in
    set_fhp buf base first_hdr_ptr;
    let data_off = ref (base + tm_hdr) in
    remaining_in_frame := data_field_size;
    while !remaining_in_frame >= pkt_size do
      let o = !data_off in
      set_apid buf o (!total_pkts mod 2048);
      set_seq buf o (!total_pkts mod 16384);
      set_dlen buf o (pkt_payload - 1);
      data_off := o + pkt_size;
      remaining_in_frame := !remaining_in_frame - pkt_size;
      incr total_pkts
    done
  done;
  (buf, !total_pkts)

let () =
  let n = 1_000_000 in
  let buf, total_pkts = generate_frames n in
  Fmt.pr
    "TM frame reassembly (%d frames, %d-byte CADUs, %d embedded packets)\n\n" n
    cadu_size total_pkts;

  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in
  let get_vcid =
    Wire.Staged.unstage (C.get Space.tm_frame_codec Space.f_tf_vcid)
  in
  let get_fhp =
    Wire.Staged.unstage (C.get Space.tm_frame_codec Space.f_tf_first_hdr)
  in
  let get_apid =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_apid)
  in
  let get_seq =
    Wire.Staged.unstage (C.get Space.packet_codec Space.f_sp_seq_count)
  in

  (* OCaml tier: process one frame, cycling through the buffer *)
  let ocaml_fn, reset =
    cycling ~data:buf ~n_items:n ~size:cadu_size (fun buf base ->
        let vcid = get_vcid buf base in
        let fhp = get_fhp buf base in
        ignore (Sys.opaque_identity vcid);
        let data_start = base + tm_hdr in
        let off = ref (data_start + fhp) in
        while !off + pkt_size <= data_start + data_field_size do
          let _apid = get_apid buf !off in
          let _seq = get_seq buf !off in
          off := !off + pkt_size
        done)
  in

  let single_frame = Bytes.sub buf 0 cadu_size in
  let t =
    ( v "Wire (staged Codec.get)" ~size:cadu_size ~reset ocaml_fn |> fun t ->
      match C_tier.tmframe_loop with
      | Some f -> with_c f single_frame t
      | None -> t )
    |> fun t ->
    match C_tier.tmframe_check with
    | Some f -> with_ffi f single_frame t
    | None -> t
  in

  run_table ~title:"TM frame reassembly" ~n ~unit:"frm" [ t ]
