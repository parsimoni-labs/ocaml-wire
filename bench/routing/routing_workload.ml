(* APID demux: the packet stream and the routing loop, with every field read
   by Wire's staged [Codec.get]. *)

module C = Wire.Codec

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

(* [n - wrap] whole packets, then one whose data field runs past the end of
   the buffer: a reader of [n] headers from offset 0 meets that truncated
   tail, finds no room for a header after it and wraps to the first packet
   for its last [wrap - 1] headers, so the wrap of every row runs. *)
let generate_stream n =
  let wrap = min n (max 1 (n / 64)) in
  let whole = n - wrap in
  let total = ref 0 in
  let hdr = Wire.Codec.wire_size Space.packet_codec in
  for i = 0 to whole - 1 do
    let apid = apid_of_index i in
    total := !total + hdr + payload_size_of_apid apid
  done;
  let tail =
    if n > 0 then payload_size_of_apid (apid_of_index whole) / 2 else 0
  in
  if n > 0 then total := !total + hdr + tail;
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
  for i = 0 to min n (whole + 1) - 1 do
    let apid = apid_of_index i in
    let plen = payload_size_of_apid apid in
    let o = !off in
    set_apid buf o apid;
    set_seq buf o (i mod 16384);
    set_dlen buf o (Wire.UInt16.v (plen - 1));
    off := o + hdr + plen;
    payload_total := !payload_total + if i < whole then plen else tail
  done;
  (buf, !total, !payload_total)

let n_apids = 2048 (* 11-bit APID field: 0..2047 *)
let n_routes = 4

let routing_table : int array =
  let t =
    Array.init n_apids (fun apid ->
        if apid < 256 then 0
        else if apid < 1024 then 1
        else if apid < 1536 then 2
        else 3)
  in
  assert (Array.length t = n_apids);
  Array.iter (fun r -> assert (r >= 0 && r < n_routes)) t;
  t

let pp_counts ppf (hk, sci, diag, idle) =
  Fmt.pf ppf "(hk=%d sci=%d diag=%d idle=%d)" hk sci diag idle

let[@inline] route_of_apid apid = Array.unsafe_get routing_table apid
let hdr = Wire.Codec.wire_size Space.packet_codec
let apid = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_apid)
let dlen = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_data_len)

type state = { off : int ref; handler_counts : int array }

let step ~buf ~total_bytes state () =
  if !(state.off) + hdr > total_bytes then state.off := 0;
  let o = !(state.off) in
  let apid = apid buf o in
  let dlen = dlen buf o in
  let route = route_of_apid apid in
  Array.unsafe_set state.handler_counts route
    (Array.unsafe_get state.handler_counts route + 1);
  state.off := o + hdr + Wire.UInt16.to_int dlen + 1

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
