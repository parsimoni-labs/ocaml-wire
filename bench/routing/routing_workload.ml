(* APID demux: the packet stream and the routing loop, each header checked by
   Wire's [Codec.View.check] and its fields read from the view with
   [Codec.View.read]. *)

module C = Wire.Codec
module V = C.View

let cf_sp_apid = Space.bf_sp_apid
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
  let hdr = Wire.Codec.min_size Space.packet_codec in
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
  for i = 0 to min n (whole + 1) - 1 do
    let apid = apid_of_index i in
    let plen = payload_size_of_apid apid in
    let o = !off in
    C.encode Space.packet_codec
      {
        Space.packet_default with
        Space.apid;
        seq_count = i mod 16384;
        data_len = Wire.UInt16.v (plen - 1);
      }
      buf ~off:o;
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

let route_of_apid apid = routing_table.(apid)
let hdr = Wire.Codec.min_size Space.packet_codec

(* A header is checked once, then its APID and data length read from its
   view. *)
let p_apid = V.field Space.packet_codec cf_sp_apid
let p_dlen = V.field Space.packet_codec cf_sp_data_len

type state = { off : int ref; handler_counts : int array }

let step ~buf ~total_bytes state () =
  if !(state.off) + hdr > total_bytes then state.off := 0;
  let o = !(state.off) in
  let (Wire.Buf.Any b) = Wire.Buf.v buf in
  let k = V.check Space.packet_codec b ~off:o in
  if V.is_ok k then begin
    let v = V.get k in
    let apid = V.read b v p_apid in
    let dlen = V.read b v p_dlen in
    let route = route_of_apid apid in
    state.handler_counts.(route) <- state.handler_counts.(route) + 1;
    state.off := o + hdr + Wire.UInt16.to_int dlen + 1
  end
  else state.off := 0

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

(* The header {!Space.packet_default} encodes, a telemetry packet with a
   secondary header, unsegmented, with the three fields a router writes. *)
let stamp out o ~apid ~seq ~dlen =
  C.encode Space.packet_codec
    { Space.packet_default with Space.apid; seq_count = seq; data_len = dlen }
    out ~off:o

let restamp ~buf ~total_bytes ~out n =
  let seqs = Array.make n_routes 0 in
  let off = ref 0 in
  for _ = 1 to n do
    if !off + hdr > total_bytes then off := 0;
    let o = !off in
    let (Wire.Buf.Any b) = Wire.Buf.v buf in
    let k = V.check Space.packet_codec b ~off:o in
    if V.is_ok k then begin
      let v = V.get k in
      let apid = V.read b v p_apid in
      let dlen = V.read b v p_dlen in
      let route = route_of_apid apid in
      let seq = seqs.(route) in
      seqs.(route) <- (seq + 1) land 0x3FFF;
      stamp out o ~apid ~seq ~dlen;
      off := o + hdr + Wire.UInt16.to_int dlen + 1
    end
    else off := 0
  done
