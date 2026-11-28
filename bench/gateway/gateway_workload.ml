(* TM frame reassembly: the frame stream and the walk over the packets each
   frame carries, each header checked by Wire's [Codec.View.check] and its
   fields read from the view with [Codec.View.read]. *)

module C = Wire.Codec
module V = C.View

let cadu_size = 1115
let hdr = Wire.Codec.min_size Space.tm_frame_codec
let data_field_size = cadu_size - hdr
let sp_hdr = Wire.Codec.min_size Space.packet_codec
let pkt_payload = 64
let pkt_size = sp_hdr + pkt_payload
let cf_tf_vcid = Space.bf_tf_vcid
let cf_tf_first_hdr = Space.bf_tf_first_hdr
let cf_sp_apid = Space.bf_sp_apid
let cf_sp_seq_count = Space.bf_sp_seq_count

(* FNV-1a in OCaml's 63-bit [int]: the 64-bit offset basis and prime taken
   modulo 2^63, each product wrapping modulo 2^63 as [int] arithmetic does.
   The Rust and C rows compute the same hash in a 64-bit word, masked to its
   low 63 bits after each product. *)
let checksum_init = 0x4bf29ce484222325
let checksum_prime = 0x100000001B3
let hash_int state v = state lxor v * checksum_prime
let idle_fhp = 0x7FE

let generate_frames n =
  let buf = Bytes.create (n * cadu_size) in
  let total_pkts = ref 0 in
  let carry = ref 0 in
  let frame_header vcid first_hdr =
    { Space.tm_frame_default with Space.vcid; first_hdr }
  in
  for frame = 0 to n - 1 do
    let base = frame * cadu_size in
    let vcid = frame mod 8 in
    if frame mod 64 = 63 then begin
      (* An idle frame: its first header pointer, 0x7FE, says that no packet
         starts in it, so a reader walks no packet there. The packet spanning
         into it is lost, and the next frame starts a packet at its first
         byte. *)
      C.encode Space.tm_frame_codec (frame_header vcid idle_fhp) buf ~off:base;
      carry := 0
    end
    else begin
      C.encode Space.tm_frame_codec (frame_header vcid !carry) buf ~off:base;
      let data_off = ref (base + hdr + !carry) in
      while !data_off + pkt_size <= base + cadu_size do
        let o = !data_off in
        C.encode Space.packet_codec
          {
            Space.packet_default with
            Space.apid = !total_pkts mod 2048;
            seq_count = !total_pkts mod 16384;
            data_len = Wire.UInt16.v (pkt_payload - 1);
          }
          buf ~off:o;
        data_off := o + pkt_size;
        incr total_pkts
      done;
      let used = !data_off - (base + hdr) in
      let trailing = data_field_size - used in
      carry := if trailing = 0 then 0 else pkt_size - trailing
    end
  done;
  (buf, !total_pkts)

(* Each header is checked once, then the fields the walk needs read from its
   view. *)
let p_vcid = V.field Space.tm_frame_codec cf_tf_vcid
let p_fhp = V.field Space.tm_frame_codec cf_tf_first_hdr
let p_apid = V.field Space.packet_codec cf_sp_apid
let p_seq = V.field Space.packet_codec cf_sp_seq_count

(* The packets of the frame at [base], whose first header pointer is [fhp]. *)
let walk_packets buf base ~fhp on_packet =
  let data_start = base + hdr in
  let off = ref (data_start + fhp) in
  let data_end = data_start + data_field_size in
  let (Wire.Buf.Any b) = Wire.Buf.v buf in
  while !off + pkt_size <= data_end do
    let k = V.check Space.packet_codec b ~off:!off in
    if V.is_ok k then begin
      let v = V.get k in
      let apid = V.read b v p_apid in
      let seq = V.read b v p_seq in
      on_packet apid seq;
      off := !off + pkt_size
    end
    else off := data_end
  done

type state = {
  buf : bytes;
  n_frames : int;
  scan_once : unit -> unit;
  reset_cycle : unit -> unit;
}

let state buf =
  let n_frames = Bytes.length buf / cadu_size in
  let scan_frame buf base =
    let (Wire.Buf.Any b) = Wire.Buf.v buf in
    let k = V.check Space.tm_frame_codec b ~off:base in
    if V.is_ok k then begin
      let v = V.get k in
      let vcid = V.read b v p_vcid in
      let fhp = V.read b v p_fhp in
      ignore (Sys.opaque_identity vcid);
      walk_packets buf base ~fhp (fun _apid _seq -> ())
    end
  in
  let scan_once, reset_cycle =
    Bench_lib.cycling ~data:buf ~n_items:n_frames ~size:cadu_size scan_frame
  in
  { buf; n_frames; scan_once; reset_cycle }

let reset state = state.reset_cycle ()
let step state () = state.scan_once ()

(* The packets of the data field of a frame, from [off] up to [data_end],
   hashed into [checksum]. *)
let frame_packets b checksum ~off ~data_end =
  let off = ref off in
  while !off + pkt_size <= data_end do
    let o = !off in
    let k = V.check Space.packet_codec b ~off:o in
    if V.is_ok k then begin
      let v = V.get k in
      let apid = V.read b v p_apid in
      let seq = V.read b v p_seq in
      checksum := hash_int (hash_int !checksum apid) seq;
      off := o + pkt_size
    end
    else off := data_end
  done

(* The packet loop of each frame, written out as the generated rows write
   theirs, with the hash in a local. *)
let checksum state =
  let (Wire.Buf.Any b) = Wire.Buf.v state.buf in
  let checksum = ref checksum_init in
  for frame = 0 to state.n_frames - 1 do
    let base = frame * cadu_size in
    let k = V.check Space.tm_frame_codec b ~off:base in
    if V.is_ok k then begin
      let v = V.get k in
      let fhp = V.read b v p_fhp in
      checksum := hash_int (hash_int !checksum (V.read b v p_vcid)) fhp;
      frame_packets b checksum
        ~off:(base + hdr + fhp)
        ~data_end:(base + hdr + data_field_size)
    end
  done;
  !checksum

(* The header {!Space.tm_frame_default} encodes, spacecraft 0x1FF, OCF flag
   set, segment length identifier 3, with the four fields a frame varies. *)
let write_frame_header out o vcid mc_count vc_count first_hdr =
  C.encode Space.tm_frame_codec
    { Space.tm_frame_default with Space.vcid; mc_count; vc_count; first_hdr }
    out ~off:o

(* The header {!Space.packet_default} encodes, a telemetry packet with a
   secondary header, unsegmented, with the three fields a packet varies. *)
let pkt_dlen = Wire.UInt16.v (pkt_payload - 1)

let stamp out o ~apid ~seq =
  C.encode Space.packet_codec
    {
      Space.packet_default with
      Space.apid;
      seq_count = seq;
      data_len = pkt_dlen;
    }
    out ~off:o

let frame ~out n =
  let carry = ref 0 and p = ref 0 in
  for f = 0 to n - 1 do
    let base = f * cadu_size in
    write_frame_header out base (f land 7) (f land 0xFF)
      ((f lsr 3) land 0xFF)
      !carry;
    let off = ref (base + hdr + !carry) in
    while !off + pkt_size <= base + cadu_size do
      stamp out !off ~apid:(!p land 0x7FF) ~seq:(!p land 0x3FFF);
      off := !off + pkt_size;
      incr p
    done;
    let trailing = data_field_size - (!off - (base + hdr)) in
    carry := if trailing = 0 then 0 else pkt_size - trailing
  done
