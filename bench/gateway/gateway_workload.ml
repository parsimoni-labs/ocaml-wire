(* TM frame reassembly: the frame stream and the walk over the packets each
   frame carries, with every field read by Wire's staged [Codec.get]. *)

module C = Wire.Codec

let cadu_size = 1115
let hdr = Wire.Codec.wire_size Space.tm_frame_codec
let data_field_size = cadu_size - hdr
let sp_hdr = Wire.Codec.wire_size Space.packet_codec
let pkt_payload = 64
let pkt_size = sp_hdr + pkt_payload
let cf_tf_vcid = Space.bf_tf_vcid
let cf_tf_first_hdr = Space.bf_tf_first_hdr
let cf_sp_apid = Space.bf_sp_apid
let cf_sp_seq_count = Space.bf_sp_seq_count
let cf_sp_data_len = Space.bf_sp_data_len

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
  let set_vcid = Wire.Staged.unstage (C.set Space.tm_frame_codec cf_tf_vcid) in
  let set_fhp =
    Wire.Staged.unstage (C.set Space.tm_frame_codec cf_tf_first_hdr)
  in
  let set_apid = Wire.Staged.unstage (C.set Space.packet_codec cf_sp_apid) in
  let set_seq =
    Wire.Staged.unstage (C.set Space.packet_codec cf_sp_seq_count)
  in
  let set_dlen =
    Wire.Staged.unstage (C.set Space.packet_codec cf_sp_data_len)
  in
  for frame = 0 to n - 1 do
    let base = frame * cadu_size in
    Wire.Codec.encode Space.tm_frame_codec Space.tm_frame_default buf base;
    set_vcid buf base (frame mod 8);
    if frame mod 64 = 63 then begin
      (* An idle frame: its first header pointer, 0x7FE, says that no packet
         starts in it, so a reader walks no packet there. The packet spanning
         into it is lost, and the next frame starts a packet at its first
         byte. *)
      set_fhp buf base idle_fhp;
      carry := 0
    end
    else begin
      set_fhp buf base !carry;
      let data_off = ref (base + hdr + !carry) in
      while !data_off + pkt_size <= base + cadu_size do
        let o = !data_off in
        set_apid buf o (!total_pkts mod 2048);
        set_seq buf o (!total_pkts mod 16384);
        set_dlen buf o (Wire.UInt16.v (pkt_payload - 1));
        data_off := o + pkt_size;
        incr total_pkts
      done;
      let used = !data_off - (base + hdr) in
      let trailing = data_field_size - used in
      carry := if trailing = 0 then 0 else pkt_size - trailing
    end
  done;
  (buf, !total_pkts)

let read_vcid = Wire.Staged.unstage (C.get Space.tm_frame_codec cf_tf_vcid)
let read_fhp = Wire.Staged.unstage (C.get Space.tm_frame_codec cf_tf_first_hdr)
let read_apid = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_apid)
let read_seq = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_seq_count)

(* The packets of the frame at [base], whose first header pointer is [fhp]. *)
let walk_packets buf base ~fhp on_packet =
  let data_start = base + hdr in
  let off = ref (data_start + fhp) in
  while !off + pkt_size <= data_start + data_field_size do
    let apid = read_apid buf !off in
    let seq = read_seq buf !off in
    on_packet apid seq;
    off := !off + pkt_size
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
    let vcid = read_vcid buf base in
    let fhp = read_fhp buf base in
    ignore (Sys.opaque_identity vcid);
    walk_packets buf base ~fhp (fun _apid _seq -> ())
  in
  let scan_once, reset_cycle =
    Bench_lib.cycling ~data:buf ~n_items:n_frames ~size:cadu_size scan_frame
  in
  { buf; n_frames; scan_once; reset_cycle }

let reset state = state.reset_cycle ()
let step state () = state.scan_once ()

(* The packet loop of each frame, written out as the generated rows write
   theirs, with the hash in a local. *)
let checksum state =
  let buf = state.buf in
  let checksum = ref checksum_init in
  for frame = 0 to state.n_frames - 1 do
    let base = frame * cadu_size in
    let fhp = read_fhp buf base in
    checksum := hash_int (hash_int !checksum (read_vcid buf base)) fhp;
    let data_end = base + hdr + data_field_size in
    let off = ref (base + hdr + fhp) in
    while !off + pkt_size <= data_end do
      let o = !off in
      let apid = read_apid buf o in
      let seq = read_seq buf o in
      checksum := hash_int (hash_int !checksum apid) seq;
      off := o + pkt_size
    done
  done;
  !checksum

(* The header {!Space.tm_frame_default} encodes, spacecraft 0x1FF, OCF flag
   set, segment length identifier 3, with the four fields a frame varies. *)
let write_frame_header =
  Wire.Staged.unstage
    (C.writer Space.tm_frame_codec Space.tm_frame_default
       [
         cf_tf_vcid; Space.bf_tf_mc_count; Space.bf_tf_vc_count; cf_tf_first_hdr;
       ])

(* The header {!Space.packet_default} encodes, a telemetry packet with a
   secondary header, unsegmented, with the three fields a packet varies. *)
let write_packet_header =
  Wire.Staged.unstage
    (C.writer Space.packet_codec Space.packet_default
       [ cf_sp_apid; cf_sp_seq_count; cf_sp_data_len ])

let pkt_dlen = Wire.UInt16.v (pkt_payload - 1)
let stamp out o ~apid ~seq = write_packet_header out o apid seq pkt_dlen

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
