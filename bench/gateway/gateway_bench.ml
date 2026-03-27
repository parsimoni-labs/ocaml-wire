(** TM frame reassembly benchmark.

    Simulates a downlink TM frame processor using Wire's staged Codec.get — all
    field access is generated from the Wire DSL. The C baseline does the same
    work with hand-written bitfield extraction. *)

open Bench_lib
module C = Wire.Codec

external c_tm_reassemble : bytes -> int -> int -> int = "c_tm_reassemble"

external c_tm_reassemble_checksum : bytes -> int -> int64
  = "c_tm_reassemble_checksum"

let cadu_size = 1115
let tm_hdr = Wire.Codec.wire_size Space.tm_frame_codec
let data_field_size = cadu_size - tm_hdr
let sp_hdr = Wire.Codec.wire_size Space.packet_codec
let cf_tf_vcid = Space.bf_tf_vcid
let cf_tf_first_hdr = Space.bf_tf_first_hdr
let cf_sp_apid = Space.bf_sp_apid
let cf_sp_seq_count = Space.bf_sp_seq_count
let cf_sp_data_len = Space.bf_sp_data_len
let checksum_init = Int64.of_string "0xcbf29ce484222325"
let checksum_prime = 0x100000001B3L

let hash_int state v =
  Int64.mul (Int64.logxor state (Int64.of_int v)) checksum_prime

let generate_frames n =
  let buf = Bytes.create (n * cadu_size) in
  let total_pkts = ref 0 in
  let carry = ref 0 in
  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in
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
    set_fhp buf base !carry;
    let data_off = ref (base + tm_hdr + !carry) in
    while !data_off + pkt_size <= base + cadu_size do
      let o = !data_off in
      set_apid buf o (!total_pkts mod 2048);
      set_seq buf o (!total_pkts mod 16384);
      set_dlen buf o (pkt_payload - 1);
      data_off := o + pkt_size;
      incr total_pkts
    done;
    let used = !data_off - (base + tm_hdr) in
    let trailing = data_field_size - used in
    carry := if trailing = 0 then 0 else pkt_size - trailing
  done;
  (buf, !total_pkts)

type state = {
  buf : bytes;
  n_frames : int;
  total_pkts : int;
  get_vcid : bytes -> int -> int;
  get_fhp : bytes -> int -> int;
  walk_frame : int -> (int -> int -> unit) -> unit;
  scan_once : unit -> unit;
  reset_cycle : unit -> unit;
}

let state n_frames =
  let buf, total_pkts = generate_frames n_frames in
  let pkt_payload = 64 in
  let pkt_size = sp_hdr + pkt_payload in
  let get_vcid = Wire.Staged.unstage (C.get Space.tm_frame_codec cf_tf_vcid) in
  let get_fhp =
    Wire.Staged.unstage (C.get Space.tm_frame_codec cf_tf_first_hdr)
  in
  let get_apid = Wire.Staged.unstage (C.get Space.packet_codec cf_sp_apid) in
  let get_seq =
    Wire.Staged.unstage (C.get Space.packet_codec cf_sp_seq_count)
  in
  let walk_frame base on_packet =
    let vcid = get_vcid buf base in
    let fhp = get_fhp buf base in
    ignore (Sys.opaque_identity vcid);
    ignore (Sys.opaque_identity fhp);
    let data_start = base + tm_hdr in
    let off = ref (data_start + fhp) in
    while !off + pkt_size <= data_start + data_field_size do
      let apid = get_apid buf !off in
      let seq = get_seq buf !off in
      on_packet apid seq;
      off := !off + pkt_size
    done
  in
  let scan_frame _buf base = walk_frame base (fun _apid _seq -> ()) in
  let scan_once, reset_cycle =
    cycling ~data:buf ~n_items:n_frames ~size:cadu_size scan_frame
  in
  {
    buf;
    n_frames;
    total_pkts;
    get_vcid;
    get_fhp;
    walk_frame;
    scan_once;
    reset_cycle;
  }

let reset state = state.reset_cycle ()
let step state () = state.scan_once ()

let checksum_frame state checksum base =
  let vcid = state.get_vcid state.buf base in
  let fhp = state.get_fhp state.buf base in
  let checksum = hash_int (hash_int checksum vcid) fhp in
  let checksum = ref checksum in
  state.walk_frame base (fun apid seq ->
      checksum := hash_int (hash_int !checksum apid) seq);
  !checksum

let process_all_frames state =
  let checksum = ref checksum_init in
  for frame = 0 to state.n_frames - 1 do
    checksum := checksum_frame state !checksum (frame * cadu_size)
  done;
  !checksum

let benchmark ~n_frames =
  let st = state n_frames in
  let ocaml_result () = process_all_frames st in
  let c_result () = c_tm_reassemble_checksum st.buf 0 in
  let t =
    v "Wire OCaml" ~size:cadu_size ~reset:(fun () -> reset st) (step st)
    |> with_c c_tm_reassemble st.buf
    |> with_expect ~equal:Int64.equal ~pp:Fmt.int64 ~c:c_result ocaml_result
  in
  (t, st)

let check ~n_frames =
  let t, _ = benchmark ~n_frames in
  Bench_lib.check t

let main () =
  Memtrace.trace_if_requested ~context:"gateway" ();
  let n_frames = 1_000_000 in
  let t, st = benchmark ~n_frames in
  Fmt.pr
    "TM frame reassembly (%d frames, %d-byte CADUs, %d embedded packets)\n\n"
    n_frames cadu_size st.total_pkts;
  run_table ~title:"TM frame reassembly" ~n:n_frames ~unit:"frm" [ t ];
  let ocaml_checksum = process_all_frames st in
  let c_checksum = c_tm_reassemble_checksum st.buf 0 in
  Fmt.pr "\n  OCaml checksum: 0x%Lx\n  C checksum:     0x%Lx\n" ocaml_checksum
    c_checksum
