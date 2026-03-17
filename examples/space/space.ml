(** CCSDS space protocol codecs.

    Real-world protocol headers (CCSDS Space Packet, CLCW, TM Frame) and a
    nested protocol demo (InnerCmd/OuterHdr) for zero-copy access. *)

open Wire

(* ── 1. CCSDS Space Packet primary header: 6 bytes ──
   Real-world protocol: 3+1+1+11 bits (uint16be) + 2+14 bits (uint16be) + uint16be *)

type packet = {
  sp_version : int;
  sp_type : int;
  sp_sec_hdr : int;
  sp_apid : int;
  sp_seq_flags : int;
  sp_seq_count : int;
  sp_data_len : int;
}

let f_sp_apid =
  Codec.field "APID" (bits ~width:11 bf_uint16be) (fun p -> p.sp_apid)

let f_sp_seq_count =
  Codec.field "SeqCount" (bits ~width:14 bf_uint16be) (fun p -> p.sp_seq_count)

let f_sp_data_len = Codec.field "DataLength" uint16be (fun p -> p.sp_data_len)

let packet_codec =
  let open Codec in
  record "SpacePacket"
    (fun version type_ sec_hdr apid seq_flags seq_count data_len ->
      {
        sp_version = version;
        sp_type = type_;
        sp_sec_hdr = sec_hdr;
        sp_apid = apid;
        sp_seq_flags = seq_flags;
        sp_seq_count = seq_count;
        sp_data_len = data_len;
      })
  |+ field "Version" (bits ~width:3 bf_uint16be) (fun p -> p.sp_version)
  |+ field "Type" (bits ~width:1 bf_uint16be) (fun p -> p.sp_type)
  |+ field "SecHdrFlag" (bits ~width:1 bf_uint16be) (fun p -> p.sp_sec_hdr)
  |+ f_sp_apid
  |+ field "SeqFlags" (bits ~width:2 bf_uint16be) (fun p -> p.sp_seq_flags)
  |+ f_sp_seq_count |+ f_sp_data_len |> seal

let packet_struct = Codec.to_struct packet_codec
let packet_size = Codec.wire_size packet_codec

let packet_default =
  {
    sp_version = 0;
    sp_type = 0;
    sp_sec_hdr = 1;
    sp_apid = 0x7FF;
    sp_seq_flags = 3;
    sp_seq_count = 0;
    sp_data_len = 255;
  }

let packet_data n =
  Array.init n (fun i ->
      let b = Bytes.create packet_size in
      let w0 = ((i mod 2) lsl 12) lor (i mod 2048) in
      Bytes.set_uint16_be b 0 w0;
      Bytes.set_uint16_be b 2 (0xC000 lor (i mod 16384));
      Bytes.set_uint16_be b 4 (i mod 256);
      b)

(* ── 2. CLCW: 4 bytes of bitfields ──
   Real-world protocol: 1+2+3+2+6+2+1+1+1+1+1+2+8 = 31 bits + 1 spare = 32 *)

type clcw = {
  cw_type : int;
  cw_version : int;
  cw_status : int;
  cw_cop : int;
  cw_vcid : int;
  cw_spare : int;
  cw_no_rf : int;
  cw_no_bitlock : int;
  cw_lockout : int;
  cw_wait : int;
  cw_retransmit : int;
  cw_farmb : int;
  cw_report : int;
}

let cw_lockout =
  Codec.field "Lockout" (bits ~width:1 bf_uint32be) (fun c -> c.cw_lockout)

let cw_wait =
  Codec.field "Wait" (bits ~width:1 bf_uint32be) (fun c -> c.cw_wait)

let cw_retransmit =
  Codec.field "Retransmit" (bits ~width:1 bf_uint32be) (fun c ->
      c.cw_retransmit)

let cw_report =
  Codec.field "ReportValue" (bits ~width:8 bf_uint32be) (fun c -> c.cw_report)

let clcw_codec =
  let open Codec in
  record "CLCW"
    (fun
      type_
      version
      status
      cop
      vcid
      spare
      no_rf
      no_bitlock
      lockout
      wait
      retransmit
      farmb
      report
    ->
      {
        cw_type = type_;
        cw_version = version;
        cw_status = status;
        cw_cop = cop;
        cw_vcid = vcid;
        cw_spare = spare;
        cw_no_rf = no_rf;
        cw_no_bitlock = no_bitlock;
        cw_lockout = lockout;
        cw_wait = wait;
        cw_retransmit = retransmit;
        cw_farmb = farmb;
        cw_report = report;
      })
  |+ field "ControlWordType" (bits ~width:1 bf_uint32be) (fun c -> c.cw_type)
  |+ field "CLCWVersion" (bits ~width:2 bf_uint32be) (fun c -> c.cw_version)
  |+ field "StatusField" (bits ~width:3 bf_uint32be) (fun c -> c.cw_status)
  |+ field "COPInEffect" (bits ~width:2 bf_uint32be) (fun c -> c.cw_cop)
  |+ field "VCID" (bits ~width:6 bf_uint32be) (fun c -> c.cw_vcid)
  |+ field "Spare" (bits ~width:2 bf_uint32be) (fun c -> c.cw_spare)
  |+ field "NoRF" (bits ~width:1 bf_uint32be) (fun c -> c.cw_no_rf)
  |+ field "NoBitlock" (bits ~width:1 bf_uint32be) (fun c -> c.cw_no_bitlock)
  |+ cw_lockout |+ cw_wait |+ cw_retransmit
  |+ field "FARMBCounter" (bits ~width:2 bf_uint32be) (fun c -> c.cw_farmb)
  |+ cw_report |> seal

let clcw_struct = Codec.to_struct clcw_codec
let clcw_size = Codec.wire_size clcw_codec

let clcw_default =
  {
    cw_type = 0;
    cw_version = 0;
    cw_status = 0;
    cw_cop = 1;
    cw_vcid = 7;
    cw_spare = 0;
    cw_no_rf = 0;
    cw_no_bitlock = 0;
    cw_lockout = 0;
    cw_wait = 0;
    cw_retransmit = 0;
    cw_farmb = 0;
    cw_report = 42;
  }

let clcw_data n =
  Array.init n (fun i ->
      let b = Bytes.create clcw_size in
      let w =
        ((i mod 4) lsl 29)
        lor ((i mod 8) lsl 26)
        lor ((i mod 4) lsl 24)
        lor ((i mod 64) lsl 18)
        lor ((i mod 32) lsl 11)
        lor ((i mod 4) lsl 9)
        lor (i mod 256)
      in
      Bytes.set_int32_be b 0 (Int32.of_int w);
      b)

(* ── 3. TM Transfer Frame primary header: 6 bytes ──
   Real-world protocol: 2+10+3+1 (uint16be) + 8+8 (uint16be) + 1+1+1+2+11 (uint16be) *)

type tm_frame = {
  tf_version : int;
  tf_scid : int;
  tf_vcid : int;
  tf_ocf_flag : int;
  tf_mc_count : int;
  tf_vc_count : int;
  tf_sec_hdr : int;
  tf_sync : int;
  tf_pkt_order : int;
  tf_seg_id : int;
  tf_first_hdr : int;
}

let tm_frame_add_identifier_fields r =
  let open Codec in
  r
  |+ field "SCID" (bits ~width:10 bf_uint16be) (fun f -> f.tf_scid)
  |+ field "VCID" (bits ~width:3 bf_uint16be) (fun f -> f.tf_vcid)
  |+ field "OCFFlag" (bits ~width:1 bf_uint16be) (fun f -> f.tf_ocf_flag)
  |+ field "MCCount" (bits ~width:8 bf_uint16be) (fun f -> f.tf_mc_count)
  |+ field "VCCount" (bits ~width:8 bf_uint16be) (fun f -> f.tf_vc_count)

let tm_frame_codec =
  let open Codec in
  record "TMFrame" (fun version scid vcid ocf mc vc sec sync pkt seg hdr ->
      {
        tf_version = version;
        tf_scid = scid;
        tf_vcid = vcid;
        tf_ocf_flag = ocf;
        tf_mc_count = mc;
        tf_vc_count = vc;
        tf_sec_hdr = sec;
        tf_sync = sync;
        tf_pkt_order = pkt;
        tf_seg_id = seg;
        tf_first_hdr = hdr;
      })
  |+ field "Version" (bits ~width:2 bf_uint16be) (fun f -> f.tf_version)
  |> tm_frame_add_identifier_fields
  |+ field "SecHdrFlag" (bits ~width:1 bf_uint16be) (fun f -> f.tf_sec_hdr)
  |+ field "SyncFlag" (bits ~width:1 bf_uint16be) (fun f -> f.tf_sync)
  |+ field "PacketOrder" (bits ~width:1 bf_uint16be) (fun f -> f.tf_pkt_order)
  |+ field "SegLenId" (bits ~width:2 bf_uint16be) (fun f -> f.tf_seg_id)
  |+ field "FirstHdrPtr" (bits ~width:11 bf_uint16be) (fun f -> f.tf_first_hdr)
  |> seal

let tm_frame_struct = Codec.to_struct tm_frame_codec
let tm_frame_size = Codec.wire_size tm_frame_codec

let tm_frame_default =
  {
    tf_version = 0;
    tf_scid = 0x1FF;
    tf_vcid = 3;
    tf_ocf_flag = 1;
    tf_mc_count = 0;
    tf_vc_count = 0;
    tf_sec_hdr = 0;
    tf_sync = 0;
    tf_pkt_order = 0;
    tf_seg_id = 3;
    tf_first_hdr = 0x7FE;
  }

let tm_frame_data n =
  Array.init n (fun i ->
      let b = Bytes.create tm_frame_size in
      let w0 =
        ((i mod 1024 land 0x3FF) lsl 4)
        lor ((i mod 8 land 0x7) lsl 1)
        lor (i mod 2)
      in
      Bytes.set_uint16_be b 0 w0;
      Bytes.set_uint16_be b 2 (((i mod 256) lsl 8) lor (i * 7 mod 256));
      Bytes.set_uint16_be b 4 ((1 lsl 14) lor (3 lsl 11) lor (i mod 2048));
      b)

(* ── 4. Nested protocol: outer header (4B fixed + variable payload) ──
   Demonstrates zero-copy field access across protocol layers with dependent
   sizes: the payload length is determined at runtime by the Length field.
   Outer: [version:u8] [type:u8] [length:u16be] [payload:length bytes]
   Inner: [cmd_id:u8] [seq:u16be] [flags:u8]
   The outer "payload" field is a byte_slice whose size comes from Codec.ref. *)

type inner_cmd = { cmd_id : int; cmd_seq : int; cmd_flags : int }

let f_cmd_id = Codec.field "CmdId" uint8 (fun c -> c.cmd_id)
let f_cmd_seq = Codec.field "Seq" uint16be (fun c -> c.cmd_seq)

let inner_cmd_codec =
  let open Codec in
  record "InnerCmd" (fun id seq flags ->
      { cmd_id = id; cmd_seq = seq; cmd_flags = flags })
  |+ f_cmd_id |+ f_cmd_seq
  |+ field "Flags" uint8 (fun c -> c.cmd_flags)
  |> seal

let inner_cmd_size = Codec.wire_size inner_cmd_codec

type outer_hdr = {
  oh_version : int;
  oh_type : int;
  oh_length : int;
  oh_payload : Bytesrw.Bytes.Slice.t;
}

let f_oh_length = Codec.field "Length" uint16be (fun h -> h.oh_length)

let f_oh_payload =
  Codec.field "Payload"
    (byte_slice ~size:(Codec.ref f_oh_length))
    (fun h -> h.oh_payload)

let outer_hdr_codec =
  let open Codec in
  record "OuterHdr" (fun version type_ length payload ->
      {
        oh_version = version;
        oh_type = type_;
        oh_length = length;
        oh_payload = payload;
      })
  |+ field "Version" uint8 (fun h -> h.oh_version)
  |+ field "Type" uint8 (fun h -> h.oh_type)
  |+ f_oh_length |+ f_oh_payload |> seal

let outer_hdr_size = Codec.min_wire_size outer_hdr_codec + inner_cmd_size

let nested_data n =
  Array.init n (fun i ->
      let b = Bytes.create outer_hdr_size in
      Bytes.set_uint8 b 0 1;
      (* version *)
      Bytes.set_uint8 b 1 (i mod 4);
      (* type *)
      Bytes.set_uint16_be b 2 inner_cmd_size;
      (* length *)
      Bytes.set_uint8 b 4 (i mod 256);
      (* cmd_id *)
      Bytes.set_uint16_be b 5 (i mod 65536);
      (* seq *)
      Bytes.set_uint8 b 7 (i mod 8);
      (* flags *)
      b)
