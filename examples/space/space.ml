(** CCSDS space protocol codecs.

    Real-world protocol headers (CCSDS Space Packet, CLCW, TM Frame) and a
    nested protocol demo (InnerCmd/OuterHdr) for zero-copy access. *)

open Wire

(* -- 1. CCSDS Space Packet primary header: 6 bytes -- *)

type packet = {
  sp_version : int;
  sp_type : int;
  sp_sec_hdr : int;
  sp_apid : int;
  sp_seq_flags : int;
  sp_seq_count : int;
  sp_data_len : int;
}

let f_sp_apid = Field.v "APID" (bits ~width:11 U16be)
let f_sp_seq_count = Field.v "SeqCount" (bits ~width:14 U16be)
let f_sp_data_len = Field.v "DataLength" uint16be

(* Bound fields — created before the codec so they ARE the codec's fields *)
let bf_sp_apid = Codec.(f_sp_apid $ fun p -> p.sp_apid)
let bf_sp_seq_count = Codec.(f_sp_seq_count $ fun p -> p.sp_seq_count)
let bf_sp_data_len = Codec.(f_sp_data_len $ fun p -> p.sp_data_len)

let packet_codec =
  Codec.v "SpacePacket"
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
    Codec.
      [
        (Field.v "Version" (bits ~width:3 U16be) $ fun p -> p.sp_version);
        (Field.v "Type" (bits ~width:1 U16be) $ fun p -> p.sp_type);
        (Field.v "SecHdrFlag" (bits ~width:1 U16be) $ fun p -> p.sp_sec_hdr);
        bf_sp_apid;
        (Field.v "SeqFlags" (bits ~width:2 U16be) $ fun p -> p.sp_seq_flags);
        bf_sp_seq_count;
        bf_sp_data_len;
      ]

let packet_struct = Everparse.struct_of_codec packet_codec
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

(* -- 1b. Proximity-1-style frame: header + dependent-size payload --
   The FrameLength field gives the total frame size in bytes; the data
   payload occupies FrameLength - header_size bytes. This exercises
   byte_array ~size:(Field.ref f - int n) with arithmetic. *)

type full_packet = {
  fp_version : int;
  fp_type : int;
  fp_apid : int;
  fp_frame_len : int;
  fp_data : string;
}

let fp_header_size = 6
let f_fp_frame_len = Field.v "FrameLength" uint16be

let full_packet_codec =
  Codec.v "FullPacket"
    (fun version type_ apid frame_len data ->
      {
        fp_version = version;
        fp_type = type_;
        fp_apid = apid;
        fp_frame_len = frame_len;
        fp_data = data;
      })
    Codec.
      [
        (Field.v "Version" (bits ~width:3 U16be) $ fun p -> p.fp_version);
        (Field.v "Type" (bits ~width:1 U16be) $ fun p -> p.fp_type);
        (Field.v "APID" (bits ~width:12 U16be) $ fun p -> p.fp_apid);
        ( Field.v "FrameLength"
            ~constraint_:Expr.(Field.ref f_fp_frame_len >= int fp_header_size)
            uint16be
        $ fun p -> p.fp_frame_len );
        ( Field.v "Data"
            (byte_array
               ~size:Expr.(Field.ref f_fp_frame_len - int fp_header_size))
        $ fun p -> p.fp_data );
      ]

(* -- 2. CLCW: 4 bytes of bitfields -- *)

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

let cw_lockout = Field.v "Lockout" (bits ~width:1 U32be)
let cw_wait = Field.v "Wait" (bits ~width:1 U32be)
let cw_retransmit = Field.v "Retransmit" (bits ~width:1 U32be)
let cw_report = Field.v "ReportValue" (bits ~width:8 U32be)

(* Bound fields — created before the codec so they ARE the codec's fields *)
let bf_cw_lockout = Codec.(cw_lockout $ fun c -> c.cw_lockout)
let bf_cw_wait = Codec.(cw_wait $ fun c -> c.cw_wait)
let bf_cw_retransmit = Codec.(cw_retransmit $ fun c -> c.cw_retransmit)
let bf_cw_report = Codec.(cw_report $ fun c -> c.cw_report)

let clcw_codec =
  Codec.v "CLCW"
    (fun type_ version status cop vcid spare no_rf no_bitlock lockout wait
         retransmit farmb report ->
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
    Codec.
      [
        (Field.v "ControlWordType" (bits ~width:1 U32be) $ fun c -> c.cw_type);
        (Field.v "CLCWVersion" (bits ~width:2 U32be) $ fun c -> c.cw_version);
        (Field.v "StatusField" (bits ~width:3 U32be) $ fun c -> c.cw_status);
        (Field.v "COPInEffect" (bits ~width:2 U32be) $ fun c -> c.cw_cop);
        (Field.v "VCID" (bits ~width:6 U32be) $ fun c -> c.cw_vcid);
        (Field.v "Spare" (bits ~width:3 U32be) $ fun c -> c.cw_spare);
        (Field.v "NoRF" (bits ~width:1 U32be) $ fun c -> c.cw_no_rf);
        (Field.v "NoBitlock" (bits ~width:1 U32be) $ fun c -> c.cw_no_bitlock);
        bf_cw_lockout;
        bf_cw_wait;
        bf_cw_retransmit;
        (Field.v "FARMBCounter" (bits ~width:2 U32be) $ fun c -> c.cw_farmb);
        bf_cw_report;
      ]

let clcw_struct = Everparse.struct_of_codec clcw_codec
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
        lor ((i mod 32) lsl 12)
        lor ((i mod 4) lsl 10)
        lor ((i mod 256) lsl 1)
      in
      Bytes.set_int32_be b 0 (Int32.of_int w);
      b)

(* -- 3. TM Transfer Frame primary header: 6 bytes -- *)

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

let f_tf_vcid = Field.v "VCID" (bits ~width:3 U16be)
let f_tf_first_hdr = Field.v "FirstHdrPtr" (bits ~width:11 U16be)

(* Bound fields — created before the codec so they ARE the codec's fields *)
let bf_tf_vcid = Codec.(f_tf_vcid $ fun f -> f.tf_vcid)
let bf_tf_first_hdr = Codec.(f_tf_first_hdr $ fun f -> f.tf_first_hdr)

let tm_frame_codec =
  Codec.v "TMFrame"
    (fun version scid vcid ocf mc vc sec sync pkt seg hdr ->
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
    Codec.
      [
        (Field.v "Version" (bits ~width:2 U16be) $ fun f -> f.tf_version);
        (Field.v "SCID" (bits ~width:10 U16be) $ fun f -> f.tf_scid);
        bf_tf_vcid;
        (Field.v "OCFFlag" (bits ~width:1 U16be) $ fun f -> f.tf_ocf_flag);
        (Field.v "MCCount" (bits ~width:8 U16be) $ fun f -> f.tf_mc_count);
        (Field.v "VCCount" (bits ~width:8 U16be) $ fun f -> f.tf_vc_count);
        (Field.v "SecHdrFlag" (bits ~width:1 U16be) $ fun f -> f.tf_sec_hdr);
        (Field.v "SyncFlag" (bits ~width:1 U16be) $ fun f -> f.tf_sync);
        (Field.v "PacketOrder" (bits ~width:1 U16be) $ fun f -> f.tf_pkt_order);
        (Field.v "SegLenId" (bits ~width:2 U16be) $ fun f -> f.tf_seg_id);
        bf_tf_first_hdr;
      ]

let tm_frame_struct = Everparse.struct_of_codec tm_frame_codec
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

(* -- 3b. TM Frame with optional OCF --
   CCSDS: the 4-byte Operational Control Field is present when OCFFlag == 1.
   This exercises optional (Field.ref f <> int 0) typ. *)

type tm_with_ocf = {
  tmo_version : int;
  tmo_scid : int;
  tmo_vcid : int;
  tmo_ocf_flag : bool;
  tmo_mc_count : int;
  tmo_vc_count : int;
  tmo_first_hdr : int;
  tmo_ocf : int option;
}

let f_tmo_ocf_flag = Field.v "OCFFlag" (bit (bits ~width:1 U16be))

let tm_with_ocf_codec =
  Codec.v "TMWithOCF"
    (fun version scid vcid ocf_flag mc vc hdr ocf ->
      {
        tmo_version = version;
        tmo_scid = scid;
        tmo_vcid = vcid;
        tmo_ocf_flag = ocf_flag;
        tmo_mc_count = mc;
        tmo_vc_count = vc;
        tmo_first_hdr = hdr;
        tmo_ocf = ocf;
      })
    Codec.
      [
        (Field.v "Version" (bits ~width:2 U16be) $ fun f -> f.tmo_version);
        (Field.v "SCID" (bits ~width:10 U16be) $ fun f -> f.tmo_scid);
        (Field.v "VCID" (bits ~width:3 U16be) $ fun f -> f.tmo_vcid);
        (f_tmo_ocf_flag $ fun f -> f.tmo_ocf_flag);
        (Field.v "MCCount" (bits ~width:8 U16be) $ fun f -> f.tmo_mc_count);
        (Field.v "VCCount" (bits ~width:8 U16be) $ fun f -> f.tmo_vc_count);
        (Field.v "FirstHdrPtr" (bits ~width:11 U16be) $ fun f -> f.tmo_first_hdr);
        ( Field.v "OCF"
            (optional Expr.(Field.ref f_tmo_ocf_flag <> int 0) uint32be)
        $ fun f -> f.tmo_ocf );
      ]

(* -- 4. Nested protocol -- *)

type inner_cmd = { cmd_id : int; cmd_seq : int; cmd_flags : int }

let f_cmd_id = Field.v "CmdId" uint8
let f_cmd_seq = Field.v "Seq" uint16be

(* Bound fields — created before the codec so they ARE the codec's fields *)
let bf_cmd_id = Codec.(f_cmd_id $ fun c -> c.cmd_id)
let bf_cmd_seq = Codec.(f_cmd_seq $ fun c -> c.cmd_seq)

let inner_cmd_codec =
  Codec.v "InnerCmd"
    (fun id seq flags -> { cmd_id = id; cmd_seq = seq; cmd_flags = flags })
    Codec.
      [ bf_cmd_id; bf_cmd_seq; (Field.v "Flags" uint8 $ fun c -> c.cmd_flags) ]

let inner_cmd_size = Codec.wire_size inner_cmd_codec

type outer_hdr = {
  oh_version : int;
  oh_type : int;
  oh_length : int;
  oh_payload : Bytesrw.Bytes.Slice.t;
}

let f_oh_length = Field.v "Length" uint16be
let f_oh_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_oh_length))

(* Bound fields — created before the codec so they ARE the codec's fields *)
let bf_oh_length = Codec.(f_oh_length $ fun h -> h.oh_length)
let bf_oh_payload = Codec.(f_oh_payload $ fun h -> h.oh_payload)

let outer_hdr_codec =
  Codec.v "OuterHdr"
    (fun version type_ length payload ->
      {
        oh_version = version;
        oh_type = type_;
        oh_length = length;
        oh_payload = payload;
      })
    Codec.
      [
        (Field.v "Version" uint8 $ fun h -> h.oh_version);
        (Field.v "Type" uint8 $ fun h -> h.oh_type);
        bf_oh_length;
        bf_oh_payload;
      ]

let outer_hdr_size = Codec.min_wire_size outer_hdr_codec + inner_cmd_size

let nested_data n =
  Array.init n (fun i ->
      let b = Bytes.create outer_hdr_size in
      Bytes.set_uint8 b 0 1;
      Bytes.set_uint8 b 1 (i mod 4);
      Bytes.set_uint16_be b 2 inner_cmd_size;
      Bytes.set_uint8 b 4 (i mod 256);
      Bytes.set_uint16_be b 5 (i mod 65536);
      Bytes.set_uint8 b 7 (i mod 8);
      b)
