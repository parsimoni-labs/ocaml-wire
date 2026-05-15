(** CCSDS space protocol codecs.

    Real-world protocol headers (CCSDS Space Packet, CLCW, TM Frame) and a
    nested protocol demo (InnerCmd/OuterHdr) for zero-copy access. *)

open Wire

(* -- 1. CCSDS Space Packet primary header: 6 bytes -- *)

type packet = {
  version : int;
  type_ : int;
  sec_hdr : int;
  apid : int;
  seq_flags : int;
  seq_count : int;
  data_len : int;
}

let f_sp_apid = Field.v "APID" (bits ~width:11 U16be)
let f_sp_seq_count = Field.v "SeqCount" (bits ~width:14 U16be)
let f_sp_data_len = Field.v "DataLength" uint16be

(* Bound fields -- created before the codec so they ARE the codec's fields *)
let bf_sp_apid = Codec.(f_sp_apid $ fun (p : packet) -> p.apid)
let bf_sp_seq_count = Codec.(f_sp_seq_count $ fun (p : packet) -> p.seq_count)
let bf_sp_data_len = Codec.(f_sp_data_len $ fun (p : packet) -> p.data_len)

let packet_codec =
  Codec.v "SpacePacket"
    (fun version type_ sec_hdr apid seq_flags seq_count data_len ->
      ({ version; type_; sec_hdr; apid; seq_flags; seq_count; data_len }
        : packet))
    Codec.
      [
        (Field.v "Version" (bits ~width:3 U16be) $ fun (p : packet) -> p.version);
        (Field.v "Type" (bits ~width:1 U16be) $ fun (p : packet) -> p.type_);
        ( Field.v "SecHdrFlag" (bits ~width:1 U16be) $ fun (p : packet) ->
          p.sec_hdr );
        bf_sp_apid;
        ( Field.v "SeqFlags" (bits ~width:2 U16be) $ fun (p : packet) ->
          p.seq_flags );
        bf_sp_seq_count;
        bf_sp_data_len;
      ]

let packet_struct = Everparse.struct_of_codec packet_codec
let packet_size = Codec.wire_size packet_codec

let packet_default =
  {
    version = 0;
    type_ = 0;
    sec_hdr = 1;
    apid = 0x7FF;
    seq_flags = 3;
    seq_count = 0;
    data_len = 255;
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
  version : int;
  type_ : int;
  apid : int;
  frame_len : int;
  data : string;
}

let fp_header_size = 6
let f_fp_frame_len = Field.v "FrameLength" uint16be

let full_packet_codec =
  Codec.v "FullPacket"
    (fun version type_ apid frame_len data ->
      ({ version; type_; apid; frame_len; data } : full_packet))
    Codec.
      [
        ( Field.v "Version" (bits ~width:3 U16be) $ fun (p : full_packet) ->
          p.version );
        (Field.v "Type" (bits ~width:1 U16be) $ fun (p : full_packet) -> p.type_);
        (Field.v "APID" (bits ~width:12 U16be) $ fun (p : full_packet) -> p.apid);
        ( Field.v "FrameLength"
            ~constraint_:Expr.(Field.ref f_fp_frame_len >= int fp_header_size)
            uint16be
        $ fun (p : full_packet) -> p.frame_len );
        ( Field.v "Data"
            (byte_array
               ~size:Expr.(Field.ref f_fp_frame_len - int fp_header_size))
        $ fun (p : full_packet) -> p.data );
      ]

(* -- 2. CLCW: 4 bytes of bitfields -- *)

type clcw = {
  type_ : int;
  version : int;
  status : int;
  cop : int;
  vcid : int;
  spare : int;
  no_rf : int;
  no_bitlock : int;
  lockout : int;
  wait : int;
  retransmit : int;
  farmb : int;
  report : int;
}

let cw_lockout = Field.v "Lockout" (bits ~width:1 U32be)
let cw_wait = Field.v "Wait" (bits ~width:1 U32be)
let cw_retransmit = Field.v "Retransmit" (bits ~width:1 U32be)
let cw_report = Field.v "ReportValue" (bits ~width:8 U32be)

(* Bound fields -- created before the codec so they ARE the codec's fields *)
let bf_cw_lockout = Codec.(cw_lockout $ fun (c : clcw) -> c.lockout)
let bf_cw_wait = Codec.(cw_wait $ fun (c : clcw) -> c.wait)
let bf_cw_retransmit = Codec.(cw_retransmit $ fun (c : clcw) -> c.retransmit)
let bf_cw_report = Codec.(cw_report $ fun (c : clcw) -> c.report)

let clcw_codec =
  Codec.v "CLCW"
    (fun type_ version status cop vcid spare no_rf no_bitlock lockout wait
         retransmit farmb report ->
      ({
         type_;
         version;
         status;
         cop;
         vcid;
         spare;
         no_rf;
         no_bitlock;
         lockout;
         wait;
         retransmit;
         farmb;
         report;
       }
        : clcw))
    Codec.
      [
        ( Field.v "ControlWordType" (bits ~width:1 U32be) $ fun (c : clcw) ->
          c.type_ );
        ( Field.v "CLCWVersion" (bits ~width:2 U32be) $ fun (c : clcw) ->
          c.version );
        ( Field.v "StatusField" (bits ~width:3 U32be) $ fun (c : clcw) ->
          c.status );
        (Field.v "COPInEffect" (bits ~width:2 U32be) $ fun (c : clcw) -> c.cop);
        (Field.v "VCID" (bits ~width:6 U32be) $ fun (c : clcw) -> c.vcid);
        (Field.v "Spare" (bits ~width:3 U32be) $ fun (c : clcw) -> c.spare);
        (Field.v "NoRF" (bits ~width:1 U32be) $ fun (c : clcw) -> c.no_rf);
        ( Field.v "NoBitlock" (bits ~width:1 U32be) $ fun (c : clcw) ->
          c.no_bitlock );
        bf_cw_lockout;
        bf_cw_wait;
        bf_cw_retransmit;
        ( Field.v "FARMBCounter" (bits ~width:2 U32be) $ fun (c : clcw) ->
          c.farmb );
        bf_cw_report;
      ]

let clcw_struct = Everparse.struct_of_codec clcw_codec
let clcw_size = Codec.wire_size clcw_codec

let clcw_default =
  {
    type_ = 0;
    version = 0;
    status = 0;
    cop = 1;
    vcid = 7;
    spare = 0;
    no_rf = 0;
    no_bitlock = 0;
    lockout = 0;
    wait = 0;
    retransmit = 0;
    farmb = 0;
    report = 42;
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
  version : int;
  scid : int;
  vcid : int;
  ocf_flag : int;
  mc_count : int;
  vc_count : int;
  sec_hdr : int;
  sync : int;
  packet_order : int;
  seg_id : int;
  first_hdr : int;
}

let f_tf_vcid = Field.v "VCID" (bits ~width:3 U16be)
let f_tf_first_hdr = Field.v "FirstHdrPtr" (bits ~width:11 U16be)

(* Bound fields -- created before the codec so they ARE the codec's fields *)
let bf_tf_vcid = Codec.(f_tf_vcid $ fun (f : tm_frame) -> f.vcid)
let bf_tf_first_hdr = Codec.(f_tf_first_hdr $ fun (f : tm_frame) -> f.first_hdr)

let tm_frame_codec =
  Codec.v "TMFrame"
    (fun version scid vcid ocf mc vc sec sync pkt seg hdr ->
      ({
         version;
         scid;
         vcid;
         ocf_flag = ocf;
         mc_count = mc;
         vc_count = vc;
         sec_hdr = sec;
         sync;
         packet_order = pkt;
         seg_id = seg;
         first_hdr = hdr;
       }
        : tm_frame))
    Codec.
      [
        ( Field.v "Version" (bits ~width:2 U16be) $ fun (f : tm_frame) ->
          f.version );
        (Field.v "SCID" (bits ~width:10 U16be) $ fun (f : tm_frame) -> f.scid);
        bf_tf_vcid;
        ( Field.v "OCFFlag" (bits ~width:1 U16be) $ fun (f : tm_frame) ->
          f.ocf_flag );
        ( Field.v "MCCount" (bits ~width:8 U16be) $ fun (f : tm_frame) ->
          f.mc_count );
        ( Field.v "VCCount" (bits ~width:8 U16be) $ fun (f : tm_frame) ->
          f.vc_count );
        ( Field.v "SecHdrFlag" (bits ~width:1 U16be) $ fun (f : tm_frame) ->
          f.sec_hdr );
        (Field.v "SyncFlag" (bits ~width:1 U16be) $ fun (f : tm_frame) -> f.sync);
        ( Field.v "PacketOrder" (bits ~width:1 U16be) $ fun (f : tm_frame) ->
          f.packet_order );
        ( Field.v "SegLenId" (bits ~width:2 U16be) $ fun (f : tm_frame) ->
          f.seg_id );
        bf_tf_first_hdr;
      ]

let tm_frame_struct = Everparse.struct_of_codec tm_frame_codec
let tm_frame_size = Codec.wire_size tm_frame_codec

let tm_frame_default =
  {
    version = 0;
    scid = 0x1FF;
    vcid = 3;
    ocf_flag = 1;
    mc_count = 0;
    vc_count = 0;
    sec_hdr = 0;
    sync = 0;
    packet_order = 0;
    seg_id = 3;
    first_hdr = 0x7FE;
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
  version : int;
  scid : int;
  vcid : int;
  ocf_flag : bool;
  mc_count : int;
  vc_count : int;
  first_hdr : int;
  ocf : int option;
}

let f_tmo_ocf_flag = Field.v "OCFFlag" (bit (bits ~width:1 U16be))

let tm_with_ocf_codec =
  Codec.v "TMWithOCF"
    (fun version scid vcid ocf_flag mc vc hdr ocf ->
      ({
         version;
         scid;
         vcid;
         ocf_flag;
         mc_count = mc;
         vc_count = vc;
         first_hdr = hdr;
         ocf;
       }
        : tm_with_ocf))
    Codec.
      [
        ( Field.v "Version" (bits ~width:2 U16be) $ fun (f : tm_with_ocf) ->
          f.version );
        (Field.v "SCID" (bits ~width:10 U16be) $ fun (f : tm_with_ocf) -> f.scid);
        (Field.v "VCID" (bits ~width:3 U16be) $ fun (f : tm_with_ocf) -> f.vcid);
        (f_tmo_ocf_flag $ fun (f : tm_with_ocf) -> f.ocf_flag);
        ( Field.v "MCCount" (bits ~width:8 U16be) $ fun (f : tm_with_ocf) ->
          f.mc_count );
        ( Field.v "VCCount" (bits ~width:8 U16be) $ fun (f : tm_with_ocf) ->
          f.vc_count );
        ( Field.v "FirstHdrPtr" (bits ~width:11 U16be) $ fun (f : tm_with_ocf) ->
          f.first_hdr );
        ( Field.optional "OCF"
            ~present:Expr.(Field.ref f_tmo_ocf_flag <> int 0)
            uint32be
        $ fun (f : tm_with_ocf) -> f.ocf );
      ]

(* -- 4. Nested protocol -- *)

type inner_cmd = { id : int; seq : int; flags : int }

let f_cmd_id = Field.v "CmdId" uint8
let f_cmd_seq = Field.v "Seq" uint16be

(* Bound fields -- created before the codec so they ARE the codec's fields *)
let bf_cmd_id = Codec.(f_cmd_id $ fun (c : inner_cmd) -> c.id)
let bf_cmd_seq = Codec.(f_cmd_seq $ fun (c : inner_cmd) -> c.seq)

let inner_cmd_codec =
  Codec.v "InnerCmd"
    (fun id seq flags -> ({ id; seq; flags } : inner_cmd))
    Codec.
      [
        bf_cmd_id;
        bf_cmd_seq;
        (Field.v "Flags" uint8 $ fun (c : inner_cmd) -> c.flags);
      ]

let inner_cmd_size = Codec.wire_size inner_cmd_codec

type outer_hdr = {
  version : int;
  type_ : int;
  length : int;
  payload : Bytesrw.Bytes.Slice.t;
}

let f_oh_length = Field.v "Length" uint16be
let f_oh_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_oh_length))

(* Bound fields -- created before the codec so they ARE the codec's fields *)
let bf_oh_length = Codec.(f_oh_length $ fun (h : outer_hdr) -> h.length)
let bf_oh_payload = Codec.(f_oh_payload $ fun (h : outer_hdr) -> h.payload)

let outer_hdr_codec =
  Codec.v "OuterHdr"
    (fun version type_ length payload ->
      ({ version; type_; length; payload } : outer_hdr))
    Codec.
      [
        (Field.v "Version" uint8 $ fun (h : outer_hdr) -> h.version);
        (Field.v "Type" uint8 $ fun (h : outer_hdr) -> h.type_);
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
