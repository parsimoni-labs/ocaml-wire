(** Benchmark schemas covering all Wire API constructs.

    Each schema is defined as a Wire.Codec with an associated struct_ for
    EverParse 3D generation. The schemas cover:

    - Integer types: uint8, uint16, uint16be, uint32, uint32be, uint64be
    - Bitfields: bf_uint8 (8 bits), bf_uint16be (16 bits), bf_uint32be (32 bits)
    - Type combinators: map, bool
    - Various struct sizes: 1B to 26B

    Realistic protocol headers (CCSDS Space Packet, CLCW, TM Frame) are included
    to show real-world performance. *)

open Wire

(* ── 1. Minimal: single uint8 = 1 byte ── *)

type minimal = { m_value : int }

let minimal_codec =
  Codec.(
    record "Minimal" (fun v -> { m_value = v })
    |+ field "Value" uint8 (fun m -> m.m_value)
    |> seal)

let minimal_struct = Codec.to_struct minimal_codec
let minimal_size = Codec.wire_size minimal_codec
let minimal_default = { m_value = 42 }

let minimal_data n =
  Array.init n (fun i ->
      let b = Bytes.create minimal_size in
      Bytes.set_uint8 b 0 (i mod 256);
      b)

(* ── 2. AllInts: all integer widths = 1+2+2+4+4+8 = 21 bytes ── *)

type all_ints = {
  ai_u8 : int;
  ai_u16 : int;
  ai_u16be : int;
  ai_u32 : int;
  ai_u32be : int;
  ai_u64be : int64;
}

let all_ints_codec =
  Codec.(
    record "AllInts" (fun u8 u16 u16be u32 u32be u64be ->
        {
          ai_u8 = u8;
          ai_u16 = u16;
          ai_u16be = u16be;
          ai_u32 = u32;
          ai_u32be = u32be;
          ai_u64be = u64be;
        })
    |+ field "U8" uint8 (fun a -> a.ai_u8)
    |+ field "U16" uint16 (fun a -> a.ai_u16)
    |+ field "U16BE" uint16be (fun a -> a.ai_u16be)
    |+ field "U32" uint32 (fun a -> a.ai_u32)
    |+ field "U32BE" uint32be (fun a -> a.ai_u32be)
    |+ field "U64BE" uint64be (fun a -> a.ai_u64be)
    |> seal)

let all_ints_struct = Codec.to_struct all_ints_codec
let all_ints_size = Codec.wire_size all_ints_codec

let all_ints_default =
  {
    ai_u8 = 0xFF;
    ai_u16 = 0x1234;
    ai_u16be = 0x5678;
    ai_u32 = 0xDEADBEEF;
    ai_u32be = 0xCAFEBABE;
    ai_u64be = 0x0102030405060708L;
  }

let all_ints_data n =
  Array.init n (fun i ->
      let b = Bytes.create all_ints_size in
      Bytes.set_uint8 b 0 (i mod 256);
      Bytes.set_uint16_le b 1 (i mod 65536);
      Bytes.set_uint16_be b 3 (i * 7 mod 65536);
      Bytes.set_int32_le b 5 (Int32.of_int (i * 13));
      Bytes.set_int32_be b 9 (Int32.of_int (i * 17));
      Bytes.set_int64_be b 13 (Int64.of_int (i * 31));
      b)

(* ── 3. Bitfield8: 3+5 bits in bf_uint8 = 1 byte ── *)

type bf8 = { bf8_tag : int; bf8_value : int }

let bf8_codec =
  Codec.(
    record "Bitfield8" (fun tag value -> { bf8_tag = tag; bf8_value = value })
    |+ field "Tag" (bits ~width:3 bf_uint8) (fun b -> b.bf8_tag)
    |+ field "Value" (bits ~width:5 bf_uint8) (fun b -> b.bf8_value)
    |> seal)

let bf8_struct = Codec.to_struct bf8_codec
let bf8_size = Codec.wire_size bf8_codec
let bf8_default = { bf8_tag = 5; bf8_value = 19 }

let bf8_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf8_size in
      let w = ((i mod 8) lsl 5) lor (i mod 32) in
      Bytes.set_uint8 b 0 w;
      b)

(* ── 4. Bitfield16: 1+4+11 bits in bf_uint16be = 2 bytes ── *)

type bf16 = { bf16_flag : int; bf16_type : int; bf16_id : int }

let bf16_codec =
  Codec.(
    record "Bitfield16" (fun flag type_ id ->
        { bf16_flag = flag; bf16_type = type_; bf16_id = id })
    |+ field "Flag" (bits ~width:1 bf_uint16be) (fun b -> b.bf16_flag)
    |+ field "Type" (bits ~width:4 bf_uint16be) (fun b -> b.bf16_type)
    |+ field "Id" (bits ~width:11 bf_uint16be) (fun b -> b.bf16_id)
    |> seal)

let bf16_struct = Codec.to_struct bf16_codec
let bf16_size = Codec.wire_size bf16_codec
let bf16_default = { bf16_flag = 1; bf16_type = 9; bf16_id = 1023 }

let bf16_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf16_size in
      let w = ((i mod 2) lsl 15) lor ((i * 3 mod 16) lsl 11) lor (i mod 2048) in
      Bytes.set_uint16_be b 0 w;
      b)

(* ── 5. Bitfield32: 4+6+14+8 bits in bf_uint32be = 4 bytes ── *)

type bf32 = {
  bf32_flags : int;
  bf32_chan : int;
  bf32_seq : int;
  bf32_pri : int;
}

let bf32_codec =
  Codec.(
    record "Bitfield32" (fun flags chan seq pri ->
        { bf32_flags = flags; bf32_chan = chan; bf32_seq = seq; bf32_pri = pri })
    |+ field "Flags" (bits ~width:4 bf_uint32be) (fun b -> b.bf32_flags)
    |+ field "Channel" (bits ~width:6 bf_uint32be) (fun b -> b.bf32_chan)
    |+ field "Seq" (bits ~width:14 bf_uint32be) (fun b -> b.bf32_seq)
    |+ field "Priority" (bits ~width:8 bf_uint32be) (fun b -> b.bf32_pri)
    |> seal)

let bf32_struct = Codec.to_struct bf32_codec
let bf32_size = Codec.wire_size bf32_codec

let bf32_default =
  { bf32_flags = 5; bf32_chan = 26; bf32_seq = 4660; bf32_pri = 171 }

let bf32_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf32_size in
      let w =
        ((i mod 16) lsl 28)
        lor ((i * 3 mod 64) lsl 22)
        lor ((i * 17 mod 16384) lsl 8)
        lor (i mod 256)
      in
      Bytes.set_int32_be b 0 (Int32.of_int w);
      b)

(* ── 6. BoolFields: bool(1bit)+bool(1bit)+6bits+uint8 = 2 bytes ── *)

type bool_fields = {
  bl_active : bool;
  bl_valid : bool;
  bl_mode : int;
  bl_code : int;
}

let bool_fields_codec =
  Codec.(
    record "BoolFields" (fun active valid mode code ->
        { bl_active = active; bl_valid = valid; bl_mode = mode; bl_code = code })
    |+ field "Active" (bool (bits ~width:1 bf_uint8)) (fun b -> b.bl_active)
    |+ field "Valid" (bool (bits ~width:1 bf_uint8)) (fun b -> b.bl_valid)
    |+ field "Mode" (bits ~width:6 bf_uint8) (fun b -> b.bl_mode)
    |+ field "Code" uint8 (fun b -> b.bl_code)
    |> seal)

let bool_fields_struct = Codec.to_struct bool_fields_codec
let bool_fields_size = Codec.wire_size bool_fields_codec

let bool_fields_default =
  { bl_active = true; bl_valid = false; bl_mode = 7; bl_code = 0xAB }

let bool_fields_data n =
  Array.init n (fun i ->
      let b = Bytes.create bool_fields_size in
      let w = ((i mod 2) lsl 7) lor (((i + 1) mod 2) lsl 6) lor (i mod 64) in
      Bytes.set_uint8 b 0 w;
      Bytes.set_uint8 b 1 (i mod 256);
      b)

(* ── 7. CCSDS Space Packet primary header: 6 bytes ──
   Real-world protocol: 3+1+1+11 bits (uint16be) + 2+14 bits (uint16be) + uint16be *)

type space_packet = {
  sp_version : int;
  sp_type : int;
  sp_sec_hdr : int;
  sp_apid : int;
  sp_seq_flags : int;
  sp_seq_count : int;
  sp_data_len : int;
}

let space_packet_codec =
  Codec.(
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
    |+ field "APID" (bits ~width:11 bf_uint16be) (fun p -> p.sp_apid)
    |+ field "SeqFlags" (bits ~width:2 bf_uint16be) (fun p -> p.sp_seq_flags)
    |+ field "SeqCount" (bits ~width:14 bf_uint16be) (fun p -> p.sp_seq_count)
    |+ field "DataLength" uint16be (fun p -> p.sp_data_len)
    |> seal)

let space_packet_struct = Codec.to_struct space_packet_codec
let space_packet_size = Codec.wire_size space_packet_codec

let space_packet_default =
  {
    sp_version = 0;
    sp_type = 0;
    sp_sec_hdr = 1;
    sp_apid = 0x7FF;
    sp_seq_flags = 3;
    sp_seq_count = 0;
    sp_data_len = 255;
  }

let space_packet_data n =
  Array.init n (fun i ->
      let b = Bytes.create space_packet_size in
      let w0 = ((i mod 2) lsl 12) lor (i mod 2048) in
      Bytes.set_uint16_be b 0 w0;
      Bytes.set_uint16_be b 2 (0xC000 lor (i mod 16384));
      Bytes.set_uint16_be b 4 (i mod 256);
      b)

(* ── 8. CLCW: 4 bytes of bitfields ──
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

let cw_type =
  Codec.field "ControlWordType" (bits ~width:1 bf_uint32be) (fun c -> c.cw_type)

let cw_version =
  Codec.field "CLCWVersion" (bits ~width:2 bf_uint32be) (fun c -> c.cw_version)

let cw_status =
  Codec.field "StatusField" (bits ~width:3 bf_uint32be) (fun c -> c.cw_status)

let cw_cop =
  Codec.field "COPInEffect" (bits ~width:2 bf_uint32be) (fun c -> c.cw_cop)

let cw_vcid =
  Codec.field "VCID" (bits ~width:6 bf_uint32be) (fun c -> c.cw_vcid)

let cw_spare =
  Codec.field "Spare" (bits ~width:2 bf_uint32be) (fun c -> c.cw_spare)

let cw_no_rf =
  Codec.field "NoRF" (bits ~width:1 bf_uint32be) (fun c -> c.cw_no_rf)

let cw_no_bitlock =
  Codec.field "NoBitlock" (bits ~width:1 bf_uint32be) (fun c -> c.cw_no_bitlock)

let cw_lockout =
  Codec.field "Lockout" (bits ~width:1 bf_uint32be) (fun c -> c.cw_lockout)

let cw_wait =
  Codec.field "Wait" (bits ~width:1 bf_uint32be) (fun c -> c.cw_wait)

let cw_retransmit =
  Codec.field "Retransmit" (bits ~width:1 bf_uint32be) (fun c ->
      c.cw_retransmit)

let cw_farmb =
  Codec.field "FARMBCounter" (bits ~width:2 bf_uint32be) (fun c -> c.cw_farmb)

let cw_report =
  Codec.field "ReportValue" (bits ~width:8 bf_uint32be) (fun c -> c.cw_report)

let clcw_codec =
  Codec.(
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
    |+ cw_type |+ cw_version |+ cw_status |+ cw_cop |+ cw_vcid |+ cw_spare
    |+ cw_no_rf |+ cw_no_bitlock |+ cw_lockout |+ cw_wait |+ cw_retransmit
    |+ cw_farmb |+ cw_report |> seal)

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

(* ── 9. TM Transfer Frame primary header: 6 bytes ──
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

let tm_frame_codec =
  Codec.(
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
    |+ field "SCID" (bits ~width:10 bf_uint16be) (fun f -> f.tf_scid)
    |+ field "VCID" (bits ~width:3 bf_uint16be) (fun f -> f.tf_vcid)
    |+ field "OCFFlag" (bits ~width:1 bf_uint16be) (fun f -> f.tf_ocf_flag)
    |+ field "MCCount" (bits ~width:8 bf_uint16be) (fun f -> f.tf_mc_count)
    |+ field "VCCount" (bits ~width:8 bf_uint16be) (fun f -> f.tf_vc_count)
    |+ field "SecHdrFlag" (bits ~width:1 bf_uint16be) (fun f -> f.tf_sec_hdr)
    |+ field "SyncFlag" (bits ~width:1 bf_uint16be) (fun f -> f.tf_sync)
    |+ field "PacketOrder" (bits ~width:1 bf_uint16be) (fun f -> f.tf_pkt_order)
    |+ field "SegLenId" (bits ~width:2 bf_uint16be) (fun f -> f.tf_seg_id)
    |+ field "FirstHdrPtr" (bits ~width:11 bf_uint16be) (fun f ->
        f.tf_first_hdr)
    |> seal)

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

(* ── 10. Large mixed: u32be+u8+u8+u16be+u8+u8+u16be+u16be+u32be+u64be = 26 bytes ── *)

type large_mixed = {
  lg_sync : int;
  lg_version : int;
  lg_type : int;
  lg_spacecraft : int;
  lg_vcid : int;
  lg_count : int;
  lg_offset : int;
  lg_length : int;
  lg_crc : int;
  lg_timestamp : int64;
}

let large_mixed_codec =
  Codec.(
    record "LargeMixed"
      (fun
        sync version type_ spacecraft vcid count offset length crc timestamp ->
        {
          lg_sync = sync;
          lg_version = version;
          lg_type = type_;
          lg_spacecraft = spacecraft;
          lg_vcid = vcid;
          lg_count = count;
          lg_offset = offset;
          lg_length = length;
          lg_crc = crc;
          lg_timestamp = timestamp;
        })
    |+ field "SyncMarker" uint32be (fun l -> l.lg_sync)
    |+ field "Version" uint8 (fun l -> l.lg_version)
    |+ field "Type" uint8 (fun l -> l.lg_type)
    |+ field "SpacecraftId" uint16be (fun l -> l.lg_spacecraft)
    |+ field "VCID" uint8 (fun l -> l.lg_vcid)
    |+ field "FrameCount" uint8 (fun l -> l.lg_count)
    |+ field "DataOffset" uint16be (fun l -> l.lg_offset)
    |+ field "DataLength" uint16be (fun l -> l.lg_length)
    |+ field "CRC" uint32be (fun l -> l.lg_crc)
    |+ field "Timestamp" uint64be (fun l -> l.lg_timestamp)
    |> seal)

let large_mixed_struct = Codec.to_struct large_mixed_codec
let large_mixed_size = Codec.wire_size large_mixed_codec

let large_mixed_default =
  {
    lg_sync = 0x1ACFFC1D;
    lg_version = 2;
    lg_type = 0;
    lg_spacecraft = 0x01FF;
    lg_vcid = 3;
    lg_count = 66;
    lg_offset = 16;
    lg_length = 1024;
    lg_crc = 0xDEADBEEF;
    lg_timestamp = 0x0102030405060708L;
  }

let large_mixed_data n =
  Array.init n (fun i ->
      let b = Bytes.create large_mixed_size in
      Bytes.set_int32_be b 0 0x1ACFFC1Dl;
      Bytes.set_uint8 b 4 (i mod 4);
      Bytes.set_uint8 b 5 (i mod 3);
      Bytes.set_uint16_be b 6 (i mod 1024);
      Bytes.set_uint8 b 8 (i mod 8);
      Bytes.set_uint8 b 9 (i mod 256);
      Bytes.set_uint16_be b 10 16;
      Bytes.set_uint16_be b 12 (i mod 2048);
      Bytes.set_int32_be b 14 (Int32.of_int (i * 0x1234));
      Bytes.set_int64_be b 18 (Int64.of_int (i * 1_000_000));
      b)

(* ── Schema registry for benchmark infrastructure ── *)

type 'a schema = {
  name : string;
  codec : 'a Codec.t;
  struct_ : struct_;
  size : int;
  default : 'a;
  make_data : int -> bytes array;
  decode : bytes -> int -> 'a;
  encode : 'a -> bytes -> int -> unit;
}

let schema name codec struct_ size default make_data =
  {
    name;
    codec;
    struct_;
    size;
    default;
    make_data;
    decode = Codec.decode codec;
    encode = Codec.encode codec;
  }

(* Type-erased wrapper for iterating over all schemas *)
type any_schema = Any : 'a schema -> any_schema

let all_schemas =
  [
    Any
      (schema "Minimal" minimal_codec minimal_struct minimal_size
         minimal_default minimal_data);
    Any
      (schema "AllInts" all_ints_codec all_ints_struct all_ints_size
         all_ints_default all_ints_data);
    Any (schema "Bitfield8" bf8_codec bf8_struct bf8_size bf8_default bf8_data);
    Any
      (schema "Bitfield16" bf16_codec bf16_struct bf16_size bf16_default
         bf16_data);
    Any
      (schema "Bitfield32" bf32_codec bf32_struct bf32_size bf32_default
         bf32_data);
    Any
      (schema "BoolFields" bool_fields_codec bool_fields_struct bool_fields_size
         bool_fields_default bool_fields_data);
    Any
      (schema "SpacePacket" space_packet_codec space_packet_struct
         space_packet_size space_packet_default space_packet_data);
    Any (schema "CLCW" clcw_codec clcw_struct clcw_size clcw_default clcw_data);
    Any
      (schema "TMFrame" tm_frame_codec tm_frame_struct tm_frame_size
         tm_frame_default tm_frame_data);
    Any
      (schema "LargeMixed" large_mixed_codec large_mixed_struct large_mixed_size
         large_mixed_default large_mixed_data);
  ]

(* All struct_ definitions for 3D generation *)
let all_structs = List.map (fun (Any s) -> s.struct_) all_schemas
