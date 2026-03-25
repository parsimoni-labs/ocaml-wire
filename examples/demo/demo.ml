(** Synthetic benchmark schemas exercising all Wire API constructs.

    These schemas are designed for benchmarking and testing coverage, not based
    on real protocols. They cover:

    - Integer types: uint8, uint16, uint16be, uint32, uint32be, uint64be
    - Bitfields: U8 (8 bits), U16be (16 bits), U32be (32 bits)
    - Type combinators: map, bool
    - Various struct sizes: 1B to 26B *)

open Wire
open Wire.Everparse.Raw

(* ── 1. Minimal: single uint8 = 1 byte ── *)

type minimal = { m_value : int }

let f_minimal_value = Field.v "Value" uint8
let bf_minimal_value = Codec.(f_minimal_value $ fun m -> m.m_value)

let minimal_codec =
  Codec.v "Minimal" (fun v -> { m_value = v }) [ bf_minimal_value ]

let minimal_struct = Everparse.struct_of_codec minimal_codec
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

let f_ints_u64be = Field.v "U64BE" uint64be
let bf_ints_u64be = Codec.(f_ints_u64be $ fun a -> a.ai_u64be)

let all_ints_codec =
  Codec.v "AllInts"
    (fun u8 u16 u16be u32 u32be u64be ->
      {
        ai_u8 = u8;
        ai_u16 = u16;
        ai_u16be = u16be;
        ai_u32 = u32;
        ai_u32be = u32be;
        ai_u64be = u64be;
      })
    Codec.
      [
        (Field.v "U8" uint8 $ fun a -> a.ai_u8);
        (Field.v "U16" uint16 $ fun a -> a.ai_u16);
        (Field.v "U16BE" uint16be $ fun a -> a.ai_u16be);
        (Field.v "U32" uint32 $ fun a -> a.ai_u32);
        (Field.v "U32BE" uint32be $ fun a -> a.ai_u32be);
        bf_ints_u64be;
      ]

let all_ints_struct = Everparse.struct_of_codec all_ints_codec
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

(* ── 3. Bitfield8: 3+5 bits in U8 = 1 byte ── *)

type bf8 = { bf8_tag : int; bf8_value : int }

let f_bf8_value = Field.v "Value" (bits ~width:5 U8)
let bf_bf8_value = Codec.(f_bf8_value $ fun b -> b.bf8_value)

let bf8_codec =
  Codec.v "Bitfield8"
    (fun tag value -> { bf8_tag = tag; bf8_value = value })
    Codec.
      [ (Field.v "Tag" (bits ~width:3 U8) $ fun b -> b.bf8_tag); bf_bf8_value ]

let bf8_struct = Everparse.struct_of_codec bf8_codec
let bf8_size = Codec.wire_size bf8_codec
let bf8_default = { bf8_tag = 5; bf8_value = 19 }

let bf8_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf8_size in
      let w = ((i mod 8) lsl 5) lor (i mod 32) in
      Bytes.set_uint8 b 0 w;
      b)

(* ── 4. Bitfield16: 1+4+11 bits in U16be = 2 bytes ── *)

type bf16 = { bf16_flag : int; bf16_type : int; bf16_id : int }

let f_bf16_id = Field.v "Id" (bits ~width:11 U16be)
let bf_bf16_id = Codec.(f_bf16_id $ fun b -> b.bf16_id)

let bf16_codec =
  Codec.v "Bitfield16"
    (fun flag type_ id -> { bf16_flag = flag; bf16_type = type_; bf16_id = id })
    Codec.
      [
        (Field.v "Flag" (bits ~width:1 U16be) $ fun b -> b.bf16_flag);
        (Field.v "Type" (bits ~width:4 U16be) $ fun b -> b.bf16_type);
        bf_bf16_id;
      ]

let bf16_struct = Everparse.struct_of_codec bf16_codec
let bf16_size = Codec.wire_size bf16_codec
let bf16_default = { bf16_flag = 1; bf16_type = 9; bf16_id = 1023 }

let bf16_data n =
  Array.init n (fun i ->
      let b = Bytes.create bf16_size in
      let w = ((i mod 2) lsl 15) lor ((i * 3 mod 16) lsl 11) lor (i mod 2048) in
      Bytes.set_uint16_be b 0 w;
      b)

(* ── 5. Bitfield32: 4+6+14+8 bits in U32be = 4 bytes ── *)

type bf32 = {
  bf32_flags : int;
  bf32_chan : int;
  bf32_seq : int;
  bf32_pri : int;
}

let f_bf32_pri = Field.v "Priority" (bits ~width:8 U32be)
let bf_bf32_pri = Codec.(f_bf32_pri $ fun b -> b.bf32_pri)

let bf32_codec =
  Codec.v "Bitfield32"
    (fun flags chan seq pri ->
      { bf32_flags = flags; bf32_chan = chan; bf32_seq = seq; bf32_pri = pri })
    Codec.
      [
        (Field.v "Flags" (bits ~width:4 U32be) $ fun b -> b.bf32_flags);
        (Field.v "Channel" (bits ~width:6 U32be) $ fun b -> b.bf32_chan);
        (Field.v "Seq" (bits ~width:14 U32be) $ fun b -> b.bf32_seq);
        bf_bf32_pri;
      ]

let bf32_struct = Everparse.struct_of_codec bf32_codec
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

let f_bool_active = Field.v "Active" (bool (bits ~width:1 U8))
let bf_bool_active = Codec.(f_bool_active $ fun b -> b.bl_active)

let bool_fields_codec =
  Codec.v "BoolFields"
    (fun active valid mode code ->
      { bl_active = active; bl_valid = valid; bl_mode = mode; bl_code = code })
    Codec.
      [
        bf_bool_active;
        (Field.v "Valid" (bool (bits ~width:1 U8)) $ fun b -> b.bl_valid);
        (Field.v "Mode" (bits ~width:6 U8) $ fun b -> b.bl_mode);
        (Field.v "Code" uint8 $ fun b -> b.bl_code);
      ]

let bool_fields_struct = Everparse.struct_of_codec bool_fields_codec
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

(* ── 7. Large mixed: u32be+u8+u8+u16be+u8+u8+u16be+u16be+u32be+u64be = 26 bytes ── *)

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

let f_mixed_timestamp = Field.v "Timestamp" uint64be
let bf_mixed_timestamp = Codec.(f_mixed_timestamp $ fun l -> l.lg_timestamp)

let large_mixed_codec =
  Codec.v "LargeMixed"
    (fun sync version type_ spacecraft vcid count offset length crc timestamp ->
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
    Codec.
      [
        (Field.v "SyncMarker" uint32be $ fun l -> l.lg_sync);
        (Field.v "Version" uint8 $ fun l -> l.lg_version);
        (Field.v "Type" uint8 $ fun l -> l.lg_type);
        (Field.v "SpacecraftId" uint16be $ fun l -> l.lg_spacecraft);
        (Field.v "VCID" uint8 $ fun l -> l.lg_vcid);
        (Field.v "FrameCount" uint8 $ fun l -> l.lg_count);
        (Field.v "DataOffset" uint16be $ fun l -> l.lg_offset);
        (Field.v "DataLength" uint16be $ fun l -> l.lg_length);
        (Field.v "CRC" uint32be $ fun l -> l.lg_crc);
        bf_mixed_timestamp;
      ]

let large_mixed_struct = Everparse.struct_of_codec large_mixed_codec
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

(* ── 8. Mapped: user-defined decode/encode via map combinator = 2 bytes ──
   The map decode function runs on every Codec.get call, adding a function
   call to the hot path. On the C side, map is invisible (3D sees the inner
   type), so this measures the OCaml mapping overhead vs bare C validation. *)

type priority = Low | Medium | High | Critical

let priority_of_int = function
  | 0 -> Low
  | 1 -> Medium
  | 2 -> High
  | _ -> Critical

let int_of_priority = function
  | Low -> 0
  | Medium -> 1
  | High -> 2
  | Critical -> 3

type mapped = { mp_priority : priority; mp_value : int }

let f_mp_priority =
  Field.v "Priority" (map ~decode:priority_of_int ~encode:int_of_priority uint8)

let f_mp_value = Field.v "Value" uint8
let bf_mp_priority = Codec.(f_mp_priority $ fun m -> m.mp_priority)

let mapped_codec =
  Codec.v "Mapped"
    (fun pri value -> { mp_priority = pri; mp_value = value })
    Codec.[ bf_mp_priority; (f_mp_value $ fun m -> m.mp_value) ]

let mapped_struct = Everparse.struct_of_codec mapped_codec
let mapped_size = Codec.wire_size mapped_codec
let mapped_default = { mp_priority = High; mp_value = 42 }

let mapped_data n =
  let buf = Bytes.create (n * mapped_size) in
  for i = 0 to n - 1 do
    Codec.encode mapped_codec
      { mp_priority = priority_of_int (i mod 4); mp_value = i mod 256 }
      buf (i * mapped_size)
  done;
  buf

(* ── 9. CasesDemo: variant dispatch via cases combinator = 1 byte ──
   The cases combinator uses an array lookup on decode (Codec.get) and a
   linear scan on encode (Codec.set). On the C side, it's just a 1-bit
   bitfield — the variant mapping is OCaml-only. *)

type ptype = Telemetry | Telecommand
type cases_demo = { cd_type : ptype; cd_id : int }

let f_cd_type =
  Field.v "PacketType"
    (variants "PacketType"
       [ ("TM", Telemetry); ("TC", Telecommand) ]
       (bits ~width:1 U8))

let f_cd_id = Field.v "Id" (bits ~width:7 U8)
let bf_cd_type = Codec.(f_cd_type $ fun c -> c.cd_type)

let cases_demo_codec =
  Codec.v "CasesDemo"
    (fun ptype id -> { cd_type = ptype; cd_id = id })
    Codec.[ bf_cd_type; (f_cd_id $ fun c -> c.cd_id) ]

let cases_demo_struct = Everparse.struct_of_codec cases_demo_codec
let cases_demo_size = Codec.wire_size cases_demo_codec
let cases_demo_default = { cd_type = Telemetry; cd_id = 42 }

let cases_demo_data n =
  let buf = Bytes.create (n * cases_demo_size) in
  for i = 0 to n - 1 do
    Codec.encode cases_demo_codec
      {
        cd_type = (if i mod 2 = 0 then Telemetry else Telecommand);
        cd_id = i mod 128;
      }
      buf (i * cases_demo_size)
  done;
  buf

(* ── 10. EnumDemo: enum + map for OCaml variant types = 2 bytes ──
   Combines enum (3D validation: rejects values outside {0,1,2,3}) with map
   (OCaml variant decode/encode). EverParse C validates enum membership;
   Codec.get calls the map decode on every read. *)

type status = [ `Ok | `Warn | `Err | `Crit ]
type enum_demo = { en_status : status; en_code : int }

let f_en_status =
  Field.v "StatusCode"
    (variants "Status"
       [ ("OK", `Ok); ("WARN", `Warn); ("ERR", `Err); ("CRIT", `Crit) ]
       uint8)

let f_en_code = Field.v "Code" uint8
let bf_en_status = Codec.(f_en_status $ fun e -> e.en_status)

let enum_demo_codec =
  Codec.v "EnumDemo"
    (fun status code -> { en_status = status; en_code = code })
    Codec.[ bf_en_status; (f_en_code $ fun e -> e.en_code) ]

let enum_demo_struct = Everparse.struct_of_codec enum_demo_codec
let enum_demo_size = Codec.wire_size enum_demo_codec
let enum_demo_default = { en_status = `Ok; en_code = 42 }

let enum_demo_data n =
  let buf = Bytes.create (n * enum_demo_size) in
  let statuses = [| `Ok; `Warn; `Err; `Crit |] in
  for i = 0 to n - 1 do
    Codec.encode enum_demo_codec
      { en_status = statuses.(i mod 4); en_code = i mod 256 }
      buf (i * enum_demo_size)
  done;
  buf

(* ── 11. Constrained: where clause, validation on C side only = 2 bytes ──
   The where constraint generates a check in the EverParse C validator
   (Version must be 0) but Codec.get strips the constraint entirely.
   This measures C constraint-checking overhead vs OCaml unchecked read. *)

type constrained = { co_version : int; co_data : int }

let f_co_version_c = field "Version" uint8

let f_co_version =
  Field.v "Version" (where Expr.(field_ref f_co_version_c = int 0) uint8)

let f_co_data = Field.v "Data" uint8
let bf_co_data = Codec.(f_co_data $ fun c -> c.co_data)

let constrained_codec =
  Codec.v "Constrained"
    (fun version data -> { co_version = version; co_data = data })
    Codec.[ (f_co_version $ fun c -> c.co_version); bf_co_data ]

let constrained_struct = Everparse.struct_of_codec constrained_codec
let constrained_size = Codec.wire_size constrained_codec
let constrained_default = { co_version = 0; co_data = 42 }

let constrained_data n =
  let buf = Bytes.create (n * constrained_size) in
  for i = 0 to n - 1 do
    Codec.encode constrained_codec
      { co_version = 0; co_data = i mod 256 }
      buf (i * constrained_size)
  done;
  buf

(* ══════════════════════════════════════════════════════════════════════════
   3D Feature Coverage
   ══════════════════════════════════════════════════════════════════════════
   The following demonstrate Wire DSL features targeting EverParse 3D output.
   They don't have Codec views (variable-size or 3D-only) but exercise the
   full API surface and produce valid 3D modules via Wire.to_3d. *)

(* ── 12. Array: repeated fixed-count elements ── *)

let f_count = field "Count" uint8

let array_struct =
  struct_ "ArrayDemo"
    [
      field "Count" uint8;
      field "Items" (array ~len:(field_ref f_count) uint16be);
    ]

(* ── 13. ByteArray: byte blob with copy (vs byte_slice zero-copy) ── *)

let f_ba_length = field "Length" uint16be

let byte_array_struct =
  struct_ "ByteArrayDemo"
    [
      field "Length" uint16be;
      field "Data" (byte_array ~size:(field_ref f_ba_length));
    ]

(* ── 14. Trailing / padding types ── *)

let all_bytes_struct =
  struct_ "TrailingData" [ field "Header" uint32be; field "Rest" all_bytes ]

let all_zeros_struct =
  struct_ "PaddedMsg" [ field "Value" uint16be; field "Padding" all_zeros ]

(* ── 15. SingleElemArray: single element with byte-size constraint ── *)

let f_se_size = field "Size" uint16be

let single_elem_struct =
  struct_ "SingleElem"
    [
      field "Size" uint16be;
      field "Elem" (nested ~size:(field_ref f_se_size) uint32be);
    ]

let f_se_maxsize = field "MaxSize" uint16be

let single_elem_at_most_struct =
  struct_ "SingleElemAtMost"
    [
      field "MaxSize" uint16be;
      field "Elem" (nested_at_most ~size:(field_ref f_se_maxsize) uint16be);
    ]

(* ── 16. Anonymous fields (padding) ── *)

let anon_field_struct =
  struct_ "WithPadding"
    [ field "X" uint8; anon_field uint8; field "Y" uint16be ]

(* ── 17. Actions: side-effects during EverParse validation ── *)

let f_magic = field "Magic" uint32be

let action_struct =
  struct_ "Validated"
    [
      field "Magic" uint32be;
      field "Data"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(field_ref f_magic = int 0x1ACFFC1D) ])
        uint16be;
    ]

(* ── 18. Actions: full — Action.var, Action.if_, Action.assign, Action.abort ── *)

let action_full_struct =
  let out_value = Param.output "out_value" uint32be in
  let f_tag = field "Tag" uint8 in
  let f_value = field "Value" uint16be in
  let f_x = field "x" uint16be in
  param_struct "ActionFull"
    [ Param.v out_value ]
    [
      field "Tag" uint8;
      field "Value"
        ~action:
          (Action.on_act
             [
               Action.var "x" Expr.(field_ref f_tag + field_ref f_value);
               Action.if_
                 Expr.(field_ref f_x > int 0)
                 [ Action.assign out_value (field_ref f_x) ]
                 (Some [ Action.abort ]);
             ])
        uint16be;
    ]

(* ── 19. Parameterized struct: reusable with constraints ── *)

let param_demo_struct =
  let out_len = Param.output "out_len" uint16be in
  let f_pd_length = field "Length" uint16be in
  let f_pd_max_len = field "max_len" uint16be in
  param_struct "BoundedPayload"
    [ param "max_len" uint16be; Param.v out_len ]
    ~where:Expr.(field_ref f_pd_length <= field_ref f_pd_max_len)
    [
      field "Length"
        ~action:
          (Action.on_success [ Action.assign out_len (field_ref f_pd_length) ])
        uint16be;
      field "Data" (byte_array ~size:(field_ref f_pd_length));
    ]

(* ── 20. Casetype: tagged union (different wire layout per tag) ──
   Unlike enum (same wire size, different named values), casetype selects
   a different struct/type based on a discriminant field. *)

let casetype_module =
  let f_dp_length = field "Length" uint16be in
  let f_msg_kind = field "Kind" uint8 in
  module_
    [
      typedef (struct_ "PingPayload" [ field "Timestamp" uint64be ]);
      typedef
        (struct_ "DataPayload"
           [
             field "SeqNo" uint32be;
             field "Length" uint16be;
             field "Body" (byte_array ~size:(field_ref f_dp_length));
           ]);
      typedef (struct_ "AckPayload" [ field "AckedSeqNo" uint32be ]);
      casetype_decl "MsgBody"
        [ param "kind" uint8 ]
        uint8
        [
          decl_case 0 (type_ref "PingPayload");
          decl_case 1 (type_ref "DataPayload");
          decl_case 2 (type_ref "AckPayload");
          decl_default uint8;
        ];
      typedef ~entrypoint:true
        (struct_ "Message"
           [
             field "Kind" uint8;
             field "Body" (apply (type_ref "MsgBody") [ field_ref f_msg_kind ]);
           ]);
    ]

(* ── 21. Module-level declarations: define, extern_fn, extern_probe ── *)

let extern_module =
  let f_ext_length = field "Length" uint16be in
  module_
    [
      define "MAX_PAYLOAD" 1024;
      extern_fn "compute_crc"
        [ param "data" uint8; param "len" uint32be ]
        uint32be;
      extern_probe "on_packet";
      extern_probe ~init:true "init_parser";
      typedef ~entrypoint:true
        (struct_ "ExternDemo"
           [
             field "Length" uint16be;
             field "Payload" (byte_array ~size:(field_ref f_ext_length));
           ]);
    ]

(* ── 22. Type references: type_ref, qualified_ref ── *)

let type_ref_module =
  module_
    [
      typedef (struct_ "Inner" [ field "Value" uint32be ]);
      typedef ~entrypoint:true
        (struct_ "Outer" [ field "Tag" uint8; field "Body" (type_ref "Inner") ]);
    ]

let qualified_ref_example : unit Wire.typ =
  qualified_ref "OtherModule" "SomeType"
