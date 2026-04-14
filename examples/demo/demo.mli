(** Synthetic benchmark schemas exercising all Wire API constructs. *)

(** {1 Minimal (1 byte)} *)

type minimal = { m_value : int }

val minimal_codec : minimal Wire.Codec.t
(** Codec for the minimal 1-byte schema. *)

val f_minimal_value : int Wire.Field.t
(** Zero-copy field accessor for the Value field. *)

val bf_minimal_value : (int, minimal) Wire.Codec.field
(** Bound field for Codec.get/set on Value. *)

val minimal_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val minimal_size : int
(** Wire size in bytes (1). *)

val minimal_default : minimal
(** Default value for benchmarking. *)

val minimal_data : int -> bytes array
(** [minimal_data n] generates [n] encoded minimal buffers. *)

(** {1 AllInts (21 bytes)} *)

type all_ints = {
  ai_u8 : int;
  ai_u16 : int;
  ai_u16be : int;
  ai_u32 : int;
  ai_u32be : int;
  ai_u64be : int64;
}

val all_ints_codec : all_ints Wire.Codec.t
(** Codec covering all integer widths and endiannesses. *)

val f_ints_u64be : int64 Wire.Field.t
(** Zero-copy field accessor for the U64BE field (boxed int64). *)

val bf_ints_u64be : (int64, all_ints) Wire.Codec.field
(** Bound field for Codec.get/set on U64BE. *)

val all_ints_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val all_ints_size : int
(** Wire size in bytes (21). *)

val all_ints_default : all_ints
(** Default value for benchmarking. *)

val all_ints_data : int -> bytes array
(** [all_ints_data n] generates [n] encoded all_ints buffers. *)

(** {1 Bitfield8 (1 byte)} *)

type bf8 = { bf8_tag : int; bf8_value : int }

val bf8_codec : bf8 Wire.Codec.t
(** Codec with two bitfields packed into a single uint8. *)

val f_bf8_value : int Wire.Field.t
(** Zero-copy field accessor for the Value bitfield (5 bits in bf_uint8). *)

val bf_bf8_value : (int, bf8) Wire.Codec.field
(** Bound field for Codec.get/set on Value. *)

val bf8_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val bf8_size : int
(** Wire size in bytes (1). *)

val bf8_default : bf8
(** Default value for benchmarking. *)

val bf8_data : int -> bytes array
(** [bf8_data n] generates [n] encoded bf8 buffers. *)

(** {1 Bitfield16 (2 bytes)} *)

type bf16 = { bf16_flag : int; bf16_type : int; bf16_id : int }

val bf16_codec : bf16 Wire.Codec.t
(** Codec with three bitfields packed into a uint16be. *)

val f_bf16_id : int Wire.Field.t
(** Zero-copy field accessor for the Id bitfield (11 bits in bf_uint16be). *)

val bf_bf16_id : (int, bf16) Wire.Codec.field
(** Bound field for Codec.get/set on Id. *)

val bf16_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val bf16_size : int
(** Wire size in bytes (2). *)

val bf16_default : bf16
(** Default value for benchmarking. *)

val bf16_data : int -> bytes array
(** [bf16_data n] generates [n] encoded bf16 buffers. *)

(** {1 Bitfield32 (4 bytes)} *)

type bf32 = {
  bf32_flags : int;
  bf32_chan : int;
  bf32_seq : int;
  bf32_pri : int;
}

val bf32_codec : bf32 Wire.Codec.t
(** Codec with four bitfields packed into a uint32be. *)

val f_bf32_pri : int Wire.Field.t
(** Zero-copy field accessor for the Priority bitfield (8 bits in bf_uint32be).
*)

val bf_bf32_pri : (int, bf32) Wire.Codec.field
(** Bound field for Codec.get/set on Priority. *)

val bf32_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val bf32_size : int
(** Wire size in bytes (4). *)

val bf32_default : bf32
(** Default value for benchmarking. *)

val bf32_data : int -> bytes array
(** [bf32_data n] generates [n] encoded bf32 buffers. *)

(** {1 BoolFields (2 bytes)} *)

type bool_fields = {
  bl_active : bool;
  bl_valid : bool;
  bl_mode : int;
  bl_code : int;
}

val bool_fields_codec : bool_fields Wire.Codec.t
(** Codec with boolean and integer bitfields in a uint16be. *)

val f_bool_active : bool Wire.Field.t
(** Zero-copy field accessor for the Active field (bool from bf1 in bf_uint8).
*)

val bf_bool_active : (bool, bool_fields) Wire.Codec.field
(** Bound field for Codec.get/set on Active. *)

val bool_fields_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val bool_fields_size : int
(** Wire size in bytes (2). *)

val bool_fields_default : bool_fields
(** Default value for benchmarking. *)

val bool_fields_data : int -> bytes array
(** [bool_fields_data n] generates [n] encoded bool_fields buffers. *)

(** {1 LargeMixed (26 bytes)} *)

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

val large_mixed_codec : large_mixed Wire.Codec.t
(** Codec mixing uint8/16/32/64 and bitfield groups. *)

val f_mixed_timestamp : int64 Wire.Field.t
(** Zero-copy field accessor for the Timestamp field (uint64be, last of 10
    fields). *)

val bf_mixed_timestamp : (int64, large_mixed) Wire.Codec.field
(** Bound field for Codec.get/set on Timestamp. *)

val large_mixed_struct : Wire.Everparse.Raw.struct_
(** Struct definition for 3D codegen. *)

val large_mixed_size : int
(** Wire size in bytes (26). *)

val large_mixed_default : large_mixed
(** Default value for benchmarking. *)

val large_mixed_data : int -> bytes array
(** [large_mixed_data n] generates [n] encoded large_mixed buffers. *)

(** {1 Mapped (2 bytes)} *)

type priority = Low | Medium | High | Critical
type mapped = { mp_priority : priority; mp_value : int }

val mapped_codec : mapped Wire.Codec.t
(** Mapped-priority codec. *)

val mapped_struct : Wire.Everparse.Raw.struct_
(** 3D struct for mapped. *)

val f_mp_priority : priority Wire.Field.t
(** Priority field. *)

val bf_mp_priority : (priority, mapped) Wire.Codec.field
(** Bound field for Codec.get/set on Priority. *)

val f_mp_value : int Wire.Field.t
(** Value field. *)

val mapped_size : int
(** Wire size. *)

val mapped_default : mapped
(** Default value. *)

val mapped_data : int -> bytes
(** Test data. *)

(** {1 CasesDemo (1 byte)} *)

type ptype = Telemetry | Telecommand
type cases_demo = { cd_type : ptype; cd_id : int }

val cases_demo_codec : cases_demo Wire.Codec.t
(** Cases-demo codec. *)

val cases_demo_struct : Wire.Everparse.Raw.struct_
(** 3D struct for cases-demo. *)

val f_cd_type : ptype Wire.Field.t
(** Type field. *)

val bf_cd_type : (ptype, cases_demo) Wire.Codec.field
(** Bound field for Codec.get/set on PacketType. *)

val f_cd_id : int Wire.Field.t
(** ID field. *)

val cases_demo_size : int
(** Wire size. *)

val cases_demo_default : cases_demo
(** Default value. *)

val cases_demo_data : int -> bytes
(** Test data. *)

(** {1 EnumDemo (2 bytes)} *)

type status = [ `Ok | `Warn | `Err | `Crit ]
type enum_demo = { en_status : status; en_code : int }

val enum_demo_codec : enum_demo Wire.Codec.t
(** Enum-demo codec. *)

val enum_demo_struct : Wire.Everparse.Raw.struct_
(** 3D struct for enum-demo. *)

val f_en_status : status Wire.Field.t
(** Status field. *)

val bf_en_status : (status, enum_demo) Wire.Codec.field
(** Bound field for Codec.get/set on StatusCode. *)

val f_en_code : int Wire.Field.t
(** Code field. *)

val enum_demo_size : int
(** Wire size. *)

val enum_demo_default : enum_demo
(** Default value. *)

val enum_demo_data : int -> bytes
(** Test data. *)

(** {1 Constrained (2 bytes)} *)

type constrained = { co_version : int; co_data : int }

val constrained_codec : constrained Wire.Codec.t
(** Constrained codec. *)

val constrained_struct : Wire.Everparse.Raw.struct_
(** 3D struct for constrained. *)

val f_co_version : int Wire.Field.t
(** Version field. *)

val f_co_data : int Wire.Field.t
(** Data field. *)

val bf_co_data : (int, constrained) Wire.Codec.field
(** Bound field for Codec.get/set on Data. *)

val constrained_size : int
(** Wire size. *)

val constrained_default : constrained
(** Default value. *)

val constrained_data : int -> bytes
(** Test data. *)

type lowercase_record

val lowercase_codec : lowercase_record Wire.Codec.t
(** Codec with a lowercase snake_case name. Exercises filename capitalization in
    the 3D pipeline (EverParse requires filenames to start uppercase). *)

(** {1 3D Feature Coverage}

    The following are struct/module definitions exercising Wire DSL features
    that target EverParse 3D output. They don't have Codec views. *)

val array_struct : Wire.Everparse.Raw.struct_
(** Array field demo. *)

val byte_array_struct : Wire.Everparse.Raw.struct_
(** Byte-array field demo. *)

val all_bytes_struct : Wire.Everparse.Raw.struct_
(** All-bytes field demo. *)

val all_zeros_struct : Wire.Everparse.Raw.struct_
(** All-zeros field demo. *)

val single_elem_struct : Wire.Everparse.Raw.struct_
(** Single-element array demo. *)

val single_elem_at_most_struct : Wire.Everparse.Raw.struct_
(** Single-element at-most demo. *)

val anon_field_struct : Wire.Everparse.Raw.struct_
(** Anonymous field demo. *)

val action_struct : Wire.Everparse.Raw.struct_
(** Action field demo. *)

val action_full_struct : Wire.Everparse.Raw.struct_
(** Full action demo with mutable param. *)

val param_demo_struct : Wire.Everparse.Raw.struct_
(** Parameterised struct demo. *)

val param_payload_struct : Wire.Everparse.Raw.struct_
(** Parameterised byte_array demo: payload sized by input parameter. *)

val casetype_module : Wire.Everparse.Raw.module_
(** Casetype module demo. *)

val extern_module : Wire.Everparse.Raw.module_
(** Extern function module demo. *)

val type_ref_module : Wire.Everparse.Raw.module_
(** Type-reference module demo. *)

val qualified_ref_example : unit Wire.typ
(** Qualified-reference demo. *)
