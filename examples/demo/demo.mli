(** Synthetic benchmark schemas exercising all Wire API constructs. *)

(** {1 Minimal (1 byte)} *)

type minimal = { m_value : int }

val minimal_codec : minimal Wire.Codec.t
(** Codec for the minimal 1-byte schema. *)

val minimal_struct : Wire.struct_
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

val all_ints_struct : Wire.struct_
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

val bf8_struct : Wire.struct_
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

val bf16_struct : Wire.struct_
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

val bf32_struct : Wire.struct_
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

val bool_fields_struct : Wire.struct_
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

val large_mixed_struct : Wire.struct_
(** Struct definition for 3D codegen. *)

val large_mixed_size : int
(** Wire size in bytes (26). *)

val large_mixed_default : large_mixed
(** Default value for benchmarking. *)

val large_mixed_data : int -> bytes array
(** [large_mixed_data n] generates [n] encoded large_mixed buffers. *)
