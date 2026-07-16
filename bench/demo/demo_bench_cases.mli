(** Demo benchmark case definitions: datasets, codecs, and projection structs.
*)

type dataset = { items : bytes array; packed : bytes; n_items : int }

type id =
  | Minimal
  | All_ints
  | Large_mixed
  | Bitfield8
  | Bitfield16
  | Bitfield32
  | Bool_fields
  | Clcw_report
  | Space_packet_apid
  | Ipv4_src
  | Tcp_dst_port
  | Tcp_syn
  | Mapped_priority
  | Cases_type
  | Enum_status
  | Constrained_data

type _ c_field_decode =
  | Int : (int -> 'a) -> 'a c_field_decode
  | Int64 : (int64 -> 'a) -> 'a c_field_decode

type 'a read_case =
  | Read_case : {
      id : id;
      label : string;
      size : int;
      dataset : dataset;
      struct_ : Wire.Everparse.struct_;
      codec : 'r Wire.Codec.t;
      get : bytes -> int -> 'a;
      set : bytes -> int -> 'a -> unit;
      write_template : bytes;
      write_offset : int;
      write_value : 'a;
      equal : 'a -> 'a -> bool;
      bench_read : bool;
      of_c_field : 'a c_field_decode;
    }
      -> 'a read_case

type packed_case = C : _ read_case -> packed_case
type write_case = { label : string; run : unit -> unit; verify : unit -> unit }

val minimal_case : int read_case
(** Minimal 1-byte uint8 schema. *)

val all_ints_case : int64 read_case
(** All integer widths, projecting u64be. *)

val large_mixed_case : int64 read_case
(** Large mixed-field struct, projecting a uint64be timestamp. *)

val bitfield8_case : int read_case
(** 8-bit bitfield extraction (5-bit value). *)

val bitfield16_case : int read_case
(** 16-bit bitfield extraction (11-bit id). *)

val bitfield32_case : int read_case
(** 32-bit bitfield extraction (8-bit priority). *)

val bool_fields_case : bool read_case
(** Boolean bitfield (1-bit active flag). *)

val clcw_case : int read_case
(** CLCW report field (8-bit bitfield). *)

val packet_case : int read_case
(** CCSDS Space Packet APID (11-bit bitfield). *)

val ipv4_case : Optint.t read_case
(** IPv4 source address (uint32be). *)

val tcp_case : int read_case
(** TCP destination port (uint16be). *)

val tcp_syn_case : bool read_case
(** TCP SYN flag (1-bit boolean bitfield). *)

val mapped_case : Demo.priority read_case
(** Mapped field with custom decode function (priority enum). *)

val cases_case : Demo.ptype read_case
(** Cases variant field (telemetry/telecommand). *)

val enum_case : Demo.status read_case
(** Enum field with mapped status values. *)

val constrained_case : int read_case
(** Constrained field with a where-clause predicate. *)

val projection_cases : packed_case list
(** [projection_cases] is the subset of read cases used for EverParse C
    projection. *)

val projection_structs : Wire.Everparse.struct_ list
(** [projection_structs] is the list of EverParse struct definitions derived
    from projection cases. *)

val read_benchmark_cases : packed_case list
(** [read_benchmark_cases] is the full list of read benchmark cases covering all
    Wire types. *)

val write_benchmark_cases : write_case list
(** [write_benchmark_cases] is the list of write benchmark cases for Codec.set.
*)
