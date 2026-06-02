(** Codecs shared by the differential generator and fuzzer.

    Each is a small record of fixed-width scalars or bitfields: shapes that
    project to an EverParse default-plug validator, so the same definition feeds
    both the OCaml {!Wire.Codec} decode path and the generated C parser the
    fuzzer compares it against. No field carries a constraint, so for any input
    of sufficient length both sides must accept and decode identical values. *)

type u8 = { u8 : int }

val c_u8 : u8 Wire.Codec.t
(** A single unsigned 8-bit field. *)

type u16 = { u16 : int }

val c_u16 : u16 Wire.Codec.t
(** A single unsigned 16-bit big-endian field. *)

type u32 = { u32 : int }

val c_u32 : u32 Wire.Codec.t
(** A single unsigned 32-bit big-endian field. *)

type u64 = { u64 : int64 }

val c_u64 : u64 Wire.Codec.t
(** A single unsigned 64-bit big-endian field. *)

type bits2 = { hi : int; lo : int }

val c_bits : bits2 Wire.Codec.t
(** Two 4-bit bitfields packed into one byte. *)

type triple = { a : int; b : int; c : int }

val c_triple : triple Wire.Codec.t
(** A multi-field record (u8, u16be, u32be) exercising offsets and endianness.
*)

val schemas : Wire.Everparse.t list
(** The codecs projected to 3D schemas, in the order the generator emits them.
*)
