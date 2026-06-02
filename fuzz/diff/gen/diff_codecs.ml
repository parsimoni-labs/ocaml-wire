(** Codecs shared by the differential generator and fuzzer.

    Each is a small record of fixed-width scalars or bitfields: shapes that
    project to an EverParse default-plug validator, so the same definition feeds
    both the OCaml {!Wire.Codec} decode path and the generated C parser the
    fuzzer compares it against. No field carries a constraint, so for any input
    of sufficient length both sides must accept and decode identical values; a
    divergence is a real projection bug (endianness, width, sign, or bit
    layout). *)

open Wire

type u8 = { u8 : int }

let c_u8 =
  Codec.(v "DiffU8" (fun u8 -> { u8 }) [ (Field.v "v" uint8 $ fun r -> r.u8) ])

type u16 = { u16 : int }

let c_u16 =
  Codec.(
    v "DiffU16" (fun u16 -> { u16 }) [ (Field.v "v" uint16be $ fun r -> r.u16) ])

type u32 = { u32 : int }

let c_u32 =
  Codec.(
    v "DiffU32" (fun u32 -> { u32 }) [ (Field.v "v" uint32be $ fun r -> r.u32) ])

type u64 = { u64 : int64 }

let c_u64 =
  Codec.(
    v "DiffU64" (fun u64 -> { u64 }) [ (Field.v "v" uint64be $ fun r -> r.u64) ])

type bits2 = { hi : int; lo : int }

let c_bits =
  Codec.(
    v "DiffBits"
      (fun hi lo -> { hi; lo })
      [
        (Field.v "hi" (bits ~width:4 U8) $ fun r -> r.hi);
        (Field.v "lo" (bits ~width:4 U8) $ fun r -> r.lo);
      ])

type triple = { a : int; b : int; c : int }

let c_triple =
  Codec.(
    v "DiffTriple"
      (fun a b c -> { a; b; c })
      [
        (Field.v "a" uint8 $ fun r -> r.a);
        (Field.v "b" uint16be $ fun r -> r.b);
        (Field.v "c" uint32be $ fun r -> r.c);
      ])

let schemas =
  [
    Everparse.schema c_u8;
    Everparse.schema c_u16;
    Everparse.schema c_u32;
    Everparse.schema c_u64;
    Everparse.schema c_bits;
    Everparse.schema c_triple;
  ]
