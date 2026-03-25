(* Shared bitfield utilities for wire.ml and codec.ml *)

open Types

let byte_size = function BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4
let total_bits = function BF_U8 -> 8 | BF_U16 _ -> 16 | BF_U32 _ -> 32

let equal a b =
  match (a, b) with
  | BF_U8, BF_U8 -> true
  | BF_U16 e1, BF_U16 e2 -> e1 = e2
  | BF_U32 e1, BF_U32 e2 -> e1 = e2
  | _ -> false

(* Fast word reads — avoid Bytes.get_int32_be which goes through Int32
   boxing/unboxing and byte-by-byte assembly on ARM64. *)
let[@inline always] u16_le buf off =
  Char.code (Bytes.unsafe_get buf off)
  lor (Char.code (Bytes.unsafe_get buf (off + 1)) lsl 8)

let[@inline always] u16_be buf off =
  (Char.code (Bytes.unsafe_get buf off) lsl 8)
  lor Char.code (Bytes.unsafe_get buf (off + 1))

let[@inline always] u32_le buf off =
  Char.code (Bytes.unsafe_get buf off)
  lor (Char.code (Bytes.unsafe_get buf (off + 1)) lsl 8)
  lor (Char.code (Bytes.unsafe_get buf (off + 2)) lsl 16)
  lor (Char.code (Bytes.unsafe_get buf (off + 3)) lsl 24)

let[@inline always] u32_be buf off =
  (Char.code (Bytes.unsafe_get buf off) lsl 24)
  lor (Char.code (Bytes.unsafe_get buf (off + 1)) lsl 16)
  lor (Char.code (Bytes.unsafe_get buf (off + 2)) lsl 8)
  lor Char.code (Bytes.unsafe_get buf (off + 3))

let read_word base buf off =
  match base with
  | BF_U8 -> Bytes.get_uint8 buf off
  | BF_U16 Little -> u16_le buf off
  | BF_U16 Big -> u16_be buf off
  | BF_U32 Little -> u32_le buf off
  | BF_U32 Big -> u32_be buf off

let write_word base buf off v =
  match base with
  | BF_U8 -> Bytes.set_uint8 buf off v
  | BF_U16 Little -> Bytes.set_uint16_le buf off v
  | BF_U16 Big -> Bytes.set_uint16_be buf off v
  | BF_U32 Little -> UInt32.set_le buf off v
  | BF_U32 Big -> UInt32.set_be buf off v

(** Is this base type LSB-first (matching EverParse convention)? UINT8, UINT16
    (little), UINT32 (little) are LSBFirst. UINT16BE, UINT32BE are MSBFirst. *)
let is_lsb_first = function
  | BF_U8 | BF_U16 Little | BF_U32 Little -> true
  | BF_U16 Big | BF_U32 Big -> false

(** Extract [width] bits at position [bits_used] in a [total]-bit word. Bit
    ordering follows EverParse 3D conventions:
    - LSBFirst (UINT8, UINT16, UINT32): first declared field at bit 0
    - MSBFirst (UINT16BE, UINT32BE): first declared field at MSB *)
let extract ~base ~total ~bits_used ~width word =
  let shift =
    if is_lsb_first base then bits_used else total - bits_used - width
  in
  let mask = (1 lsl width) - 1 in
  (word lsr shift) land mask
