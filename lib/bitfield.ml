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
let[@inline always] get_u16_le buf off =
  Char.code (Bytes.unsafe_get buf off)
  lor (Char.code (Bytes.unsafe_get buf (off + 1)) lsl 8)

let[@inline always] get_u16_be buf off =
  (Char.code (Bytes.unsafe_get buf off) lsl 8)
  lor Char.code (Bytes.unsafe_get buf (off + 1))

let[@inline always] get_u32_le buf off =
  Char.code (Bytes.unsafe_get buf off)
  lor (Char.code (Bytes.unsafe_get buf (off + 1)) lsl 8)
  lor (Char.code (Bytes.unsafe_get buf (off + 2)) lsl 16)
  lor (Char.code (Bytes.unsafe_get buf (off + 3)) lsl 24)

let[@inline always] get_u32_be buf off =
  (Char.code (Bytes.unsafe_get buf off) lsl 24)
  lor (Char.code (Bytes.unsafe_get buf (off + 1)) lsl 16)
  lor (Char.code (Bytes.unsafe_get buf (off + 2)) lsl 8)
  lor Char.code (Bytes.unsafe_get buf (off + 3))

let read_word base buf off =
  match base with
  | BF_U8 -> Bytes.get_uint8 buf off
  | BF_U16 Little -> get_u16_le buf off
  | BF_U16 Big -> get_u16_be buf off
  | BF_U32 Little -> get_u32_le buf off
  | BF_U32 Big -> get_u32_be buf off

let write_word base buf off v =
  match base with
  | BF_U8 -> Bytes.set_uint8 buf off v
  | BF_U16 Little -> Bytes.set_uint16_le buf off v
  | BF_U16 Big -> Bytes.set_uint16_be buf off v
  | BF_U32 Little -> UInt32.set_le buf off v
  | BF_U32 Big -> UInt32.set_be buf off v

(** Extract [width] bits from MSB position [bits_used] in a [total]-bit word. *)
let extract ~total ~bits_used ~width word =
  let shift = total - bits_used - width in
  let mask = (1 lsl width) - 1 in
  (word lsr shift) land mask
