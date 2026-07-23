(* Shared bitfield utilities for wire.ml and codec.ml *)

open Types

let byte_size = function U8 -> 1 | U16 _ -> 2 | U32 _ -> 4
let total_bits = function U8 -> 8 | U16 _ -> 16 | U32 _ -> 32

let equal a b =
  match (a, b) with
  | U8, U8 -> true
  | U16 e1, U16 e2 -> e1 = e2
  | U32 e1, U32 e2 -> e1 = e2
  | _ -> false

(* Fast word reads -- avoid Bytes.get_int32_be which goes through Int32
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

(* A bitfield word is a bag of bits manipulated in the native [int], never a
   uint32 field value, so these stay [int]-based rather than going through
   [UInt32]/[Optint]. On a host whose native [int] is narrower than 32 bits a
   U32 word does not fit the int: the halves-based field accessors below keep
   every intermediate under 31 bits, and the [bits] constructor caps a field's
   width at [Sys.int_size - 1] there, so no extracted value overflows. *)
let int_holds_u32 = Sys.int_size > 32

let[@inline always] set_u32_le buf off v =
  Bytes.set_uint16_le buf off (v land 0xFFFF);
  Bytes.set_uint16_le buf (off + 2) ((v lsr 16) land 0xFFFF)

let[@inline always] set_u32_be buf off v =
  Bytes.set_uint16_be buf off ((v lsr 16) land 0xFFFF);
  Bytes.set_uint16_be buf (off + 2) (v land 0xFFFF)

(* Field access over a u32 word split into 16-bit halves ([hi] is word bits
   16..31): every intermediate stays under 31 significant bits, so these are
   exact on a narrow-int host. [hi_part]/[lo_part] are the two halves of
   [v lsl shift] for a [v] that fits its field width. *)

let[@inline] halves_get hi lo shift mask =
  if shift >= 16 then (hi lsr (shift - 16)) land mask
  else (hi lsl (16 - shift)) lor (lo lsr shift) land mask

let[@inline] hi_part v shift =
  if shift >= 16 then (v lsl (shift - 16)) land 0xFFFF
  else (v lsr (16 - shift)) land 0xFFFF

let[@inline] lo_part v shift =
  if shift >= 16 then 0 else (v lsl shift) land 0xFFFF

let u32_field_le buf off shift mask =
  halves_get (u16_le buf (off + 2)) (u16_le buf off) shift mask

let u32_field_be buf off shift mask =
  halves_get (u16_be buf off) (u16_be buf (off + 2)) shift mask

(* Write a lone field as a full word (other bits zero). *)
let u32_field_word_le buf off shift v =
  Bytes.set_uint16_le buf off (lo_part v shift);
  Bytes.set_uint16_le buf (off + 2) (hi_part v shift)

let u32_field_word_be buf off shift v =
  Bytes.set_uint16_be buf off (hi_part v shift);
  Bytes.set_uint16_be buf (off + 2) (lo_part v shift)

(* OR a field into the current word. *)
let u32_field_or_le buf off shift v =
  Bytes.set_uint16_le buf off (u16_le buf off lor lo_part v shift);
  Bytes.set_uint16_le buf (off + 2) (u16_le buf (off + 2) lor hi_part v shift)

let u32_field_or_be buf off shift v =
  Bytes.set_uint16_be buf off (u16_be buf off lor hi_part v shift);
  Bytes.set_uint16_be buf (off + 2) (u16_be buf (off + 2) lor lo_part v shift)

(* Replace a field, clearing its bits first. *)
let[@inline] half_set cur mask_part v_part =
  cur land lnot mask_part lor v_part land 0xFFFF

let u32_field_set_le buf off shift mask v =
  Bytes.set_uint16_le buf off
    (half_set (u16_le buf off) (lo_part mask shift) (lo_part v shift));
  Bytes.set_uint16_le buf (off + 2)
    (half_set (u16_le buf (off + 2)) (hi_part mask shift) (hi_part v shift))

let u32_field_set_be buf off shift mask v =
  Bytes.set_uint16_be buf off
    (half_set (u16_be buf off) (hi_part mask shift) (hi_part v shift));
  Bytes.set_uint16_be buf (off + 2)
    (half_set (u16_be buf (off + 2)) (lo_part mask shift) (lo_part v shift))

let read_word base buf off =
  match base with
  | U8 -> Bytes.get_uint8 buf off
  | U16 Little -> u16_le buf off
  | U16 Big -> u16_be buf off
  | U32 Little -> u32_le buf off
  | U32 Big -> u32_be buf off

let write_word base buf off v =
  match base with
  | U8 -> Bytes.set_uint8 buf off v
  | U16 Little -> Bytes.set_uint16_le buf off v
  | U16 Big -> Bytes.set_uint16_be buf off v
  | U32 Little -> set_u32_le buf off v
  | U32 Big -> set_u32_be buf off v

(** EverParse's native bit order for a base: LE bases default to LSB-first (MSVC
    C bit-field packing), BE bases to MSB-first (network byte order). *)
let native_bit_order = function
  | U8 | U16 Little | U32 Little -> Lsb_first
  | U16 Big | U32 Big -> Msb_first

let[@inline] shift ~bit_order ~total ~bits_used ~width =
  match bit_order with
  | Lsb_first -> bits_used
  | Msb_first -> total - bits_used - width

let extract ~bit_order ~total ~bits_used ~width word =
  let s = shift ~bit_order ~total ~bits_used ~width in
  let mask = (1 lsl width) - 1 in
  (word lsr s) land mask
