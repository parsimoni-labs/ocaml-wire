(* Reads with no bounds check, for a caller that has already checked the span
   it reads from: [Codec.decode] reads a record's fixed fields this way after
   checking the record's extent. The [paranoid] profile checks them anyway. *)

open Prims

let checked = Paranoid.enabled

let[@inline] u8 buf off =
  if Paranoid.enabled then Bytes.get_uint8 buf off
  else Char.code (Bytes.unsafe_get buf off)

let[@inline] u16le buf off =
  if Paranoid.enabled then Bytes.get_uint16_le buf off
  else if Sys.big_endian then swap16 (get16 buf off)
  else get16 buf off

let[@inline] u16be buf off =
  if Paranoid.enabled then Bytes.get_uint16_be buf off
  else if Sys.big_endian then get16 buf off
  else swap16 (get16 buf off)

let[@inline] i32le buf off =
  if Paranoid.enabled then Bytes.get_int32_le buf off
  else if Sys.big_endian then swap32 (get32 buf off)
  else get32 buf off

let[@inline] i32be buf off =
  if Paranoid.enabled then Bytes.get_int32_be buf off
  else if Sys.big_endian then get32 buf off
  else swap32 (get32 buf off)

let[@inline] set_u8 buf off v =
  if Paranoid.enabled then Bytes.set_uint8 buf off v
  else Bytes.unsafe_set buf off (Char.unsafe_chr (v land 0xff))

let[@inline] set_u16le buf off v =
  if Paranoid.enabled then Bytes.set_uint16_le buf off v
  else if Sys.big_endian then set16 buf off (swap16 v)
  else set16 buf off v

let[@inline] set_u16be buf off v =
  if Paranoid.enabled then Bytes.set_uint16_be buf off v
  else if Sys.big_endian then set16 buf off v
  else set16 buf off (swap16 v)

let[@inline] set_i32le buf off v =
  if Paranoid.enabled then Bytes.set_int32_le buf off v
  else if Sys.big_endian then set32 buf off (swap32 v)
  else set32 buf off v

let[@inline] set_i32be buf off v =
  if Paranoid.enabled then Bytes.set_int32_be buf off v
  else if Sys.big_endian then set32 buf off v
  else set32 buf off (swap32 v)

let[@inline] i64le buf off =
  if Paranoid.enabled then Bytes.get_int64_le buf off
  else if Sys.big_endian then swap64 (get64 buf off)
  else get64 buf off

let[@inline] i64be buf off =
  if Paranoid.enabled then Bytes.get_int64_be buf off
  else if Sys.big_endian then get64 buf off
  else swap64 (get64 buf off)

let[@inline] set_i64le buf off v =
  if Paranoid.enabled then Bytes.set_int64_le buf off v
  else if Sys.big_endian then set64 buf off (swap64 v)
  else set64 buf off v

let[@inline] set_i64be buf off v =
  if Paranoid.enabled then Bytes.set_int64_be buf off v
  else if Sys.big_endian then set64 buf off v
  else set64 buf off (swap64 v)

let[@inline] blit_string s buf off =
  let len = String.length s in
  if Paranoid.enabled then Bytes.blit_string s 0 buf off len
  else Bytes.unsafe_blit_string s 0 buf off len
