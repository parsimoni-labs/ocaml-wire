(* Reads with no bounds check, for a caller that has already checked the span
   it reads from: [Codec.decode] reads a record's fixed fields this way after
   checking the record's extent. The [paranoid] profile checks them anyway. *)

let checked = Paranoid.enabled

external get16 : bytes -> int -> int = "%caml_bytes_get16u"
external get32 : bytes -> int -> int32 = "%caml_bytes_get32u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"

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

external set16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
external set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"

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

external get64 : bytes -> int -> int64 = "%caml_bytes_get64u"
external swap64 : int64 -> int64 = "%bswap_int64"

let[@inline] i64le buf off =
  if Paranoid.enabled then Bytes.get_int64_le buf off
  else if Sys.big_endian then swap64 (get64 buf off)
  else get64 buf off

let[@inline] i64be buf off =
  if Paranoid.enabled then Bytes.get_int64_be buf off
  else if Sys.big_endian then get64 buf off
  else swap64 (get64 buf off)

let[@inline] blit_string s buf off =
  let len = String.length s in
  if Paranoid.enabled then Bytes.blit_string s 0 buf off len
  else Bytes.unsafe_blit_string s 0 buf off len
