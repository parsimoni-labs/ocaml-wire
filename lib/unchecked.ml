(* Reads with no bounds check, for a caller that has already checked the span
   it reads from: [Codec.decode] reads a record's fixed fields this way after
   checking the record's extent. *)

external get16 : bytes -> int -> int = "%caml_bytes_get16u"
external get32 : bytes -> int -> int32 = "%caml_bytes_get32u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"

let[@inline] u8 buf off = Char.code (Bytes.unsafe_get buf off)

let[@inline] u16le buf off =
  if Sys.big_endian then swap16 (get16 buf off) else get16 buf off

let[@inline] u16be buf off =
  if Sys.big_endian then get16 buf off else swap16 (get16 buf off)

let[@inline] i32le buf off =
  if Sys.big_endian then swap32 (get32 buf off) else get32 buf off

let[@inline] i32be buf off =
  if Sys.big_endian then get32 buf off else swap32 (get32 buf off)
