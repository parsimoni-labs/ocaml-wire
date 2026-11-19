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

module Bigstring = struct
  open Prims.Bigstring

  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let[@inline] length buf = Prims.Bigstring.length buf
  let[@inline never] out_of_bounds () = invalid_arg "index out of bounds"

  let[@inline] check buf off n =
    if off < 0 || off > length buf - n then out_of_bounds ()

  let[@inline] u8 buf off =
    if Paranoid.enabled then check buf off 1;
    Char.code (get buf off)

  let[@inline] u16le buf off =
    if Paranoid.enabled then check buf off 2;
    if Sys.big_endian then swap16 (get16 buf off) else get16 buf off

  let[@inline] u16be buf off =
    if Paranoid.enabled then check buf off 2;
    if Sys.big_endian then get16 buf off else swap16 (get16 buf off)

  let[@inline] i32le buf off =
    if Paranoid.enabled then check buf off 4;
    if Sys.big_endian then swap32 (get32 buf off) else get32 buf off

  let[@inline] i32be buf off =
    if Paranoid.enabled then check buf off 4;
    if Sys.big_endian then get32 buf off else swap32 (get32 buf off)

  let[@inline] i64be buf off =
    if Paranoid.enabled then check buf off 8;
    if Sys.big_endian then get64 buf off else swap64 (get64 buf off)

  let[@inline] set_u8 buf off v =
    if Paranoid.enabled then check buf off 1;
    set buf off (Char.unsafe_chr (v land 0xff))

  let[@inline] set_u16le buf off v =
    if Paranoid.enabled then check buf off 2;
    if Sys.big_endian then set16 buf off (swap16 v) else set16 buf off v

  let[@inline] set_u16be buf off v =
    if Paranoid.enabled then check buf off 2;
    if Sys.big_endian then set16 buf off v else set16 buf off (swap16 v)

  let[@inline] set_i32le buf off v =
    if Paranoid.enabled then check buf off 4;
    if Sys.big_endian then set32 buf off (swap32 v) else set32 buf off v

  let[@inline] set_i32be buf off v =
    if Paranoid.enabled then check buf off 4;
    if Sys.big_endian then set32 buf off v else set32 buf off (swap32 v)

  let[@inline] set_i64le buf off v =
    if Paranoid.enabled then check buf off 8;
    if Sys.big_endian then set64 buf off (swap64 v) else set64 buf off v

  let[@inline] set_i64be buf off v =
    if Paranoid.enabled then check buf off 8;
    if Sys.big_endian then set64 buf off v else set64 buf off (swap64 v)

  (* The checked reads are the compiler's own: its bounds check jumps to a
     handler that raises [Invalid_argument "index out of bounds"], as [Bytes]'
     do, and keeps nothing live across it. *)

  let[@inline] get_uint8 buf off = Char.code (safe_get buf off)

  let[@inline] get_uint16_le buf off =
    if Sys.big_endian then swap16 (safe_get16 buf off) else safe_get16 buf off

  let[@inline] get_uint16_be buf off =
    if Sys.big_endian then safe_get16 buf off else swap16 (safe_get16 buf off)

  let[@inline] get_int32_le buf off =
    if Sys.big_endian then swap32 (safe_get32 buf off) else safe_get32 buf off

  let[@inline] get_int32_be buf off =
    if Sys.big_endian then safe_get32 buf off else swap32 (safe_get32 buf off)

  let sub_string buf off n =
    if off < 0 || n < 0 || off > length buf - n then
      invalid_arg "String.sub / Bytes.sub";
    String.init n (fun i -> get buf (off + i))

  let unsafe_blit_string s soff buf off n =
    for i = 0 to n - 1 do
      set buf (off + i) (String.unsafe_get s (soff + i))
    done

  let to_bytes buf = Bytes.init (length buf) (fun i -> get buf i)
end
