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

module Bigstring = struct
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external length : t -> int = "%caml_ba_dim_1"
  external get : t -> int -> char = "%caml_ba_unsafe_ref_1"
  external set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
  external get16 : t -> int -> int = "%caml_bigstring_get16u"
  external get32 : t -> int -> int32 = "%caml_bigstring_get32u"
  external get64 : t -> int -> int64 = "%caml_bigstring_get64u"
  external set16 : t -> int -> int -> unit = "%caml_bigstring_set16u"
  external set32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"

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

  (* The checked reads are the compiler's own: its bounds check jumps to a
     handler that raises [Invalid_argument "index out of bounds"], as [Bytes]'
     do, and keeps nothing live across it. *)
  external safe_get : t -> int -> char = "%caml_ba_ref_1"
  external safe_get16 : t -> int -> int = "%caml_bigstring_get16"
  external safe_get32 : t -> int -> int32 = "%caml_bigstring_get32"

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
