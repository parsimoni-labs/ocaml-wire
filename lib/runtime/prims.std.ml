external get16 : bytes -> int -> int = "%caml_bytes_get16u"
external get32 : bytes -> int -> int32 = "%caml_bytes_get32u"
external get64 : bytes -> int -> int64 = "%caml_bytes_get64u"
external set16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
external set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

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
  external set64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"
  external safe_get : t -> int -> char = "%caml_ba_ref_1"
  external safe_get16 : t -> int -> int = "%caml_bigstring_get16"
  external safe_get32 : t -> int -> int32 = "%caml_bigstring_get32"
end
