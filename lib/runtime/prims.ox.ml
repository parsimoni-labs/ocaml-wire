external get16 : bytes -> int -> int @@ portable = "%caml_bytes_get16u"
external get32 : bytes -> int -> int32 @@ portable = "%caml_bytes_get32u"
external get64 : bytes -> int -> int64 @@ portable = "%caml_bytes_get64u"
external set16 : bytes -> int -> int -> unit @@ portable = "%caml_bytes_set16u"
external set32 : bytes -> int -> int32 -> unit @@ portable = "%caml_bytes_set32u"
external set64 : bytes -> int -> int64 -> unit @@ portable = "%caml_bytes_set64u"
external swap16 : int -> int @@ portable = "%bswap16"
external swap32 : int32 -> int32 @@ portable = "%bswap_int32"
external swap64 : int64 -> int64 @@ portable = "%bswap_int64"
module Bigstring = struct
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  external length : t -> int @@ portable = "%caml_ba_dim_1"
  external get : t -> int -> char @@ portable = "%caml_ba_unsafe_ref_1"
  external set : t -> int -> char -> unit @@ portable = "%caml_ba_unsafe_set_1"
  external get16 : t -> int -> int @@ portable = "%caml_bigstring_get16u"
  external get32 : t -> int -> int32 @@ portable = "%caml_bigstring_get32u"
  external get64 : t -> int -> int64 @@ portable = "%caml_bigstring_get64u"
  external set16 : t -> int -> int -> unit @@ portable = "%caml_bigstring_set16u"
  external set32 : t -> int -> int32 -> unit @@ portable = "%caml_bigstring_set32u"
  external set64 : t -> int -> int64 -> unit @@ portable = "%caml_bigstring_set64u"
  external safe_get : t -> int -> char @@ portable = "%caml_ba_ref_1"
  external safe_get16 : t -> int -> int @@ portable = "%caml_bigstring_get16"
  external safe_get32 : t -> int -> int32 @@ portable = "%caml_bigstring_get32"
end

module Identity = struct
  external unsafe_of_int : int -> int @@ portable = "%identity"
end
