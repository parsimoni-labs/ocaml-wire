external get16 : bytes -> int -> int = "%caml_bytes_get16u"
external get32 : bytes -> int -> int32 = "%caml_bytes_get32u"
external get64 : bytes -> int -> int64 = "%caml_bytes_get64u"
external set16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
external set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

module Identity = struct
  external unsafe_of_int : int -> int = "%identity"
end
