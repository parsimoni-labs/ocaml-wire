external get16 : bytes -> int -> int @@ portable = "%caml_bytes_get16u"
external get32 : bytes -> int -> int32 @@ portable = "%caml_bytes_get32u"
external get64 : bytes -> int -> int64 @@ portable = "%caml_bytes_get64u"
external set16 : bytes -> int -> int -> unit @@ portable = "%caml_bytes_set16u"
external set32 : bytes -> int -> int32 -> unit @@ portable = "%caml_bytes_set32u"
external set64 : bytes -> int -> int64 -> unit @@ portable = "%caml_bytes_set64u"
external swap16 : int -> int @@ portable = "%bswap16"
external swap32 : int32 -> int32 @@ portable = "%bswap_int32"
external swap64 : int64 -> int64 @@ portable = "%bswap_int64"

module Identity = struct
  external unsafe_of_int : int -> int @@ portable = "%identity"
end
