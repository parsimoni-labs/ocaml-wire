module Int64_u = Stdlib_upstream_compatible.Int64_u

external checked16 : bytes -> int64_u -> int
  = "%caml_bytes_get16_indexed_by_int64#"

external unchecked16 : bytes -> int64_u -> int
  = "%caml_bytes_get16u_indexed_by_int64#"

external checked32 : bytes -> int64_u -> int32
  = "%caml_bytes_get32_indexed_by_int64#"

external unchecked32 : bytes -> int64_u -> int32
  = "%caml_bytes_get32u_indexed_by_int64#"

external checked64 : bytes -> int64_u -> int64
  = "%caml_bytes_get64_indexed_by_int64#"

external unchecked64 : bytes -> int64_u -> int64
  = "%caml_bytes_get64u_indexed_by_int64#"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

(* No indexed primitive loads one byte: the paranoid profile checks the full
   offset before narrowing it to an [int]. *)
let[@inline always] u8 b off =
  if
    Paranoid.enabled
    && (Int64_u.compare off (Int64_u.of_int 0) < 0
       || Int64_u.compare off (Int64_u.of_int (Bytes.length b)) >= 0)
  then invalid_arg "index out of bounds";
  Char.code (Bytes.unsafe_get b (Int64_u.to_int off))

let[@inline always] u16 b o =
  if Paranoid.enabled then checked16 b o else unchecked16 b o

let[@inline always] i32 b o =
  if Paranoid.enabled then checked32 b o else unchecked32 b o

let[@inline always] i64 b o =
  if Paranoid.enabled then checked64 b o else unchecked64 b o

let[@inline always] u16be b o =
  if Sys.big_endian then u16 b o else swap16 (u16 b o)

let[@inline always] u16le b o =
  if Sys.big_endian then swap16 (u16 b o) else u16 b o

let[@inline always] i32be b o =
  if Sys.big_endian then i32 b o else swap32 (i32 b o)

let[@inline always] i32le b o =
  if Sys.big_endian then swap32 (i32 b o) else i32 b o

let[@inline always] i64be b o =
  if Sys.big_endian then i64 b o else swap64 (i64 b o)
