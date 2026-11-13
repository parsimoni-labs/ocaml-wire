(** Loads at an [int64_u] offset, for the code [Wire_ocaml] writes for OxCaml.

    Each load reads the bytes at [off] with no bounds check: the generated code
    checks the frame first. The paranoid profile checks the full 64-bit [off]
    before each load, so an offset that narrowing to an [int] would wrap raises
    [Invalid_argument] instead. *)

module Int64_u = Stdlib_upstream_compatible.Int64_u

val u8 : bytes -> int64_u -> int
(** [u8 buf off] is the byte at [off]. *)

val u16be : bytes -> int64_u -> int
(** [u16be buf off] is the big-endian 16-bit value at [off]. *)

val u16le : bytes -> int64_u -> int
(** [u16le buf off] is the little-endian 16-bit value at [off]. *)

val i32be : bytes -> int64_u -> int32
(** [i32be buf off] is the big-endian 32-bit value at [off]. *)

val i32le : bytes -> int64_u -> int32
(** [i32le buf off] is the little-endian 32-bit value at [off]. *)

val i64be : bytes -> int64_u -> int64
(** [i64be buf off] is the big-endian 64-bit value at [off]. *)
