(** Loads at an [int64_u] offset, for the code [Wire_ocaml] writes for OxCaml.

    Each load reads the bytes at [off] with no bounds check: the generated code
    checks the frame first. The paranoid profile checks the full 64-bit [off]
    before each load, so an offset that narrowing to an [int] would wrap raises
    [Invalid_argument] instead. *)

module Int64_u = Stdlib_upstream_compatible.Int64_u

type cell = { mutable cell : int64_u }
(** A mutable [int64_u], which no [ref] holds. The compiler unboxes one that is
    local and that only reads and assignments of [cell] use. *)

val[@zero_alloc] u8 : bytes -> int64_u -> int
(** [u8 buf off] is the byte at [off]. *)

val[@zero_alloc] u16be : bytes -> int64_u -> int
(** [u16be buf off] is the big-endian 16-bit value at [off]. *)

val[@zero_alloc] u16le : bytes -> int64_u -> int
(** [u16le buf off] is the little-endian 16-bit value at [off]. *)

val i32be : bytes -> int64_u -> int32
(** [i32be buf off] is the big-endian 32-bit value at [off]. *)

val i32le : bytes -> int64_u -> int32
(** [i32le buf off] is the little-endian 32-bit value at [off]. *)

val i64be : bytes -> int64_u -> int64
(** [i64be buf off] is the big-endian 64-bit value at [off]. *)

(** Reads and writes at an [int] offset of a buffer the caller may hold
    [local], for the code [Wire_ocaml] writes for OxCaml: the same names as
    {!Unchecked} and as [Bytes]' checked reads, so a generated module reads a
    stack-allocated buffer as it reads one on the heap. The unchecked ones
    check their bounds in the paranoid profile, as {!Unchecked}'s do. *)
module Local : sig
  val[@zero_alloc] u8 : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] u16be : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] u16le : bytes @ local -> int -> int @@ portable
  val i32be : bytes @ local -> int -> int32 @@ portable
  val i32le : bytes @ local -> int -> int32 @@ portable
  val i64be : bytes @ local -> int -> int64 @@ portable
  val i64le : bytes @ local -> int -> int64 @@ portable
  val[@zero_alloc] set_u8 : bytes @ local -> int -> int -> unit @@ portable
  val[@zero_alloc] set_u16be : bytes @ local -> int -> int -> unit @@ portable
  val[@zero_alloc] set_u16le : bytes @ local -> int -> int -> unit @@ portable
  val[@zero_alloc] set_i32be : bytes @ local -> int -> int32 -> unit @@ portable
  val[@zero_alloc] set_i32le : bytes @ local -> int -> int32 -> unit @@ portable
  val[@zero_alloc] set_i64be : bytes @ local -> int -> int64 -> unit @@ portable
  val[@zero_alloc] set_i64le : bytes @ local -> int -> int64 -> unit @@ portable

  val[@zero_alloc] get_uint8 : bytes @ local -> int -> int @@ portable
  (** [get_uint8] and the [get_] reads below check their bounds, as [Bytes]'
      do. *)

  val[@zero_alloc] get_uint16_be : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] get_uint16_le : bytes @ local -> int -> int @@ portable
  val get_int32_be : bytes @ local -> int -> int32 @@ portable
  val get_int32_le : bytes @ local -> int -> int32 @@ portable

  val sub_string : bytes @ local -> int -> int -> string @@ portable
  (** [sub_string b off n] is a copy of the [n] bytes at [off]. *)

  val[@zero_alloc] unsafe_blit_string : string -> int -> bytes @ local -> int -> int -> unit @@ portable
  (** [unsafe_blit_string s soff b off n] copies [n] bytes of [s] at [soff]
      to [b] at [off], with no bounds check. *)

  val[@zero_alloc] blit_string : string -> bytes @ local -> int -> unit @@ portable
  (** [blit_string s b off] copies [s] to [b] at [off]. *)

  (** The same on a bigarray of bytes. *)
  module Bigstring : sig
    type t =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    val[@zero_alloc] length : t @ local -> int @@ portable
    val[@zero_alloc] u8 : t @ local -> int -> int @@ portable
    val[@zero_alloc] u16be : t @ local -> int -> int @@ portable
    val[@zero_alloc] u16le : t @ local -> int -> int @@ portable
    val i32be : t @ local -> int -> int32 @@ portable
    val i32le : t @ local -> int -> int32 @@ portable
    val i64be : t @ local -> int -> int64 @@ portable
    val i64le : t @ local -> int -> int64 @@ portable
    val[@zero_alloc] set_u8 : t @ local -> int -> int -> unit @@ portable
    val[@zero_alloc] set_u16be : t @ local -> int -> int -> unit @@ portable
    val[@zero_alloc] set_u16le : t @ local -> int -> int -> unit @@ portable
    val[@zero_alloc] set_i32be : t @ local -> int -> int32 -> unit @@ portable
    val[@zero_alloc] set_i32le : t @ local -> int -> int32 -> unit @@ portable
    val[@zero_alloc] set_i64be : t @ local -> int -> int64 -> unit @@ portable
    val[@zero_alloc] set_i64le : t @ local -> int -> int64 -> unit @@ portable
    val[@zero_alloc] get_uint8 : t @ local -> int -> int @@ portable
    val[@zero_alloc] get_uint16_be : t @ local -> int -> int @@ portable
    val[@zero_alloc] get_uint16_le : t @ local -> int -> int @@ portable
    val get_int32_be : t @ local -> int -> int32 @@ portable
    val get_int32_le : t @ local -> int -> int32 @@ portable
    val sub_string : t @ local -> int -> int -> string @@ portable

    val[@zero_alloc] unsafe_blit_string :
      string -> int -> t @ local -> int -> int -> unit @@ portable
  end
end

(** The modules of the runtime that the code [Wire_ocaml] writes for OxCaml
    calls, each [portable] in its interface. The code binds [Wire_runtime] to
    this module. *)
module Portable : sig
  module Vec = Vec
  module Errors = Errors
  module Arith = Arith
  module UInt8 = UInt8
  module UInt16 = UInt16
  module SInt8 = SInt8
  module SInt16 = SInt16
  module UInt32 = UInt32
  module SInt32 = SInt32
  module Unchecked = Unchecked

  module Target : sig
    module Local = Local
  end

  module Buf = Buf
  module Staged = Staged
end
