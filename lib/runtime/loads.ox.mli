(** What the code [Wire_ocaml] writes for OxCaml reads a buffer with: {!Local}'s
    loads and stores, at an [int] offset of a buffer the caller may hold
    [local], and {!offset}, which narrows to such an offset a position the code
    holds as an [int64_u]. *)

module Int64_u = Stdlib_upstream_compatible.Int64_u
module Int32_u = Stdlib_upstream_compatible.Int32_u

type cell = { mutable cell : int64_u }
(** A mutable [int64_u], which no [ref] holds. The compiler unboxes one that is
    local and that only reads and assignments of [cell] use. *)

val[@zero_alloc] offset : int64_u -> int @@ portable
(** [offset off] is [off] as an [int]. The paranoid profile raises
    [Invalid_argument] for an [off] that does not fit an [int], which narrowing
    would wrap to another offset. *)

(** Reads and writes at an [int] offset of a buffer the caller may hold
    [local], for the code [Wire_ocaml] writes for OxCaml: the same names as
    {!Unchecked} and as [Bytes]' checked reads, so a generated module reads a
    stack-allocated buffer as it reads one on the heap. The unchecked ones
    check their bounds in the paranoid profile, as {!Unchecked}'s do. *)
module Local : sig
  val[@zero_alloc] u8 : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] u16be : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] u16le : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] i32be : bytes @ local -> int -> int32_u @@ portable
  val[@zero_alloc] i32le : bytes @ local -> int -> int32_u @@ portable
  val[@zero_alloc] i64be : bytes @ local -> int -> int64_u @@ portable
  val[@zero_alloc] i64le : bytes @ local -> int -> int64_u @@ portable
  val[@zero_alloc] u16be_at : bytes @ local -> int64_u -> int @@ portable
  (** [u16be_at] and the [_at] reads below read at an [int64_u] offset: the
      compiler adds a constant part of it in the load itself. *)

  val[@zero_alloc] u16le_at : bytes @ local -> int64_u -> int @@ portable
  val[@zero_alloc] i32be_at : bytes @ local -> int64_u -> int32_u @@ portable
  val[@zero_alloc] i32le_at : bytes @ local -> int64_u -> int32_u @@ portable
  val[@zero_alloc] i64be_at : bytes @ local -> int64_u -> int64_u @@ portable
  val[@zero_alloc] i64le_at : bytes @ local -> int64_u -> int64_u @@ portable
  val[@zero_alloc] set_u8 : bytes @ local -> int -> int -> unit @@ portable
  val[@zero_alloc] set_u16be : bytes @ local -> int -> int -> unit @@ portable
  val[@zero_alloc] set_u16le : bytes @ local -> int -> int -> unit @@ portable
  val[@zero_alloc] set_i32be : bytes @ local -> int -> int32_u -> unit @@ portable
  val[@zero_alloc] set_i32le : bytes @ local -> int -> int32_u -> unit @@ portable

  val[@zero_alloc] set_u32be : bytes @ local -> int -> int64_u -> unit @@ portable
  (** [set_u32be b off v] writes the low 32 bits of [v] big-endian. *)

  val[@zero_alloc] set_u32le : bytes @ local -> int -> int64_u -> unit @@ portable
  (** [set_u32le b off v] writes the low 32 bits of [v] little-endian. *)

  val[@zero_alloc] set_i64be : bytes @ local -> int -> int64_u -> unit @@ portable
  val[@zero_alloc] set_i64le : bytes @ local -> int -> int64_u -> unit @@ portable

  val[@zero_alloc] get_uint8 : bytes @ local -> int -> int @@ portable
  (** [get_uint8] and the [get_] reads below check their bounds, as [Bytes]'
      do. *)

  val[@zero_alloc] get_uint16_be : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] get_uint16_le : bytes @ local -> int -> int @@ portable
  val[@zero_alloc] get_int32_be : bytes @ local -> int -> int32_u @@ portable
  val[@zero_alloc] get_int32_le : bytes @ local -> int -> int32_u @@ portable

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
    val[@zero_alloc] i32be : t @ local -> int -> int32_u @@ portable
    val[@zero_alloc] i32le : t @ local -> int -> int32_u @@ portable
    val[@zero_alloc] i64be : t @ local -> int -> int64_u @@ portable
    val[@zero_alloc] i64le : t @ local -> int -> int64_u @@ portable
    val[@zero_alloc] set_u8 : t @ local -> int -> int -> unit @@ portable
    val[@zero_alloc] set_u16be : t @ local -> int -> int -> unit @@ portable
    val[@zero_alloc] set_u16le : t @ local -> int -> int -> unit @@ portable
    val[@zero_alloc] set_i32be : t @ local -> int -> int32_u -> unit @@ portable
    val[@zero_alloc] set_i32le : t @ local -> int -> int32_u -> unit @@ portable
    val[@zero_alloc] set_u32be : t @ local -> int -> int64_u -> unit @@ portable
    val[@zero_alloc] set_u32le : t @ local -> int -> int64_u -> unit @@ portable
    val[@zero_alloc] set_i64be : t @ local -> int -> int64_u -> unit @@ portable
    val[@zero_alloc] set_i64le : t @ local -> int -> int64_u -> unit @@ portable
    val[@zero_alloc] get_uint8 : t @ local -> int -> int @@ portable
    val[@zero_alloc] get_uint16_be : t @ local -> int -> int @@ portable
    val[@zero_alloc] get_uint16_le : t @ local -> int -> int @@ portable
    val[@zero_alloc] get_int32_be : t @ local -> int -> int32_u @@ portable
    val[@zero_alloc] get_int32_le : t @ local -> int -> int32_u @@ portable
    val sub_string : t @ local -> int -> int -> string @@ portable

    val[@zero_alloc] unsafe_blit_string :
      string -> int -> t @ local -> int -> int -> unit @@ portable
  end
end
