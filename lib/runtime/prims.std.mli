(** The compiler primitives the runtime reads and writes buffers with, with no
    bounds check unless named [safe_]. They are declared once, here, because
    only OxCaml can say that a primitive is [portable]: prims.ox.mli says it,
    and this file is the same declarations for OCaml. *)

external get16 : bytes -> int -> int = "%caml_bytes_get16u"
(** [get16 buf off] is the 16-bit value at [off], in the host's byte order. *)

external get32 : bytes -> int -> int32 = "%caml_bytes_get32u"
(** [get32 buf off] is the 32-bit value at [off], in the host's byte order. *)

external get64 : bytes -> int -> int64 = "%caml_bytes_get64u"
(** [get64 buf off] is the 64-bit value at [off], in the host's byte order. *)

external set16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
(** [set16 buf off v] stores the low 16 bits of [v] at [off]. *)

external set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
(** [set32 buf off v] stores [v] at [off]. *)

external set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
(** [set64 buf off v] stores [v] at [off]. *)

external swap16 : int -> int = "%bswap16"
(** [swap16 v] is the low 16 bits of [v] with their two bytes swapped. *)

external swap32 : int32 -> int32 = "%bswap_int32"
(** [swap32 v] is [v] with its bytes in reverse order. *)

external swap64 : int64 -> int64 = "%bswap_int64"
(** [swap64 v] is [v] with its bytes in reverse order. *)

(** The same on a bigarray of bytes, with [length] and the checked reads. *)
module Bigstring : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external length : t -> int = "%caml_ba_dim_1"
  (** [length buf] is the number of bytes of [buf]. *)

  external get : t -> int -> char = "%caml_ba_unsafe_ref_1"
  (** [get buf off] is the byte at [off]. *)

  external set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
  (** [set buf off c] stores [c] at [off]. *)

  external get16 : t -> int -> int = "%caml_bigstring_get16u"
  (** [get16] is {!Prims.get16} on {!type-t}. *)

  external get32 : t -> int -> int32 = "%caml_bigstring_get32u"
  (** [get32] is {!Prims.get32} on {!type-t}. *)

  external get64 : t -> int -> int64 = "%caml_bigstring_get64u"
  (** [get64] is {!Prims.get64} on {!type-t}. *)

  external set16 : t -> int -> int -> unit = "%caml_bigstring_set16u"
  (** [set16] is {!Prims.set16} on {!type-t}. *)

  external set32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
  (** [set32] is {!Prims.set32} on {!type-t}. *)

  external set64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"
  (** [set64] is {!Prims.set64} on {!type-t}. *)

  external safe_get : t -> int -> char = "%caml_ba_ref_1"
  (** [safe_get] is {!val-get}, raising [Invalid_argument] out of bounds. *)

  external safe_get16 : t -> int -> int = "%caml_bigstring_get16"
  (** [safe_get16] is {!val-get16}, raising [Invalid_argument] out of bounds. *)

  external safe_get32 : t -> int -> int32 = "%caml_bigstring_get32"
  (** [safe_get32] is {!val-get32}, raising [Invalid_argument] out of bounds. *)
end
