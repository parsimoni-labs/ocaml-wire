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

(** The identity on [int], which a carrier that is an [int] includes as its
    [unsafe_of_int]: a primitive, so the compiler keeps no binding for it. *)
module Identity : sig
  external unsafe_of_int : int -> int = "%identity"
  (** [unsafe_of_int n] is [n]. *)
end
