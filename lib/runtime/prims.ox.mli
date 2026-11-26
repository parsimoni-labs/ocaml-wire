(** The primitives of prims.std.mli, each [portable]: a primitive declared
    without the modality is not, and a module built on these is only portable
    if they are. *)

external get16 : bytes -> int -> int @@ portable = "%caml_bytes_get16u"
(** [get16 buf off] is the 16-bit value at [off], in the host's byte order. *)

external get32 : bytes -> int -> int32 @@ portable = "%caml_bytes_get32u"
(** [get32 buf off] is the 32-bit value at [off], in the host's byte order. *)

external get64 : bytes -> int -> int64 @@ portable = "%caml_bytes_get64u"
(** [get64 buf off] is the 64-bit value at [off], in the host's byte order. *)

external set16 : bytes -> int -> int -> unit @@ portable = "%caml_bytes_set16u"
(** [set16 buf off v] stores the low 16 bits of [v] at [off]. *)

external set32 : bytes -> int -> int32 -> unit @@ portable = "%caml_bytes_set32u"
(** [set32 buf off v] stores [v] at [off]. *)

external set64 : bytes -> int -> int64 -> unit @@ portable = "%caml_bytes_set64u"
(** [set64 buf off v] stores [v] at [off]. *)

external swap16 : int -> int @@ portable = "%bswap16"
(** [swap16 v] is the low 16 bits of [v] with their two bytes swapped. *)

external swap32 : int32 -> int32 @@ portable = "%bswap_int32"
(** [swap32 v] is [v] with its bytes in reverse order. *)

external swap64 : int64 -> int64 @@ portable = "%bswap_int64"
(** [swap64 v] is [v] with its bytes in reverse order. *)

(** The identity on [int], which a carrier that is an [int] includes as its
    [unsafe_of_int]: a primitive, so the compiler keeps no binding for it. *)
module Identity : sig
  external unsafe_of_int : int -> int @@ portable = "%identity"
  (** [unsafe_of_int n] is [n]. *)
end
