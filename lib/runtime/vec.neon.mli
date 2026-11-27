(** Four 32-bit lanes in a NEON register, for the code {!Wire_ocaml} generates
    with [~simd:true] on OxCaml for arm64.

    Each operation is a compiler builtin: it compiles to one or two NEON
    instructions and never calls a function. A lane holds the four bytes of a
    record in buffer order, read as a little-endian [int32]: byte [k] of the
    record is bits [8k] to [8k + 7] of its lane. *)

type t = int32x4
(** Four [int32] lanes. *)

external load : bytes @ local -> int -> t @@ portable = "%caml_bytes_getu128u"
(** [load buf off] is the 16 bytes of [buf] at [off], with no bounds check. *)

external store : bytes @ local -> int -> t -> unit @@ portable = "%caml_bytes_setu128u"
(** [store buf off v] writes the 16 bytes of [v] at [off], with no bounds
    check. *)

external add : t -> t -> t @@ portable = "caml_vec128_unreachable" "caml_neon_int32x4_add"
[@@noalloc] [@@unboxed] [@@builtin]
(** [add a b] adds lane by lane, modulo [2{^32}]. *)

external sub : t -> t -> t @@ portable = "caml_vec128_unreachable" "caml_neon_int32x4_sub"
[@@noalloc] [@@unboxed] [@@builtin]
(** [sub a b] subtracts lane by lane, modulo [2{^32}]. *)

external logand : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_and"
[@@noalloc] [@@unboxed] [@@builtin]
(** [logand a b] is the bitwise and of [a] and [b]. *)

external logor : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_or"
[@@noalloc] [@@unboxed] [@@builtin]
(** [logor a b] is the bitwise or of [a] and [b]. *)

external equal : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeq"
[@@noalloc] [@@unboxed] [@@builtin]
(** [equal a b] has all bits set in each lane where [a] and [b] are equal, and
    none elsewhere. *)

external is_zero : t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeqz"
[@@noalloc] [@@unboxed] [@@builtin]
(** [is_zero a] has all bits set in each lane of [a] that is [0], and none
    elsewhere. *)

external shift_right_logical : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_srli"
[@@noalloc] [@@builtin]
(** [shift_right_logical n a] shifts each lane of [a] right by the constant
    [n], filling with zeros. *)

external shift_left : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_slli"
[@@noalloc] [@@builtin]
(** [shift_left n a] shifts each lane of [a] left by the constant [n]. *)

val v : int -> t @@ portable
(** [v n] has [n] in every lane, modulo [2{^32}]. *)

val v4 : int -> int -> int -> int -> t @@ portable
(** [v4 a b c d] has [a] to [d] in lanes 0 to 3. *)

val get : t -> int -> int @@ portable
(** [get a i] is lane [i] of [a], from 0 to 3, as an unsigned 32-bit value. *)

val before : t -> t -> t @@ portable
(** [before prev a] is the lanes [prev.(3); a.(0); a.(1); a.(2)]: each lane of
    [a] is replaced by the one before it, the first by the last of [prev]. *)

val sum : t -> int @@ portable
(** [sum a] is the sum of the four lanes, each unsigned. *)

val is_all_zero : t -> bool @@ portable
(** [is_all_zero a] is [true] when every lane of [a] is [0]. *)
