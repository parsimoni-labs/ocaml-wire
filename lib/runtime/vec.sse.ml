type t = int32x4

external load : bytes @ local -> int -> t @@ portable = "%caml_bytes_getu128u"
external store : bytes @ local -> int -> t -> unit @@ portable = "%caml_bytes_setu128u"

external load_bigstring :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t @ local ->
  int ->
  t @@ portable = "%caml_bigstring_getu128u"

external store_bigstring :
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t @ local ->
  int ->
  t ->
  unit @@ portable = "%caml_bigstring_setu128u"

external add : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_sse2_int32x4_add"
[@@noalloc] [@@unboxed] [@@builtin]

external sub : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_sse2_int32x4_sub"
[@@noalloc] [@@unboxed] [@@builtin]

external logand : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_sse_vec128_and"
[@@noalloc] [@@unboxed] [@@builtin]

external logor : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_sse_vec128_or"
[@@noalloc] [@@unboxed] [@@builtin]

external equal : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_sse2_int32x4_cmpeq"
[@@noalloc] [@@unboxed] [@@builtin]

external shift_right_logical : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_sse2_int32x4_srli"
[@@noalloc] [@@builtin]

external shift_left : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_sse2_int32x4_slli"
[@@noalloc] [@@builtin]

external low_of : int32 -> t @@ portable
  = "caml_vec128_unreachable" "caml_int32x4_low_of_int32"
[@@noalloc] [@@unboxed] [@@builtin]

(* [shuffle 0 a a] has lane 0 of [a] in every lane. *)
external shuffle : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_sse_vec128_shuffle_32"
[@@noalloc] [@@builtin]

external extract : (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_sse41_int32x4_extract"
[@@noalloc] [@@builtin]

external insert :
  (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_sse41_int32x4_insert"
[@@noalloc] [@@builtin]

(* palignr: the 32 bytes [high:low] shifted right by [n] bytes, low half. *)
external align_right :
  (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_ssse3_vec128_align_right_bytes"
[@@noalloc] [@@builtin]

external to_int8x16 : t -> int8x16 @@ portable = "caml_vec128_unreachable" "caml_vec128_cast"
[@@noalloc] [@@unboxed] [@@builtin]

external of_int8x16 : int8x16 -> t @@ portable = "caml_vec128_unreachable" "caml_vec128_cast"
[@@noalloc] [@@unboxed] [@@builtin]

external to_int64x2 : t -> int64x2 @@ portable = "caml_vec128_unreachable" "caml_vec128_cast"
[@@noalloc] [@@unboxed] [@@builtin]

(* ptest: 1 when [a land b] is all zeros. *)
external testz : (int64x2[@unboxed]) -> (int64x2[@unboxed]) -> (int[@untagged]) @@ portable
  = "caml_vec128_unreachable" "caml_sse41_vec128_testz"
[@@noalloc] [@@builtin]

let[@inline] v n =
  let x = low_of (Int32.of_int n) in
  shuffle 0 x x

let[@inline] is_zero a = equal a (v 0)

let[@inline] v4 a b c d =
  insert 3
    (insert 2 (insert 1 (v a) (Int32.of_int b)) (Int32.of_int c))
    (Int32.of_int d)

let[@inline] get a i =
  let x =
    match i with
    | 0 -> extract 0 a
    | 1 -> extract 1 a
    | 2 -> extract 2 a
    | _ -> extract 3 a
  in
  Int32.to_int x land 0xFFFF_FFFF

(* [a] high and [prev] low, shifted right 12 bytes: [prev]'s last lane, then
   [a]'s first three. The test of [before] in test/ocaml-ox checks the operand
   order on amd64. *)
let[@inline] before prev a =
  of_int8x16 (align_right 12 (to_int8x16 a) (to_int8x16 prev))

let[@inline] sum a = get a 0 + get a 1 + get a 2 + get a 3

let[@inline] is_all_zero a =
  let w = to_int64x2 a in
  testz w w = 1
