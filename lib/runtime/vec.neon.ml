type t = int32x4

external load : bytes @ local -> int -> t @@ portable = "%caml_bytes_getu128u"
external store : bytes @ local -> int -> t -> unit @@ portable = "%caml_bytes_setu128u"

external add : t -> t -> t @@ portable = "caml_vec128_unreachable" "caml_neon_int32x4_add"
[@@noalloc] [@@unboxed] [@@builtin]

external sub : t -> t -> t @@ portable = "caml_vec128_unreachable" "caml_neon_int32x4_sub"
[@@noalloc] [@@unboxed] [@@builtin]

external logand : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_and"
[@@noalloc] [@@unboxed] [@@builtin]

external logor : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_or"
[@@noalloc] [@@unboxed] [@@builtin]

external equal : t -> t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeq"
[@@noalloc] [@@unboxed] [@@builtin]

external is_zero : t -> t @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeqz"
[@@noalloc] [@@unboxed] [@@builtin]

external shift_right_logical : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_srli"
[@@noalloc] [@@builtin]

external shift_left : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_slli"
[@@noalloc] [@@builtin]

external low_of : int32 -> t @@ portable
  = "caml_vec128_unreachable" "caml_int32x4_low_of_int32"
[@@noalloc] [@@unboxed] [@@builtin]

external dup : t -> t @@ portable = "caml_vec128_unreachable" "caml_neon_int32x4_dup"
[@@noalloc] [@@unboxed] [@@builtin]

external extract : (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_extract"
[@@noalloc] [@@builtin]

external insert :
  (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) -> (t[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int32x4_insert"
[@@noalloc] [@@builtin]

external ext8 :
  (int[@untagged]) ->
  low:(int8x16[@unboxed]) ->
  high:(int8x16[@unboxed]) ->
  (int8x16[@unboxed]) @@ portable = "caml_vec128_unreachable" "caml_neon_int8x16_ext"
[@@noalloc] [@@builtin]

external to_int8x16 : t -> int8x16 @@ portable = "caml_vec128_unreachable" "caml_vec128_cast"
[@@noalloc] [@@unboxed] [@@builtin]

external of_int8x16 : int8x16 -> t @@ portable
  = "caml_vec128_unreachable" "caml_vec128_cast"
[@@noalloc] [@@unboxed] [@@builtin]

external to_int64x2 : t -> int64x2 @@ portable = "caml_vec128_unreachable" "caml_vec128_cast"
[@@noalloc] [@@unboxed] [@@builtin]

external extract64 : (int[@untagged]) -> (int64x2[@unboxed]) -> (int64[@unboxed]) @@ portable
  = "caml_vec128_unreachable" "caml_neon_int64x2_extract"
[@@noalloc] [@@builtin]

let[@inline] v n = dup (low_of (Int32.of_int n))

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

let[@inline] before prev a =
  of_int8x16 (ext8 12 ~low:(to_int8x16 prev) ~high:(to_int8x16 a))

let[@inline] sum a = get a 0 + get a 1 + get a 2 + get a 3

let[@inline] is_all_zero a =
  let w = to_int64x2 a in
  Int64.equal (Int64.logor (extract64 0 w) (extract64 1 w)) 0L
