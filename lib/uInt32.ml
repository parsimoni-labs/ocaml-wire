type t = Optint.t

let pp = Optint.pp

(* Compose two unboxed 16-bit reads/writes through [Optint]. On a 64-bit host
   [Optint.t] is an unboxed native [int], so the shifts and [logor] stay in
   registers exactly like the old [int] code (measured identical, zero alloc);
   where the native [int] is narrower than 32 bits [Optint.t] falls back to a
   boxed [int32], which is what keeps a value with bit 31 set (a TCP sequence
   number) from being truncated. *)

let mask16 = Optint.of_int 0xFFFF

let le buf off =
  let lo = Bytes.get_uint16_le buf off in
  let hi = Bytes.get_uint16_le buf (off + 2) in
  Optint.(logor (of_int lo) (shift_left (of_int hi) 16))

let be buf off =
  let hi = Bytes.get_uint16_be buf off in
  let lo = Bytes.get_uint16_be buf (off + 2) in
  Optint.(logor (shift_left (of_int hi) 16) (of_int lo))

let low16 v = Optint.to_int (Optint.logand v mask16)

let set_le buf off v =
  Bytes.set_uint16_le buf off (low16 v);
  Bytes.set_uint16_le buf (off + 2) (low16 (Optint.shift_right_logical v 16))

let set_be buf off v =
  Bytes.set_uint16_be buf off (low16 (Optint.shift_right_logical v 16));
  Bytes.set_uint16_be buf (off + 2) (low16 v)

let to_int = Optint.to_int
let of_int v = Optint.of_int (v land 0xFFFF_FFFF)
let to_int32 = Optint.to_int32
let of_int32 = Optint.of_int32
