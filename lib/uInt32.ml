type t = int

let pp = Fmt.int

(* Compose two unboxed 16-bit reads/writes rather than going through
   [Int32.to_int] / [Int32.of_int], which box an [int32] per call. The
   result stays in the native [int]. *)

let le buf off =
  let lo = Bytes.get_uint16_le buf off in
  let hi = Bytes.get_uint16_le buf (off + 2) in
  lo lor (hi lsl 16)

let be buf off =
  let hi = Bytes.get_uint16_be buf off in
  let lo = Bytes.get_uint16_be buf (off + 2) in
  (hi lsl 16) lor lo

let set_le buf off v =
  Bytes.set_uint16_le buf off (v land 0xFFFF);
  Bytes.set_uint16_le buf (off + 2) ((v lsr 16) land 0xFFFF)

let set_be buf off v =
  Bytes.set_uint16_be buf off ((v lsr 16) land 0xFFFF);
  Bytes.set_uint16_be buf (off + 2) (v land 0xFFFF)

let to_int t = t
let of_int t = t land 0xFFFF_FFFF
