(* Unsigned 63-bit integer. Reads 8 bytes, masks to 63 bits (OCaml int). *)

type t = int

let pp = Fmt.int

(* Compose four unboxed 16-bit reads/writes rather than going through
   [Int64.to_int] / [Int64.of_int], which box an [int64] per call. The
   result stays in the native [int]; reads still mask to [max_int] (the
   top bit does not fit a 63-bit [int]), and the most-significant write
   chunk uses [asr] so it carries the same sign extension [Int64.of_int]
   produced. *)

let le buf off =
  let w0 = Bytes.get_uint16_le buf off in
  let w1 = Bytes.get_uint16_le buf (off + 2) in
  let w2 = Bytes.get_uint16_le buf (off + 4) in
  let w3 = Bytes.get_uint16_le buf (off + 6) in
  let v = w0 lor (w1 lsl 16) lor (w2 lsl 32) lor (w3 lsl 48) in
  v land max_int

let be buf off =
  let w0 = Bytes.get_uint16_be buf off in
  let w1 = Bytes.get_uint16_be buf (off + 2) in
  let w2 = Bytes.get_uint16_be buf (off + 4) in
  let w3 = Bytes.get_uint16_be buf (off + 6) in
  let v = (w0 lsl 48) lor (w1 lsl 32) lor (w2 lsl 16) lor w3 in
  v land max_int

let set_le buf off v =
  Bytes.set_uint16_le buf off (v land 0xFFFF);
  Bytes.set_uint16_le buf (off + 2) ((v lsr 16) land 0xFFFF);
  Bytes.set_uint16_le buf (off + 4) ((v lsr 32) land 0xFFFF);
  Bytes.set_uint16_le buf (off + 6) ((v asr 48) land 0xFFFF)

let set_be buf off v =
  Bytes.set_uint16_be buf off ((v asr 48) land 0xFFFF);
  Bytes.set_uint16_be buf (off + 2) ((v lsr 32) land 0xFFFF);
  Bytes.set_uint16_be buf (off + 4) ((v lsr 16) land 0xFFFF);
  Bytes.set_uint16_be buf (off + 6) (v land 0xFFFF)

let to_int t = t
let of_int t = t
