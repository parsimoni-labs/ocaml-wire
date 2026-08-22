(* Unsigned 63-bit integer. Reads 8 bytes and masks to 63 bits.

   Backed by Optint.Int63: an unboxed native int on a 64-bit host (identical to
   the old code there), a boxed int64 where the native int is narrower. On such
   a target the old plain-int composition shifted past the word ([w2 lsl 32],
   [w3 lsl 48]) and lost the high half entirely. *)

module I = Optint.Int63

type t = I.t

let pp = I.pp
let mask16 = I.of_int 0xFFFF

let compose w0 w1 w2 w3 =
  I.(
    logand
      (logor (of_int w0)
         (logor
            (shift_left (of_int w1) 16)
            (logor (shift_left (of_int w2) 32) (shift_left (of_int w3) 48))))
      max_int)

let le buf off =
  compose
    (Bytes.get_uint16_le buf off)
    (Bytes.get_uint16_le buf (off + 2))
    (Bytes.get_uint16_le buf (off + 4))
    (Bytes.get_uint16_le buf (off + 6))

let be buf off =
  compose
    (Bytes.get_uint16_be buf (off + 6))
    (Bytes.get_uint16_be buf (off + 4))
    (Bytes.get_uint16_be buf (off + 2))
    (Bytes.get_uint16_be buf off)

let chunk v shift = I.to_int (I.logand (I.shift_right_logical v shift) mask16)

(* A [size]-byte unsigned field owns exactly that many bytes, and the carrier is
   a signed 63-bit integer. A negative value, or one wider than the field, loses
   its high bits to [chunk] and reaches the wire as a legal smaller number that
   no decoder, [validate] or generated C validator can tell from the one the
   caller meant, so writing refuses instead. Eight bytes hold every non-negative
   [t], so at that width only the sign can be wrong. *)
let check_encode ~size v =
  let too_wide =
    size < 8 && I.compare v (I.shift_left I.one (8 * max size 0)) >= 0
  in
  if I.compare v I.zero < 0 || too_wide then
    Fmt.invalid_arg
      "Wire.encode: value %a does not fit an unsigned %d-byte field" I.pp v size

(* Every encoder path -- typ-level, compiled field, [Codec.set] -- writes a
   uint63 through here, so the width check belongs here too. *)
let set_le buf off v =
  check_encode ~size:8 v;
  Bytes.set_uint16_le buf off (chunk v 0);
  Bytes.set_uint16_le buf (off + 2) (chunk v 16);
  Bytes.set_uint16_le buf (off + 4) (chunk v 32);
  Bytes.set_uint16_le buf (off + 6) (chunk v 48)

let set_be buf off v =
  check_encode ~size:8 v;
  Bytes.set_uint16_be buf off (chunk v 48);
  Bytes.set_uint16_be buf (off + 2) (chunk v 32);
  Bytes.set_uint16_be buf (off + 4) (chunk v 16);
  Bytes.set_uint16_be buf (off + 6) (chunk v 0)

let to_int = I.to_int
let of_int = I.of_int
