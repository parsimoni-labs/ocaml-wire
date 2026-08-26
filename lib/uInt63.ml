(* Carrier for [uint ~size], the 1-to-7-byte unsigned integers: 56 bits at the
   widest, well inside the range Optint.Int63 represents exactly.

   Backed by Optint.Int63: an unboxed native int on a 64-bit host, a boxed
   int64 where the native int is narrower. On such a target a plain-int
   composition would shift past the word and lose the high half. *)

module I = Optint.Int63

type t = I.t

let pp = I.pp

(* A [size]-byte unsigned field owns exactly that many bytes, and the carrier is
   signed and wider than any width it serves. A negative value, or one wider
   than the field, loses its high bits on the way out and reaches the wire as a
   legal smaller number that no decoder, [validate] or generated C validator can
   tell from the one the caller meant, so writing refuses instead. *)
let check_encode ~size v =
  let too_wide =
    size < 8 && I.compare v (I.shift_left I.one (8 * max size 0)) >= 0
  in
  if I.compare v I.zero < 0 || too_wide then
    Fmt.invalid_arg
      "Wire.encode: value %a does not fit an unsigned %d-byte field" I.pp v size

let to_int = I.to_int
let of_int = I.of_int
