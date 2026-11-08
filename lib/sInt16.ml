type t = int

let min_int = -0x8000
let max_int = 0x7FFF
let zero = 0
let equal : t -> t -> bool = Int.equal
let pp = Fmt.int

(* The one place the range is enforced. Every other path takes a [t] and so
   takes a value the field holds, which is what lets the encoders drop their
   own width checks instead of carrying one that cannot fire. *)
let v n =
  if n < min_int || n > max_int then
    Fmt.invalid_arg "Wire.SInt16.v: %d is not a signed 16-bit value" n;
  n

let v_opt n = if n < min_int || n > max_int then None else Some n
let to_int n = n
let le buf off = Bytes.get_int16_le buf off
let be buf off = Bytes.get_int16_be buf off
let set_le buf off n = Bytes.set_int16_le buf off n
let set_be buf off n = Bytes.set_int16_be buf off n
let sext16 n = (n lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)
let unsafe_le buf off = sext16 (Unchecked.u16le buf off)
let unsafe_be buf off = sext16 (Unchecked.u16be buf off)
let int_eq : (t, int) Type.eq = Type.Equal
