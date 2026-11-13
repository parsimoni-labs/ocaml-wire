type t = int

let min_int = -128
let max_int = 127
let zero = 0
let equal : t -> t -> bool = Int.equal
let pp = Format.pp_print_int

(* The one place the range is enforced. Every other path takes a [t] and so
   takes a value the field holds, which is what lets the encoders drop their
   own width checks instead of carrying one that cannot fire. *)
let[@inline never] out_of_range n =
  invalid_arg
    ("Wire.SInt8.v: " ^ string_of_int n ^ " is not a signed 8-bit value")

let[@inline] v n = if n < min_int || n > max_int then out_of_range n else n
let v_opt n = if n < min_int || n > max_int then None else Some n
let to_int n = n
let get buf off = Bytes.get_int8 buf off
let set buf off n = Bytes.set_int8 buf off n

let unsafe_get buf off =
  (Unchecked.u8 buf off lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let int_eq : (t, int) Type.eq = Type.Equal
