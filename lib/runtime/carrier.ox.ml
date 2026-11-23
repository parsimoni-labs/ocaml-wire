type t = int

let is_immediate : (t, int, int32) Optint.Conditional.t = True
let zero = 0
let[@inline] of_int x = x
let[@inline] to_int x = x
let uint32_max = (0xffff lsl 16) lor 0xffff
let int32_max = 0x7fffffff

(* A negative [int32] keeps its sign, as [Optint.of_int32] does. *)
let[@inline] of_int32 x = Int32.to_int x

let to_int32 x =
  let truncated = x land uint32_max in
  if x = truncated then Int32.of_int truncated
  else if x < 0 && x lsr 31 = uint32_max then
    Int32.logor 0x80000000l (Int32.of_int (x land int32_max))
  else
    invalid_arg
      ("Optint.to_int32: " ^ string_of_int x
     ^ " can not fit into a 32 bits integer")

let[@inline] logand a b = a land b
let[@inline] shift_right_logical a n = a lsr n
let[@inline] compare (a : int) b = a - b
let[@inline] equal (a : int) b = a = b
let to_string x = string_of_int x
let pp ppf x = Format.fprintf ppf "%d" x
