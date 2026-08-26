type t = int64

let compare = Int64.compare
let equal = Int64.equal
let zero = 0L
let max_int = -1L
let pp ppf v = Fmt.pf ppf "%Lu" v
let le = Bytes.get_int64_le
let be = Bytes.get_int64_be
let set_le = Bytes.set_int64_le
let set_be = Bytes.set_int64_be
let to_int64 v = v
let of_int64 v = v
let to_int = Int64.to_int
let to_int_opt = Int64.unsigned_to_int

(* Refuses rather than reinterprets. A negative [int] spells a value above
   2^63 as an unsigned 64-bit number, which is not what a caller writing a
   negative literal meant; [of_int64] is where a bit pattern is reinterpreted
   on purpose. *)
let of_int n =
  if n < 0 then
    Fmt.invalid_arg "Wire.UInt64.of_int: %d is not an unsigned 64-bit value" n;
  Int64.of_int n
