let[@inline] add_overflows a b sum = a lxor sum land (b lxor sum) < 0

let[@inline] sub_overflows a b difference =
  a lxor b land (a lxor difference) < 0

let[@inline] mul_overflows a b product =
  a <> 0 && ((a = -1 && b = min_int) || product / a <> b)

exception Overflow

let[@inline] add a b =
  let sum = a + b in
  if add_overflows a b sum then raise_notrace Overflow else sum

let[@inline] sub a b =
  let difference = a - b in
  if sub_overflows a b difference then raise_notrace Overflow else difference

let[@inline] mul a b =
  let product = a * b in
  if mul_overflows a b product then raise_notrace Overflow else product

(* [Value_out_of_range] carries an [int64], but a product may overflow that as
   well as the native int. The first value on the overflowing side is enough
   to report the failed range check without wrapping its diagnostic too. *)
let[@inline never] overflow ~positive =
  let value =
    if positive then Int64.succ (Int64.of_int max_int)
    else Int64.pred (Int64.of_int min_int)
  in
  raise
    (Errors.Parse_error
       { at = 0; field = []; kind = Value_out_of_range { value } })

let[@inline] checked_add a b =
  let sum = a + b in
  if add_overflows a b sum then overflow ~positive:(a >= 0) else sum

let[@inline] checked_sub a b =
  let difference = a - b in
  if sub_overflows a b difference then overflow ~positive:(a >= 0)
  else difference

let[@inline] checked_mul a b =
  let product = a * b in
  if mul_overflows a b product then
    overflow ~positive:(Bool.equal (a < 0) (b < 0))
  else product
