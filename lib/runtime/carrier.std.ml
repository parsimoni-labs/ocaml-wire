type t = Optint.t

let is_immediate = Optint.is_immediate
let zero = Optint.zero
let of_int = Optint.of_int

(* [Optint.to_int] is a field of a first-class module, which OCaml without
   flambda calls; where the carrier is the native [int] it is the identity,
   which the match below inlines to nothing. *)
let to_int (v : t) : int =
  match is_immediate with True -> v | False -> Optint.to_int v

let of_int32 = Optint.of_int32
let to_int32 = Optint.to_int32
let logand = Optint.logand
let shift_right_logical = Optint.shift_right_logical
let compare = Optint.compare
let equal = Optint.equal
let to_string = Optint.to_string
let pp = Optint.pp
