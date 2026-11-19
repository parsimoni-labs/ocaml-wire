(* No SIMD lanes outside OxCaml on arm64 or amd64: no rows. *)

let clcw (_ : bytes) = 0
let report (_ : int) (out : bytes) = out
let rows = false
