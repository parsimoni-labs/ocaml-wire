(* A key is [(w lxor ports) + (proto lsl 32) + (bytes lsl 40) + (version lsl
   57)], [w] the xor of its address words: below 2^62, so the sum of the low
   62 bits is the one of cargo/src/flow.rs. *)

type t = { mutable keys : int; mutable sum : int }

let state = { keys = 0; sum = 0 }

let reset () =
  state.keys <- 0;
  state.sum <- 0

let fold ~version ~proto ~ports ~bytes w =
  state.keys <- state.keys + 1;
  state.sum <-
    state.sum
    + ((w lxor ports) + (proto lsl 32) + (bytes lsl 40) + (version lsl 57))

let add4 ~src ~dst ~proto ~ports ~bytes =
  fold ~version:4 ~proto ~ports ~bytes (src lxor dst)

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  fold ~version:6 ~proto ~ports ~bytes
    (s0 lxor s1 lxor s2 lxor s3 lxor d0 lxor d1 lxor d2 lxor d3)

let mask62 = (1 lsl 62) - 1
let summary () = (state.keys, state.sum land mask62)
