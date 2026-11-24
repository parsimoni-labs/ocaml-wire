(* A key is [(w lxor ports) + (proto lsl 32) + (bytes lsl 40) + (version lsl
   57)], [w] the xor of its address words: below 2^62, so the sum of the low
   62 bits is the one of cargo/src/flow.rs. *)

type t = { mutable keys : int; mutable sum : int }

let state = { keys = 0; sum = 0 }

let reset () =
  state.keys <- 0;
  state.sum <- 0

let key ~version ~proto ~ports ~bytes w =
  (w lxor ports) + (proto lsl 32) + (bytes lsl 40) + (version lsl 57)

let key4 ~src ~dst ~proto ~ports ~bytes =
  key ~version:4 ~proto ~ports ~bytes (src lxor dst)

let key6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  key ~version:6 ~proto ~ports ~bytes
    (s0 lxor s1 lxor s2 lxor s3 lxor d0 lxor d1 lxor d2 lxor d3)

let add_pass ~keys ~sum =
  state.keys <- state.keys + keys;
  state.sum <- state.sum + sum

let add4 ~src ~dst ~proto ~ports ~bytes =
  add_pass ~keys:1 ~sum:(key4 ~src ~dst ~proto ~ports ~bytes)

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  add_pass ~keys:1
    ~sum:(key6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes)

let mask62 = (1 lsl 62) - 1
let summary () = (state.keys, state.sum land mask62)
