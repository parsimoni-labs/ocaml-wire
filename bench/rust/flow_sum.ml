(* A key is [(Flow_int.of_int lxor ports) + (proto lsl 32) + (bytes lsl 40) + (version lsl
   57)], [w] the xor of its address words, computed on [Flow_int]: below
   2^62, so the sum of the low 62 bits is the one of cargo/src/flow.rs. *)

type t = { mutable keys : int; mutable sum : int }

let state = { keys = 0; sum = 0 }

let reset () =
  state.keys <- 0;
  state.sum <- 0

let key ~version ~proto ~ports ~bytes w =
  let open Flow_int in
  add
    (add (add (logxor w ports) (shift_left proto 32)) (shift_left bytes 40))
    (shift_left version 57)

let key4 ~src ~dst ~proto ~ports ~bytes =
  key ~version:(Flow_int.of_int 4) ~proto ~ports ~bytes
    (Flow_int.logxor src dst)

let key6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  let open Flow_int in
  key ~version:(of_int 6) ~proto ~ports ~bytes
    (logxor
       (logxor
          (logxor (logxor (logxor (logxor (logxor s0 s1) s2) s3) d0) d1)
          d2)
       d3)

let add_pass ~keys ~sum =
  state.keys <- state.keys + keys;
  state.sum <- state.sum + sum

let add4 ~src ~dst ~proto ~ports ~bytes =
  add_pass ~keys:1
    ~sum:
      (Flow_int.to_int
         (key4 ~src:(Flow_int.of_int src) ~dst:(Flow_int.of_int dst)
            ~proto:(Flow_int.of_int proto) ~ports:(Flow_int.of_int ports)
            ~bytes:(Flow_int.of_int bytes)))

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  add_pass ~keys:1
    ~sum:
      (Flow_int.to_int
         (key6 ~s0:(Flow_int.of_int s0) ~s1:(Flow_int.of_int s1)
            ~s2:(Flow_int.of_int s2) ~s3:(Flow_int.of_int s3)
            ~d0:(Flow_int.of_int d0) ~d1:(Flow_int.of_int d1)
            ~d2:(Flow_int.of_int d2) ~d3:(Flow_int.of_int d3)
            ~proto:(Flow_int.of_int proto) ~ports:(Flow_int.of_int ports)
            ~bytes:(Flow_int.of_int bytes)))

let mask62 = (1 lsl 62) - 1
let summary () = (state.keys, state.sum land mask62)
