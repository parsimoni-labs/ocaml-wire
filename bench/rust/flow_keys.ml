(* The keys a pass hands its sink, in the order it hands them. *)

let stride = 12
let keys = ref []

let add4 ~src ~dst ~proto ~ports ~bytes =
  keys := [| 4; proto; ports; bytes; src; 0; 0; 0; dst; 0; 0; 0 |] :: !keys

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  keys := [| 6; proto; ports; bytes; s0; s1; s2; s3; d0; d1; d2; d3 |] :: !keys

let take () =
  let k = Array.concat (List.rev !keys) in
  keys := [];
  k
