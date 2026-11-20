(* What the code F* extracts from the model needs of FStar.List.Tot.Base. *)

let op_At = List.append
let mem x l = List.mem x l
let map = List.map
let partition = List.partition
let existsb = List.exists
let length l = Prims.of_int (List.length l)

let rec assoc k = function
  | [] -> None
  | (k', v) :: r -> if k' = k then Some v else assoc k r
