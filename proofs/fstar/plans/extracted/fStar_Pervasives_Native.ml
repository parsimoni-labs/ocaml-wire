(* What the code F* extracts from the model needs of
   FStar.Pervasives.Native. *)

type nonrec 'a option = 'a option = None | Some of 'a

let uu___is_None = function None -> true | Some _ -> false
let uu___is_Some = function Some _ -> true | None -> false
let fst (a, _) = a
let snd (_, b) = b
