(* The flow rows' entries over g_flow: [classify] adds each packet of an index
   to [Flow_table], [parse] folds its key into [Flow_sum]. *)

let classify buf offs lens =
  ignore (G_flow.classify_index buf ~offs ~lens ~init:0 : int)

let parse buf offs lens =
  Flow_parse.finish
    (G_flow.parse_index buf ~offs ~lens ~init:(Flow_parse.init ()))
