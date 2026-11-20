module Defect.WordPast.Witness

(* Two bytes, the whole buffer, read with the shared words: the codec
   accepts, the walk of word-past-check.sed loads a word of eight bytes at 0
   and reads outside the window. This module only holds against that
   emitter; the model's own keeps no word there. *)

open Plan.Expr
open Plan.Scan
open Plan.Proof

let lim : var = 0
let plan : list node = [ Prefix [ { id = 1; off = 0; width = 1 }; { id = 2; off = 1; width = 1 } ] 2 ]

let rd : reader = fun _ _ -> 0
let en : env = fun v -> if v = lim then 2 else 0
let o : opts = { plain with words = true }

let _ = assert_norm (wf (start lim).sc plan)
let _ = assert_norm (Accept? (spec rd 0 2 [] en 0 plan))
let _ = assert_norm (Fault? (exec o rd (top 2 lim) (start lim) en plan))
