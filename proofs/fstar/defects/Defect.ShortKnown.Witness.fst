module Defect.ShortKnown.Witness

(* A frame of one byte, whose leading prefix needs two, the second holding a
   field: the walk short-known.sed gives it knows both bytes, tests no bound
   and reads the field past the frame, where the codec rejects. This module
   only holds against that loop; the model's walks the frame knowing no
   byte. *)

open Plan.Expr
open Plan.Scan
open Plan.Proof
open Plan.Loop

let lim : var = 0

let plan : list node = [ Prefix [ { id = 1; off = 1; width = 1 } ] 2 ]

let rd : reader = Plan.Bytes.reader_of (fun _ -> 0)
let en : env = fun v -> if v = lim then 1 else 0
let x : win = { wb = 0; len = 1; lim = lim; frames = [] }

let _ = assert_norm (spec rd 0 1 [] en 0 plan == Reject)
let _ = assert_norm (Fault? (exec plain rd x ({ start lim with known = frame_known 2 1 }) en plan))
