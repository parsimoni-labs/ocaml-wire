module Defect.IndexAheadPast.Witness

(* A field of two bytes at 0 loaded ahead by a loop that knows one byte:
   under index-ahead-past.sed make plans accepts it, and the loop loads it
   from a frame of one byte at 0 in a buffer of one byte, past both. This
   module only holds against that check; the model's refuses the field. *)

open Plan.Decide
open Plan.Index

let ix : index = { n = 1; offs = (fun _ -> 0); lens = (fun _ -> 1) }

let _ = assert_norm (ahead_in 1 [ (0, 2) ])
let _ = assert_norm (index_loop ix 1 1 [ (0, 2) ] == [ Test 0 true; Load 0 0 2; Walk 0 true ])
