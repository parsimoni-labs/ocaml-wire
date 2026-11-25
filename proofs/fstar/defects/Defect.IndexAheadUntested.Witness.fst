module Defect.IndexAheadUntested.Witness

(* An index of one frame of no byte at 0, in a buffer of none, whose
   leading fields need one byte, the one at 0 loaded ahead: under
   index-ahead-untested.sed the loop loads that byte before the frame's
   test, which fails. This module only holds against that loop; the
   model's loads nothing of a frame whose test fails. *)

open Plan.Index

let ix : index = { n = 1; offs = (fun _ -> 0); lens = (fun _ -> 0) }

let _ = assert_norm (index_loop ix 0 1 [ (0, 1) ] == [ Load 0 0 1; Test 0 false; Walk 0 false ])
