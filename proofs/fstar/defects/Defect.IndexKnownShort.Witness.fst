module Defect.IndexKnownShort.Witness

(* A frame of one byte at 0, in a buffer of one byte, whose leading fields
   need two, the one at 1 loaded ahead: under index-known-short.sed its test
   passes, the field is loaded past the frame and the buffer, and the frame
   is walked knowing two bytes. This module only holds against that test;
   the model's fails the frame. *)

open Plan.Index

let ix : index = { n = 1; offs = (fun _ -> 0); lens = (fun _ -> 1) }

let _ = assert_norm (known 1 2 0 1)
let _ = assert_norm (index_loop ix 1 2 [ (1, 1) ] == [ Test 0 true; Load 0 1 1; Walk 0 true ])
