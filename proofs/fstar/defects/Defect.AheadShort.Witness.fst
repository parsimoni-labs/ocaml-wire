module Defect.AheadShort.Witness

(* A loop of four frames an iteration whose leading fields need two bytes,
   one of them at offset 1 loaded ahead, over frames of one byte: under
   ahead-short.sed the frame at 0 has the byte at 1, past its end, loaded
   ahead of its walk. This module only holds against that loop; the model's
   loads nothing ahead of a frame shorter than the leading fields. *)

open Plan.Loop

let _ = assert_norm (frame_loads 4 2 [ (1, 1) ] 0 1 == [ (1, 1) ])
