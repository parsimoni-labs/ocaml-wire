module Defect.RangeTop.Witness

(* A byte whose bits 5 to 4 hold [f = 1] and bits 3 to 2 [g >= 0], two bits
   above them unchecked: range-top-bits.sed passes the bounds [16, 31], and
   the byte 80, whose checks hold, lies outside them. This module only
   holds against that check. *)

open Plan.Word
open Plan.Range

let _ = assert_norm (range_ok 8 4 2 2 2 1 0 16 31)
let _ = assert_norm (view 80 4 2 == 1 /\ view 80 2 2 >= 0 /\ not (80 <= 31))
