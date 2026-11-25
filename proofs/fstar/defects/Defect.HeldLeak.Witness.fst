module Defect.HeldLeak.Witness

(* Twelve bytes holding 0 to 11, read big-endian: eight bytes whose first
   two the scanner takes from one word at 0, then a region of four bytes
   whose first byte must be 0. The codec rejects, the byte being 8; the walk
   of held-leak.sed takes it from the word held at 0 of the buffer, reads 0
   and accepts. This module only holds against that emitter; the model's
   own clears the words on entering the region. *)

open Plan.Expr
open Plan.Scan
open Plan.Proof

let lim : var = 0

let plan : list node = [
  Prefix [ { id = 1; off = 0; width = 1 }; { id = 2; off = 1; width = 1 } ] 8;
  Region 3 4 (Lit 4) true [
    Prefix [ { id = 5; off = 0; width = 1 } ] 1;
    Test (Ge (Lit 0) (V 5));
  ];
]

let buf : Plan.Bytes.mem = fun i -> if 0 <= i && i < 12 then i else 0
let rd : reader = Plan.Bytes.reader_of buf
let en : env = fun v -> if v = lim then 12 else 0
let o : opts = { plain with words = true }

let _ = assert_norm (wf (start lim).sc plan)
let _ = assert_norm (spec rd 0 12 [] en 0 plan == Reject)
let _ = assert_norm (Accept? (exec o rd (top 12 lim) (start lim) en plan))
