module Defect.PrefixWrap.Witness

(* Found by this model at b1f6f756, fixed by 1883d145. A variable-size
   nested codec adds nothing to its parent's min_size, so 33 nested codecs,
   each ending with a slice of 2^57 - 10 bytes, pass
   Codec_seal.add_min_size; Planner.fuse merged the 33 slices and the uint8
   after them into one prefix, whose size the emitter summed with OCaml's
   wrapping [+]. The sum wrapped negative, no bound was written, and the
   uint8 was loaded at a wrapped offset: the scanner accepted two bytes the
   codec rejects, reading far outside them.

   The plan: a uint8 length, a span of that many bytes, then the fused
   prefix. On two bytes the codec rejects, and the walk of prefix-wrap.sed,
   which adds with [wrap], reads outside the window. This module only holds
   against that emitter; the model's own, which adds with [extent], rejects
   as the codec does. *)

open FStar.Mul
open Plan.Expr
open Plan.Scan
open Plan.Proof

let run : nat = 33 * (pow2 57 - 10) + 1

let lim : var = 0
let plan : list node = [
  Prefix [ { id = 1; off = 0; width = 1 } ] 1;
  Span 2 3 (V 1);
  Prefix [ { id = 4; off = run - 1; width = 1 } ] run;
]

let rd : reader = fun o w -> 0
let en : env = fun v -> if v = lim then 2 else 0
let no_o1 : opts = plain

let _ = assert_norm (wf (start lim).sc plan)
let _ = assert_norm (spec rd 0 2 [] en 0 plan == Reject)
let _ = assert_norm (Fault? (exec no_o1 rd (top 2 lim) (start lim) en plan))
