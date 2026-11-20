module Defect.PrefixWrap.Buggy

(* Found by this model at b1f6f756. A variable-size nested codec adds
   nothing to its parent's min_size, so 33 nested codecs, each ending with
   a slice of 2^57 - 10 bytes, pass Codec_seal.add_min_size; Planner.fuse
   merges the 33 slices and the uint8 after them into one prefix, whose
   size the emitter sums with OCaml's wrapping [+]. The sum wraps negative,
   no bound is written, and the uint8 is loaded at a wrapped offset: the
   scanner accepts two bytes the codec rejects, reading far outside them.

   The plan: a uint8 length, a span of that many bytes, then the fused
   prefix. The theorem the model proves for well-formed plans fails for it,
   since a run longer than any record breaks [wf]. *)

open FStar.Mul
open Plan.Expr
open Plan.Scan
open Plan.Proof

let big : int = pow2 57 - 10
let run : nat = 33 * (pow2 57 - 10) + 1

let lim : var = 0
let plan : list node = [
  Prefix [ { id = 1; off = 0; width = 1 } ] 1;
  Span 2 3 (V 1);
  Prefix [ { id = 4; off = run - 1; width = 1 } ] run;
]

(* Two bytes: a zero length, then one byte. *)
let rd : reader = fun o w -> 0
let en : env = fun v -> if v = lim then 2 else 0
let no_o1 : opts = { cancel = false; drop = false }

(* The codec rejects; the walk the emitter writes reads outside the window. *)
let _ = assert_norm (spec 2 rd en 0 plan == Reject)
let _ = assert_norm (Fault? (exec no_o1 2 lim rd (start lim) en plan))

(* What the model proves for the planner's plans, applied to this one. *)
let scanner_safe ()
  : Lemma (~ (Fault? (exec no_o1 2 lim rd (start lim) en plan)))
  = ()
