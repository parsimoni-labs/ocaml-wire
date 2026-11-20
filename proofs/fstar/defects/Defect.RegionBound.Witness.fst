module Defect.RegionBound.Witness

(* The plan of the codec that shows the region defect: a tag and a pad byte,
   then one of two cases, each a region of at most 4 bytes holding two
   bytes and a region of at most 4 bytes holding a 32-bit word. The inner
   region needs 6 bytes of the outer one's 4. On six zero bytes the codec
   rejects; the emitter that keeps the dispatch's bounds in the region
   accepts, its innermost window reaching 2 bytes past the outer one. This
   module only holds against the emitter of region-bounded-leak.sed. *)

open Plan.Expr
open Plan.Scan
open Plan.Proof

let lim : var = 0

let case (x y n2 e2 l n e:var) : list node = [
  Region n e (Lit 4) true [
    Prefix [ { id = x; off = 0; width = 1 }; { id = y; off = 1; width = 1 } ] 2;
    Region n2 e2 (Lit 4) true [ Prefix [ { id = l; off = 0; width = 4 } ] 4 ];
  ];
]

let plan : list node = [
  Prefix [ { id = 1; off = 0; width = 1 }; { id = 2; off = 1; width = 1 } ] 2;
  Gate 1 [ (0, case 3 4 5 6 7 8 9) ] (Some (case 10 11 12 13 14 15 16));
]

let rd : reader = fun _ _ -> 0
let en : env = fun v -> if v = lim then 6 else 0
let o : opts = plain

let _ = assert_norm (wf (start lim).sc plan)
let _ = assert_norm (spec rd 0 6 [] en 0 plan == Reject)
let _ = assert_norm (Accept? (exec o rd (top 6 lim) (start lim) en plan))
