module Defect.RepeatBudget.Buggy

(* 787089f1 "codec: make validate reject a repeat's partial element", before
   the fix: Codec.validate reached a repeat only through its populate, which
   was [no_populate], so a byte budget that does not split into whole
   elements was accepted where decode and EverParse reject it. *)

open FStar.Mul

(* Decode and EverParse: elements of [k] bytes consume the budget exactly. *)
let rec consumes (budget:nat) (k:pos) : Tot bool (decreases budget) =
  if budget = 0 then true else if budget < k then false else consumes (budget - k) k

let validate (budget:nat) (k:pos) : bool = true

let _ = assert_norm (validate 3 2 = true /\ consumes 3 2 = false)

(* validate fails exactly when decode does. *)
let one_answer (budget:nat) (k:pos) : Lemma (validate budget k == consumes budget k) = ()
