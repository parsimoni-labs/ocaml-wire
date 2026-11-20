module Defect.RepeatBudget.Fixed

(* 787089f1 after the fix: the repeat's populate runs its reader, whose
   budget check is [budget mod k = 0]. *)

open FStar.Mul

let rec consumes (budget:nat) (k:pos) : Tot bool (decreases budget) =
  if budget = 0 then true else if budget < k then false else consumes (budget - k) k

let validate (budget:nat) (k:pos) : bool = budget % k = 0

let rec one_answer (budget:nat) (k:pos)
  : Lemma (ensures validate budget k == consumes budget k) (decreases budget)
  = if budget = 0 || budget < k then FStar.Math.Lemmas.small_mod budget k
    else begin
      FStar.Math.Lemmas.lemma_mod_sub budget k 1;
      one_answer (budget - k) k
    end
