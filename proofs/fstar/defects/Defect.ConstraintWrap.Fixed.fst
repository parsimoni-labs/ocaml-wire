module Defect.ConstraintWrap.Fixed

(* e976e0cd after the fix: the generated test uses Wire_runtime.Arith.add,
   which raises [Overflow] where the sum leaves the native int, and the
   overflow rejects the record. *)

open FStar.Mul
open Plan.Expr

let near_max : int = max_int - 5

let generated_accepts (a:nat{a < pow2 32}) : bool =
  match chk (a + near_max) with Some x -> x <> 0 | None -> false

let codec_accepts (a:nat{a < pow2 32}) : bool =
  match chk (a + near_max) with Some x -> x <> 0 | None -> false

let agrees (a:nat{a < pow2 32}) : Lemma (generated_accepts a == codec_accepts a) = ()
