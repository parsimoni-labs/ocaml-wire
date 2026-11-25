module Defect.ConstraintWrap.Buggy

(* e976e0cd "ocaml: check constraint arithmetic as the codec does", before
   the fix: Wire_ocaml wrote the [+] of a constraint as a plain, wrapping
   addition where the codec checks it and rejects on overflow. The Overflow
   schema of test/ocaml constrains a uint32 [A] by [A + (max_int - 5) <> 0]:
   for [A = 6] the sum wraps to [min_int], the generated validate accepts and
   the codec rejects. *)

open Plan.Expr

let wrap (x:int) : int = ((x + pow2 62) % pow2 63) - pow2 62

let near_max : int = max_int - 5

(* The generated test, plain arithmetic. *)
let generated_accepts (a:nat{a < pow2 32}) : bool = wrap (a + near_max) <> 0

(* The codec's test: checked arithmetic, overflow rejects. *)
let codec_accepts (a:nat{a < pow2 32}) : bool =
  match chk (a + near_max) with Some x -> x <> 0 | None -> false

let _ = assert_norm (generated_accepts 6 = true /\ codec_accepts 6 = false)

(* The generated code answers as the codec does. *)
let agrees (a:nat{a < pow2 32}) : Lemma (generated_accepts a == codec_accepts a) = ()
