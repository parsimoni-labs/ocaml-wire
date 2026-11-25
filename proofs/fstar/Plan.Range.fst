module Plan.Range

(* The bounds of a range of bitfields (Planner.word_range, Decision.Word_range):
   the code tests a word of [bits] bits once, [lo <= word <= hi], in place of
   the checks [f = c] of its top [width] bits and [g >= d] of the [width']
   bits right below. [range_ok] is the check make plans runs on the bounds
   each Word_range decision reports, with the fields' layout and checks from
   the plan; [range_correct] proves that bounds it accepts test exactly the
   two checks, for every word. *)

open Plan.Word

(* The bounds [lo] and [hi] are those of the two checks, for fields that
   stack in a word of [bits] bits narrower than OCaml's int. *)
let range_ok (bits shift width shift' width' c d lo hi:nat) : bool =
  shift' + width' = shift && shift + width = bits && 0 < width' && bits < 62 &&
  c < pow2 width && d < pow2 width' &&
  lo = c * pow2 shift + d * pow2 shift' && hi = (c + 1) * pow2 shift - 1

let range_correct (bits shift width shift' width' c d lo hi:nat) (word:nat)
  : Lemma (requires range_ok bits shift width shift' width' c d lo hi /\ word < pow2 bits)
          (ensures (view word shift width = c && view word shift' width' >= d) <==>
                   (lo <= word && word <= hi))
  = word_range word shift width shift' width' c d
