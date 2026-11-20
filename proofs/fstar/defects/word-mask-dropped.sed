# mutates: Plan.Word.fst
# fails: Plan.Word.fst
#
# Not a past defect: Emitter.unsigned without the mask for a field that does
# not start its word, which then keeps the bytes before it.
s|  if a = k then v else ocaml_land v (pow2 (8 \* m) - 1)|  if true then v else ocaml_land v (pow2 (8 * m) - 1)|
