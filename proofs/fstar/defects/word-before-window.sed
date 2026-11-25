# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
# witness: Defect.WordBefore.Witness.fst
#
# Emitter.shared_words without [max lo]: a word moved back to end at the
# last checked byte may start before the window, or before a prefix at a
# position known only at run time. On a buffer of two bytes the word
# starts six bytes before it.
s|let a = imax lo (imin off (hi - 8)) in|let a = imin off (hi - 8) in|
