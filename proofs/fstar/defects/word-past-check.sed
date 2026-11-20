# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
# witness: Defect.WordPast.Witness.fst
#
# Emitter.shared_words without [a + 8 <= hi]: a word kept though it ends
# past the bytes the last check covered. On a prefix of two bytes at the
# end of a buffer of two, the word loads eight.
s|if keep a off wd \&\& a + 8 <= hi \&\& a <= off|if keep a off wd \&\& a <= off|
