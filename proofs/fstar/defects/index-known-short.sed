# mutates: Plan.Index.fst
# fails: Plan.Index.fst
# witness: Defect.IndexKnownShort.Witness.fst
#
# Not a past defect: the test of a frame of the index without
# [stop >= need], so a frame that lies in the buffer but is shorter than the
# leading fields is walked knowing all of them, and its fields are loaded
# ahead past its end.
s|let known (length:int) (need:nat) (b s:int) : bool = b >= 0 \&\& s >= need \&\& s <= length - b|let known (length:int) (need:nat) (b s:int) : bool = b >= 0 \&\& s <= length - b|
