# mutates: Plan.Loop.fst
# fails: Plan.Loop.fst
# witness: Defect.ShortKnown.Witness.fst
#
# Not a past defect: the frame loop's test off by one, [stride + 1 < need]
# for [stride < need], so a frame one byte short of the leading fields is
# walked knowing all of them, and its last field is read past the frame.
s|let frame_known (need:nat) (s:int) : nat = if s < need then 0 else need|let frame_known (need:nat) (s:int) : nat = if s + 1 < need then 0 else need|
