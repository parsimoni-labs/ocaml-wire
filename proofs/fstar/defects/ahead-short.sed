# mutates: Plan.Loop.fst
# fails: Plan.Loop.fst
# witness: Defect.AheadShort.Witness.fst
#
# Not a past defect: the fields loaded ahead of each frame's walk also in
# the loop over frames shorter than the leading fields, which does not know
# their bytes are there: a field of the leading bytes is loaded past the
# frame.
s|  if frames = 4 \&\& not (s < need) then map (fun (kw:nat \& nat)|  if frames = 4 then map (fun (kw:nat \& nat)|
