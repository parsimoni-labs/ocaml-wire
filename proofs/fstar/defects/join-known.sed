# mutates: Plan.Join.fst
# fails: Plan.Join.fst
#
# Not a past defect: the join keeps the bytes the walk before the cases
# knew, though its position is where a case ended, past some of them.
s|  { w1 with sc = extend w1.sc e; known = 0; p = { pv = Some e; at = 0 } }|  { w1 with sc = extend w1.sc e; p = { pv = Some e; at = 0 } }|
