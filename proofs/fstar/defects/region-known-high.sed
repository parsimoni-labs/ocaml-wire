# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: a region's window known to hold the high end of its
# size's range rather than the low one (95f307ce), so a region of 2 to 8
# bytes reads its first prefix of 8 unchecked.
s|           let lo = fst (region_range w1 n) in|           let lo = snd (region_range w1 n) in|
