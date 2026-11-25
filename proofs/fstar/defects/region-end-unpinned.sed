# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: an exact region closed untested where its contents end
# at the low end of its size's range, though the range does not pin the
# size there (95f307ce takes both ends).
s|let ends_exact (r:int \& int) (p:pos) : bool = None? p.pv \&\& r = (p.at, p.at)|let ends_exact (r:int \& int) (p:pos) : bool = None? p.pv \&\& fst r = p.at|
