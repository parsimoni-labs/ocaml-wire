# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: a region's start split into a term and a constant
# that leaves the position's own constant out of the displacement
# (b633f225), so the region's loads land that many bytes early.
s|then (e, s.const + p.at) else (pos_expr p, 0)|then (e, s.const) else (pos_expr p, 0)|
