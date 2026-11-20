# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: a region opened without checking its size against the
# window around it, so its contents can borrow bytes from beyond it.
s|match (if mem (s, w.p) w.bounded then Some false else outside o w lim en s ov nonneg k) with|match (if true then Some false else outside o w lim en s ov nonneg k) with|
