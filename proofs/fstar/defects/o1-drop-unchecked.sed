# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: O1 leaving a span's bound to the next node whatever it
# is, as if Emitter.checks_next answered true for a where clause or the end
# of the plan, which check no bytes.
s|let implied = o.drop \&\& checks_next r \&\& nonneg \&\& Some? ends in|let implied = o.drop \&\& nonneg \&\& Some? ends in|
