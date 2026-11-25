# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: O1 without the premise that the span's size is proved
# not negative. A negative size moves the position back, so the next check
# passes for a span the codec rejects.
s|let implied = o.drop \&\& checks_next r \&\& nonneg \&\& Some? ends in|let implied = o.drop \&\& checks_next r \&\& Some? ends in|
