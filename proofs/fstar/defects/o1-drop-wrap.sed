# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: O1 without the premise that the span's end fits the
# native int (Emitter.end_range). With the bound left out, [e = p + n] is
# plain arithmetic that can wrap.
s|let implied = o.drop \&\& checks_next r \&\& nonneg \&\& Some? ends in|let implied = o.drop \&\& checks_next r \&\& nonneg in|
