# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: an exact region ([nested], not [nested_at_most]) closed
# without checking its contents used all of it.
s|       match (if am \|\| ends_exact f.frg w.p then Some false else|       match (if true then Some false else|
