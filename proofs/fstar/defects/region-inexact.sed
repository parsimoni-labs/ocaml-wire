# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: an exact region ([nested], not [nested_at_most]) closed
# without checking its contents used all of it.
s|       match (if am then Some false else|       match (if true then Some false else|
