# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# Not a past defect: f0bd4b6b's rule applied to a region whose contents
# load a byte, so its bound is left to the check after it though the
# contents read their fields first, past the window when the region runs
# past it.
s|  \| Prefix \[\] _ :: r -> unread_nodes r|  \| Prefix _ _ :: r -> unread_nodes r|
