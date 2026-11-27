# mutates: Plan.Index.fst
# fails: Plan.Index.fst
# witness: Defect.IndexAheadUntested.Witness.fst
#
# Not a past defect: the pipelined [_index] loop of a fold loads the next
# frame's fields ahead before its test, whatever its answer, so a field is
# loaded from a frame that does not hold its bytes, or lies outside the
# buffer.
s|  Test j k :: (if k then loads j (ix.offs j) ah else \[\])|  loads j (ix.offs j) ah @ [ Test j k ]|
