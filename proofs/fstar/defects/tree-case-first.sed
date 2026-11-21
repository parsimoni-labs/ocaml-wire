# mutates: Plan.Tree.fst
# fails: Plan.Tree.fst
# witness: Defect.CaseFirst.Witness.fst
#
# Not a past defect: a leaf may run a case where a condition before it also
# holds, as a tree that tested the cases out of order would: the codec runs
# the earlier one.
s|    else j < i \&\& decided r c = Some false \&\& leaf_ok r rest x (j + 1)|    else j < i \&\& leaf_ok r rest x (j + 1)|
