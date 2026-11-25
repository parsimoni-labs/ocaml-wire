# mutates: Plan.Tree.fst
# fails: Plan.Tree.fst
#
# Not a past defect: the check of a dispatch's tree no longer refuses a
# path that tests an atom it tested before, so a tree may evaluate one
# condition twice.
s|  \| If a y n -> None? (assoc a r) \&\& tree_ok|  \| If a y n -> tree_ok|
