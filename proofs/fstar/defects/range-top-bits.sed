# mutates: Plan.Range.fst
# fails: Plan.Range.fst
# witness: Defect.RangeTop.Witness.fst
#
# Not a past defect: a range of two bitfields that are not the top bits of
# their word: bits above the first field change the word but not the
# checks.
s|  shift' + width' = shift \&\& shift + width = bits \&\&|  shift' + width' = shift \&\& shift + width <= bits \&\&|
