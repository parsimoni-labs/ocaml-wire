# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# 4da4e8ad "codec: bound-check field reads on the validate path", before the
# fix: validate reached the fields after a variable-size one at the offsets
# the layout computed, behind the record's fixed-size check only. Here: the
# bytes known after a span stay those the frame check gave, instead of none.
s|    known = 0;|    known = w.known;|
