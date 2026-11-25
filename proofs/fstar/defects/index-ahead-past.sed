# mutates: Plan.Decide.fst
# fails: Plan.Index.fst
# witness: Defect.IndexAheadPast.Witness.fst
#
# Not a past defect: make plans accepts a field loaded ahead that starts in
# the bytes the frame loop knows but ends past them, so the pipelined loop
# of a fold's [_index] reads past the bytes its test ensures.
s|  match ah with \[\] -> true \| (k, wd) :: r -> k + wd <= need \&\& ahead_in need r|  match ah with [] -> true \| (k, wd) :: r -> k < need \&\& ahead_in need r|
