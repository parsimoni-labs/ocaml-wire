# mutates: Plan.Values.fst
# fails: Plan.Values.fst
#
# Not a past defect: the sign extension of an int64 written with the shift
# of a 63-bit int, [Sys.int_size - w]: the field's sign bit is not moved to
# the word's, and a negative field reads positive.
s|shift_right64 (shift_left64 x (64 - w)) (64 - w)$|shift_right64 (shift_left64 x (if w < 64 then 63 - w else 0)) (if w < 64 then 63 - w else 0)|
