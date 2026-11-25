# mutates: Plan.Frames.fst
# fails: Plan.Frames.fst
#
# Not a past defect: the four-frame loop of fb3cb405 with its fourth base
# written as the third, [c + stride_2] twice, so a frame is walked twice and
# the one at [c + stride_3] never.
s|(c + 2 \* s) :: (c + 3 \* s) :: quad|(c + 2 * s) :: (c + 2 * s) :: quad|
