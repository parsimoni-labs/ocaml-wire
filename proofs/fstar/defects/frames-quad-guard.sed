# mutates: Plan.Frames.fst
# fails: Plan.Frames.fst
#
# Not a past defect: the four-frame loop entered without fb3cb405's guard
# [stride <= len / 4], so [last_4 = off + len - 4 * stride] may lie before
# [off] and, for a stride near max_int, [4 * stride] wraps.
s|  : Lemma (requires buflen <= max_int /\\ guard buflen off len s /\\ s <= len / 4)|  : Lemma (requires buflen <= max_int /\\ guard buflen off len s /\\ in_int s)|
