# mutates: Plan.Word.fst
# fails: Plan.Word.fst
#
# Not a past defect: Planner.word_range's [lo] without the premise that the
# lower field's bound fits below the upper field ([d < 2^width'] in the
# planner). Overlapping bits make the [lor] smaller than the sum.
/0 < shift \/\\ shift < 62/s| d < pow2 (shift - shift') /\\||
