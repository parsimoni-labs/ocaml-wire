# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
#
# The premise Defect.PrefixWrap shows the planner breaks at b1f6f756: a run
# of prefixes no longer than Sys.max_string_length. Without it the constant
# the emitter adds, [p.at + need], may wrap.
s|    ahead l <= max_string_length \&\& fields_ok sc fs size|    fields_ok sc fs size|
