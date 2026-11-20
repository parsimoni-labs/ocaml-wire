# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
# witness: Defect.PrefixWrap.Witness.fst
#
# The emitter before 1883d145: the bytes a run of prefixes needs, added to
# the position's offset at generation time with OCaml's wrapping +, rather
# than Planner.extent, which stops past any buffer.
s/if x < extent w.p.at need then/if x < wrap (w.p.at + need) then/
