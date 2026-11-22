# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
# witness: Defect.RegionBound.Witness.fst
#
# Found by this model at b1f6f756: Emitter.step walks a region's contents
# with the bounds a dispatch before it checked, keyed by size and position,
# though positions in the region count from its own start. A span or region
# of the same size at the same offset inside then skips its check. The
# witness runs the plan of the counterexample codec: outer region of 4 bytes
# at 2, inner region of 4 bytes at 2 inside it.
s|known = lo; bounded = \[\]; held = \[\] } in|known = lo; held = [] } in|
