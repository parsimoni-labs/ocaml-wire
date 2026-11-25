# mutates: Plan.Scan.fst
# fails: Plan.Proof.fst
# witness: Defect.HeldLeak.Witness.fst
#
# Emitter.step entering a region without clearing [held]: the words loaded
# at constant offsets of the window around answer for the same offsets of
# the region, which count from its own start. A field at offset 0 of a
# region at 8 is taken from the byte at 0 of the buffer.
s|known = lo; bounded = \[\]; held = \[\] } in|known = lo; bounded = [] } in|
