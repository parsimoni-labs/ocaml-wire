(* The checksum of the frames of a corpus through the fold [checksum] of
   g_selected: one frame, or the frames of a stride, which the rows pass as a
   literal so that it stays a constant once inlined. On OxCaml the batch is
   checked to allocate nothing where the fold is inlined, under the bench
   profile. *)

let[@inline always] fold base buf off len =
  Int64.to_int (G_selected.checksum buf off len ~init:(Int64.of_int base))

let[@inline always] [@zero_alloc opt] batch ~stride base buf off len =
  Int64.to_int
    (G_selected.checksum_frames buf off len ~stride ~init:(Int64.of_int base))
