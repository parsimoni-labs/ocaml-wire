(* The generator modules, each with one job, composed back into the one
   interface the fuzz executables use. *)

include Gen
include Leaves
include Wrappers
include Record
include Compose
include Properties
include Cases
include Curated
include File_input
include Codec_sampler
include Invariants
include Api_checks
include Projection
include Entry_points
