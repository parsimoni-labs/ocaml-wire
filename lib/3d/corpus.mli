(** The oracle half of the standalone pipeline's differential self-check.

    The standalone projection emits a validator-only C parser with no FFI, so
    nothing otherwise confirms that the generated validator accepts exactly the
    inputs the OCaml codec accepts: a bug in the projection (wrong bit order, a
    constraint that means something different over the wire type) would pass
    unnoticed. {!v} prints inputs tagged with the OCaml verdict, and [Wire_3d]'s
    [agree.c] runs the validator on each and fails on any disagreement. *)

val v : count:int -> Format.formatter -> 'a Wire.Codec.t -> unit
(** [v ~count ppf c] prints [count] inputs of [c] as
    [<codec> <params> <hex> <verdict>] lines, [verdict] [1] when [c] accepts the
    bytes as a whole and [0] otherwise. The inputs are accepting seeds, boundary
    inputs around each constrained field's values, mutants of the seeds and
    uniform bytes. Raises [Failure] naming the codec when the inputs land on one
    side of the accept/reject boundary only. *)
