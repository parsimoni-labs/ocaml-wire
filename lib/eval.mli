(** Top-level expression evaluator and value-to-int conversion.

    The full struct-internal expression machinery (with [Ref]/[Sizeof_this]/
    [Field_pos] resolution against bound fields) lives in {!Codec} as the
    [compile_int_arr] family, which compiles expressions to [int array]
    accessors at codec construction. This module is the residual evaluator for
    the {!Wire.decode_string}/{!Wire.encode} paths, which only ever evaluate
    expressions in {!empty}: no field references, no cross-field dependencies.
*)

type ctx = unit
(** Empty context type. Cross-field state lives in [Codec]'s int-array
    machinery; at top level there is nothing to thread. *)

val empty : ctx

val int_of : 'a Types.typ -> 'a -> int option
(** [int_of typ v] converts a typed value to [int]. Returns [None] for types
    that don't fit in OCaml int (uint64 > 2^62, non-numeric). *)

val expr : ctx -> 'a Types.expr -> 'a
(** Evaluate a top-level expression. Raises on [Ref] (cross-field references are
    only valid inside a struct). [Sizeof_this] and [Field_pos] return 0. *)
