(** Wire sizes and C-level type names of the Wire DSL.

    The fixed size of a field type, the encoded size of a value, and the C and
    OCaml type names a field projects to in generated stubs. {!Types} re-exports
    this module. *)

open Dsl

val array : len:int expr -> 'a typ -> 'a list typ
(** Fixed-count array of elements. *)

val array_seq : ('a, 'seq) seq_map -> len:int expr -> 'a typ -> 'seq typ
(** Fixed-count array with custom builder. *)

val size_of_typ_value :
  ?field:(string -> int option) -> eval_ctx -> 'a typ -> 'a -> int
(** [size_of_typ_value ?field ctx typ v] is the encoded byte size of [v] under
    [typ], computed from the value rather than from a buffer. A size that names
    an input parameter resolves through [ctx], and one that names a sibling
    field through [field], which the codec holding [v] supplies from the record
    being encoded (default: no field resolves). A position never resolves, and a
    typ measured only by a size that does not resolve raises [Invalid_argument]
    rather than reporting [0] for a field the encoder will fill. Also raises for
    a casetype value no case projects, which encoding refuses for the same
    reason. *)

val sized_by_field : 'a typ -> bool
(** [sized_by_field typ] is [true] when {!size_of_typ_value} may read a size of
    [typ] that is not a literal, and so may need [?field]. *)

val c_type_of : 'a typ -> string
(** [c_type_of typ] returns the C type name (e.g., ["uint8_t"], ["uint32_t"]).
*)

val ml_type_of : 'a typ -> string
(** [ml_type_of typ] returns the OCaml type name for FFI stub generation:
    ["int"] for integer types that fit in OCaml integers, ["int64"] for uint64.
*)
