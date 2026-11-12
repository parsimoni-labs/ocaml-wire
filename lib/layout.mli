(** Wire sizes and C-level type names of the Wire DSL.

    The fixed size of a field type, the encoded size of a value, and the C and
    OCaml type names a field projects to in generated stubs. {!Types} re-exports
    this module. *)

open Dsl

val field_wire_size : 'a typ -> int option
(** Fixed wire size of a field type, if determinable. *)

val size_of_typ_value : eval_ctx -> 'a typ -> 'a -> int
(** [size_of_typ_value ctx typ v] is the encoded byte size of [v] under [typ],
    computed from the value rather than from a buffer. A size that names an
    input parameter resolves through [ctx]; one that names a sibling field or a
    position does not, since a value on its own does not carry the record around
    it, and a typ measured only by such a size raises [Invalid_argument] rather
    than reporting [0] for a field the encoder will fill. Also raises for a
    casetype value no case projects, which encoding refuses for the same reason.
*)

val c_type_of : 'a typ -> string
(** [c_type_of typ] returns the C type name (e.g., ["uint8_t"], ["uint32_t"]).
*)

val ml_type_of : 'a typ -> string
(** [ml_type_of typ] returns the OCaml type name for FFI stub generation:
    ["int"] for integer types that fit in OCaml integers, ["int64"] for uint64.
*)
