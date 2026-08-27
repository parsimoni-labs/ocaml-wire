(** Compiler from the {!Types.expr} GADT to closures.

    An expression appears in a codec in two very different places: as the size
    of a variable-width field, which has to be resolved against the bytes being
    parsed, and as a field constraint or [where] clause, which is resolved
    against the int slots the validator has already populated. Both need the
    same operator dispatch, the same 64-bit fallback and the same [Eq]/[Ne] type
    refinement; they differ only in how a leaf ([Ref], [Param_ref], [Sizeof],
    [Sizeof_this], [Field_pos]) is looked up.

    This module holds the shared half. It is parametric in the leaf resolution
    strategy ({!leaves}) and in the three context arguments that strategy
    consumes, so it never mentions buffers, slots or codec state. Compilation
    happens once, when a codec is sealed; the closure it returns is what runs
    per decode.

    {!Codec} supplies the two strategies and keeps them, since they are the part
    that does know about buffers and slots. *)

(** A parameter reference with its input/output kind hidden, so that leaf
    resolution can be a single non-polymorphic function. *)
type packed_param = Pack_param : ('a, 'k) Types.param_handle -> packed_param

type ('c1, 'c2, 'c3) leaves = {
  ref_ : string -> 'c1 -> 'c2 -> 'c3 -> int;
  i64 : string -> 'c1 -> 'c2 -> 'c3 -> int64;
  param_ref : packed_param -> 'c1 -> 'c2 -> 'c3 -> int;
  sizeof_typ : Types.packed_typ -> 'c1 -> 'c2 -> 'c3 -> int;
  sizeof_this : 'c1 -> 'c2 -> 'c3 -> int;
  field_pos : 'c1 -> 'c2 -> 'c3 -> int;
}
(** How to resolve each leaf form, for one access layer. The context is three
    curried arguments rather than one packed value so that a layer needing fewer
    than three passes immediate units and allocates nothing per call. *)

val compile_int :
  ('c1, 'c2, 'c3) leaves -> int Types.expr -> 'c1 -> 'c2 -> 'c3 -> int
(** [compile_int l e c1 c2 c3] is the value of [e] in the context [c1 c2 c3],
    with leaves resolved by [l]. The walk over [e] happens in the partial
    application [compile_int l e], so a caller stages that once and calls the
    result per decode. Raises [Invalid_argument] for a shape with no compilable
    form, such as a non-constant [Lsr64] shift. *)

val compile_bool :
  ('c1, 'c2, 'c3) leaves -> bool Types.expr -> 'c1 -> 'c2 -> 'c3 -> bool
(** [compile_bool l e c1 c2 c3] is {!compile_int} at [bool]. A comparison takes
    its operand width from the operands themselves, and a 64-bit one compares
    unsigned. *)
