(** Top-level expression evaluator.

    The full struct-internal expression machinery (with [Ref]/[Sizeof_this]/
    [Field_pos] resolution against bound fields) lives in {!Codec} as the
    [compile_int_arr] family, which compiles expressions to [int array]
    accessors at codec construction. This module is the residual evaluator for
    the {!Wire.of_string}/{!Wire.encode} paths, which only ever evaluate
    expressions in {!empty}: no field references, no cross-field dependencies.
*)

type ctx
(** Top-level evaluation context. Cross-field state lives in [Codec]'s int-array
    machinery, so at top level it binds no field. *)

val empty : ctx
(** Empty context, no bindings. *)

val int_of : 'a Types.typ -> 'a -> int option
(** {!Types.int_of}, re-exported: the conversion is a fold over the type
    description and is decided there. *)

val int_of_exn : 'a Types.typ -> 'a -> int
(** {!Types.int_of_exn}, re-exported. Used by the cross-field
    size/offset/present readers, where an unrepresentable value must fail the
    parse. *)

val expr : ctx -> 'a Types.expr -> 'a
(** [expr ctx e] evaluates a top-level expression. Raises on [Ref] (cross-field
    references are only valid inside a struct). [Sizeof_this] and [Field_pos]
    return 0. *)

val byte_test :
  elt_var:string ->
  cond:bool Types.expr ->
  (int -> bool) option Atomic.t ->
  int ->
  bool
(** [byte_test ~elt_var ~cond test b] is [true] when the byte [b] passes [cond],
    the per-byte refinement of a [byte_array_where] over the byte [elt_var].
    [cond] is compiled on its first use and kept in [test], the type's own cell,
    so a reader that walks the type on each call compiles it once. [cond] is
    computed at the width 3D gives it: on 64 bits, unsigned, where its value can
    leave the native int. *)

val first_bad : (int -> bool) -> bytes -> first:int -> len:int -> int
(** [first_bad test buf ~first ~len] is the index within
    [buf.[first .. first + len - 1]] of the first byte that fails [test], or
    [-1] when every byte passes. *)

val bad_byte :
  elt_var:string ->
  cond:bool Types.expr ->
  test:(int -> bool) option Atomic.t ->
  bytes ->
  first:int ->
  len:int ->
  int
(** [bad_byte ~elt_var ~cond ~test buf ~first ~len] is {!first_bad} of
    {!byte_test}: the index of the first byte of the span that fails [cond], the
    per-byte refinement of a [byte_array_where], or [-1]. Single source of truth
    for the refinement: the direct parser, the compiled codec and both encoders
    all decide it here, so they admit exactly the same spans as the EverParse
    validator built from the same schema. *)

val check_byte_refinement :
  elt_var:string ->
  cond:bool Types.expr ->
  test:(int -> bool) option Atomic.t ->
  string ->
  unit
(** [check_byte_refinement ~elt_var ~cond ~test s] raises [Invalid_argument] on
    the first byte of [s] that fails [cond], the per-byte refinement of a
    [byte_array_where]. Encode-side counterpart of the decode check, so both
    halves reject the same spans. It is staged as {!bad_byte} is. *)
