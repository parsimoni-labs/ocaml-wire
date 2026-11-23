(** The integer arithmetic of a struct's expressions, as 3D types and verifies
    it.

    Every integer subexpression of a constraint, size or presence condition has
    a 3D type and a range of values (docs/projection.md, Arithmetic). The
    functions below compute them, refuse with [Invalid_argument] what 3d.exe
    cannot verify or what the C would compute differently from OCaml, and return
    the expression the projection emits: an addition, multiplication or shift
    whose range needs a wider type has its operands cast to it, and a comparison
    of a signed field with a literal is in two's-complement form. *)

open Dsl

type env
(** The fields a struct's expressions read, and what the refinements and
    conditions before an expression prove of them. *)

val empty : env
(** [empty] is the environment of a body that names no field. *)

val envs :
  ?formals:string list -> ?where:bool expr -> field list -> env * env list
(** [envs ~formals ~where fields] is the environment of the struct's [where]
    and, for each of [fields], the environment its expressions are checked in:
    every field's type, [where], and the refinements of the fields before it.
    [formals] are the parameters of a synthesised declaration, through which the
    struct that applies it passes values it has checked; an expression over one
    projects as written. *)

val bounded : env -> field -> field
(** [bounded env f] is [f], or, when [f] is an optional region whose size
    verifies only under the bounds [env] proves of the fields it reads, [f] with
    its region under a [Where] stating them: the region's declaration states it
    as its [where], and its name records it. *)

val refinement : env -> name:string -> 'a typ -> bool expr -> bool expr
(** [refinement env ~name t cond] is the refinement 3D checks for [cond] on the
    field [name] of type [t]. *)

val condition : env -> bool expr -> bool expr
(** [condition env cond] is [cond] as 3D checks it. *)

val field : env -> field -> field
(** [field env f] is [f] with its sizes and presence conditions as 3D checks
    them. Its refinement is left to {!refinement}. *)

val wide : (string -> packed_typ option) -> int expr -> bool
(** [wide typ_of e] is [true] when OCaml computes [e] on 64 bits, as unsigned
    integers, rather than in its native int: [e] reads a field the native int
    cannot hold, or it or one of its subexpressions can take a value past
    [max_int] or shifts by [Sys.int_size] or more. 3D computes it at a type that
    holds each of those values, so the two agree. [typ_of n] is the type of the
    field [n]. [e] is an expression of a struct {!envs} has checked: what its
    refinements and conditions prove is not known here, so a value it bounds is
    bounded by its type instead. [false] when [e] reads a name [typ_of] does not
    know. *)
