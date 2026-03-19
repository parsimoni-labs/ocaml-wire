(** Typed parameter handles.

    Input parameters are set before decoding with {!init}, which returns the
    corresponding expression. Output parameters are mutable cells written by
    actions during decoding, read back with {!get}. *)

type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

val input : string -> 'a Types.typ -> ('a, input) t
(** [input name typ] declares an input parameter. *)

val output : string -> 'a Types.typ -> ('a, output) t
(** [output name typ] declares an output parameter (mutable cell, initially 0).
*)

val v : ('a, 'k) t -> Types.param
(** Formal declaration for 3D rendering. *)

val name : ('a, 'k) t -> string

val get : ('a, 'k) t -> 'a
(** Read the current value. *)

val set : ('a, output) t -> 'a -> unit
(** Set the value of an output parameter. *)

val init : ('a, input) t -> 'a -> int Types.expr
(** [init p v] sets the input param to [v] and returns its expression. *)

val expr : ('a, 'k) t -> int Types.expr
(** [expr p] returns the expression referencing this param. *)

type packed = Pack : ('a, 'k) t -> packed
