(** Formal parameters for codecs.

    Input parameters are bound before decoding with {!bind}. Output parameters
    are written by actions during decoding, read back with {!get}. *)

type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

val input : string -> 'a Types.typ -> ('a, input) t
(** [input name typ] declares an input parameter. *)

val output : string -> 'a Types.typ -> ('a, output) t
(** [output name typ] declares an output parameter (initially 0). *)

val decl : ('a, 'k) t -> Types.param
(** Project to an untyped formal declaration (for 3D rendering). *)

val name : ('a, 'k) t -> string

val expr : ('a, 'k) t -> int Types.expr
(** [expr p] returns the expression referencing this param. *)

(** {1 Parameter environment} *)

type env = Types.param_env
(** Immutable parameter environment backed by a flat [int array]. Create one
    with {!Codec.env}, bind inputs with {!bind}, read outputs with {!get}. *)

val bind : ('a, input) t -> 'a -> env -> env
(** [bind p v env] returns an environment with input [p] set to [v]. *)

val get : env -> ('a, 'k) t -> 'a
(** [get env p] reads the current value of param [p] from [env]. For output
    params, call after {!Codec.decode}. *)

type packed = Pack : ('a, 'k) t -> packed
