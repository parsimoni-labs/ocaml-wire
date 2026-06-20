(** Formal parameters for codecs.

    Input parameters are bound before decoding with {!bind}. Output parameters
    are written by actions during decoding, read back with {!get}. *)

type input = Types.param_input
type output = Types.param_output
type ('a, 'k) t = ('a, 'k) Types.param_handle

val pp : Format.formatter -> ('a, 'k) t -> unit
(** Pretty-print a parameter by name. *)

val input : string -> 'a Types.typ -> ('a, input) t
(** [input name typ] declares an input parameter. *)

val output : string -> 'a Types.typ -> ('a, output) t
(** [output name typ] declares an output parameter (initially 0). *)

val decl : ('a, 'k) t -> Types.param
(** Project to an untyped formal declaration (for 3D rendering). *)

val name : ('a, 'k) t -> string
(** [name p] is the formal parameter name. *)

val expr : ('a, 'k) t -> int Types.expr
(** [expr p] returns the expression referencing this param. *)

(** {1 Parameter environment} *)

type env = Types.param_env
(** Immutable parameter environment backed by a flat [int array]. Create one
    with {!Codec.env}, bind inputs with {!bind}, read outputs with {!get}. *)

val bind : ('a, input) t -> 'a -> env -> env
(** [bind p v env] returns an environment with input [p] set to [v]. *)

val bind_by_name : string -> int -> env -> env
(** [bind_by_name name v env] binds the input parameter called [name] to the
    integer value [v] without needing its typed handle, for tooling that has the
    codec but not the {!t} handles (e.g. the differential harness binding params
    generically). A no-op if the codec does not reference [name]. *)

val get : env -> ('a, 'k) t -> 'a
(** [get env p] reads the current value of param [p] from [env]. For output
    params, call after {!Codec.decode}. *)

type packed = Pack : ('a, 'k) t -> packed
