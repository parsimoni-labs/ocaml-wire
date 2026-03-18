(** Typed parameter handles and runtime environments. *)

type input
type output
type ('a, 'k) t
type env

val input : string -> 'a Types.typ -> ('a, input) t
(** Create an immutable input parameter handle. *)

val output : string -> 'a Types.typ -> ('a, output) t
(** Create a mutable output parameter handle. *)

val v : ('a, 'k) t -> Types.param
(** Extract the formal declaration from a typed handle. *)

val empty : env
(** Empty runtime parameter environment. *)

val is_empty : env -> bool
(** [true] iff no parameters are bound. *)

val bind : env -> ('a, input) t -> 'a -> env
(** Bind an input parameter to a value. *)

val init : env -> ('a, output) t -> 'a -> env
(** Initialise an output parameter to a value. *)

val get : env -> ('a, 'k) t -> 'a
(** Read back the current value of a bound parameter. *)

val to_ctx : env -> (string * int) list
(** Export bindings as name-value pairs for the decode context. *)

val store_name : env -> string -> int -> unit
(** Update a parameter by name (used by action execution). *)
