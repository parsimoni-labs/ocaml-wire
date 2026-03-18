(** Typed parameter handles.

    Input parameters carry their value. Output parameters carry a mutable cell
    that actions write to during decoding. Both are passed as regular OCaml
    function arguments — no separate environment needed. *)

type input
type output
type ('a, 'k) t

val input : string -> 'a Types.typ -> 'a -> ('a, input) t
(** [input name typ value] creates an input parameter bound to [value]. *)

val output : string -> 'a Types.typ -> ('a, output) t
(** [output name typ] creates an output parameter with an internal mutable cell,
    initially zero. Actions update it during decoding. *)

val v : ('a, 'k) t -> Types.param
(** Formal declaration for codecs and 3D structs. *)

val name : ('a, 'k) t -> string
(** Parameter name. *)

val get : ('a, 'k) t -> 'a
(** Read the current value of a parameter. For output params, call this after
    decoding to observe action results. *)

val set : ('a, 'k) t -> 'a -> unit
(** Set the value of a parameter. *)

type packed = Pack : ('a, 'k) t -> packed

val to_ctx : packed list -> (string * int) list
(** Export bindings as name-value pairs for the eval context. *)

val store_name : packed list -> string -> int -> unit
(** Update a parameter by name (used by action execution). *)
