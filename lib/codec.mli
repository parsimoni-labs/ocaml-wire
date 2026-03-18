(** Zero-copy record codecs for binary wire formats. *)

type ('a, 'r) field
(** A field specification for a value of type ['a] in a record of type ['r]. *)

type 'r t
(** A sealed record codec for type ['r]. *)

type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

val field : string -> 'a Types.typ -> ('r -> 'a) -> ('a, 'r) field
val view : string -> 'f -> ('f, 'r) fields -> 'r t
val wire_size : 'r t -> int
val min_wire_size : 'r t -> int
val wire_size_at : 'r t -> bytes -> int -> int
val is_fixed : 'r t -> bool
val decode : 'r t -> bytes -> int -> 'r
val encode : 'r t -> 'r -> bytes -> int -> unit
val to_struct : 'r t -> Types.struct_
val get : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a) Staged.t
val set : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a -> unit) Staged.t
val field_ref : ('a, 'r) field -> int Types.expr
