(** Struct validators: a {!Types.struct_} checked without a record constructor,
    through the same int-array validation kernel as a codec's {!Codec.validate},
    with the writer and reader projections left out. Internal to {!Codec}. *)

type t
(** A compiled struct validator. *)

val v : Types.struct_ -> t
(** [v s] compiles [s] into a reusable validator. Constraints, [where] clauses
    and per-field actions run over a per-decode int array. *)

val validate : t -> bytes -> int -> unit
(** [validate t buf off] checks the struct at [off]. Raises {!Types.Parse_error}
    on failure. *)

val size_of : t -> bytes -> int -> int
(** [size_of t buf off] is the byte size of the struct at [off]. *)

val min_size : t -> int
(** [min_size t] is the minimum byte size accepted. *)

val wire_size_info : t -> [ `Fixed of int | `Variable of bytes -> int -> int ]
(** [wire_size_info t] is [`Fixed n] for a struct of [n] bytes, else
    [`Variable f] where [f buf off] is the end offset of the struct at [off]. *)
