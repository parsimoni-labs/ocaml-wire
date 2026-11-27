(** The one Rust module of a codec, whatever its shape.

    Its parts are written from {!Wire_ocaml.Plan}: the check and the fold from
    the codec's plan, the views, values and errors from the plan of each record
    it reaches. Internal to {!Wire_rust}. *)

val rs : codec:string -> ?fold:string list -> 'r Wire.Codec.t -> string
(** [rs ~codec ?fold c] is the source of the module of [c], [codec] naming it in
    the module's header, with a fold of the fields [fold] names by their paths,
    as {!Wire_ocaml.v} names them. Raises [Invalid_argument] naming the reason
    for a codec the walk does not cover, a path no fold reads, an expression
    Rust cannot spell, a name that is not UTF-8, and two names that would be one
    Rust name. *)
