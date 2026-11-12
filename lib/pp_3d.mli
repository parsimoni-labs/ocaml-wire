(** EverParse 3D pretty-printer for the Wire DSL.

    Renders expressions, types, fields, structs and declarations of {!Dsl} as 3D
    source, after the rewrites of {!Lower_3d}. {!Types} re-exports it. *)

open Dsl

val pp_expr : Format.formatter -> 'a expr -> unit
(** Pretty-print an expression. *)

val pp_typ : Format.formatter -> 'a typ -> unit
(** Pretty-print a type. *)

val pp_action : Format.formatter -> action -> unit
(** Pretty-print an action block. *)

val pp_module : Format.formatter -> module_ -> unit
(** Pretty-print a module as 3D source. *)

val to_3d : ?enum_as_type:bool -> module_ -> string
(** Render a module as a 3D source string. With [~enum_as_type:true] (the
    documentation projection) an enum field renders as its named 3D enum type
    rather than the base type plus a membership refinement. *)

val to_3d_file : ?enum_as_type:bool -> string -> module_ -> unit
(** [to_3d_file path m] writes module [m] to a [.3d] file at [path]. See
    {!to_3d} for [enum_as_type]. *)

val escape_3d : string -> string
(** [escape_3d name] appends [_] if [name] is a 3D or C reserved word. *)

val inner_wire_size : 'a typ -> int option
(** [inner_wire_size t] is the fixed byte width of a scalar, bitfield base, unit
    or fixed-size sub-codec, seen through [Map] / [Enum] / [Where]; [None] for
    every other shape. *)

val index_bound_elt : 'a typ -> (string * bool expr) option
(** [index_bound_elt elem] is [Some (elt_var, cond)] when [elem] is a 1-byte
    array / repeat element carrying a lookup index bound: it then projects like
    a {!Dsl.byte_array_where} whose synthesised element struct is named
    [synth_name_of_elt_var elt_var] and refines its byte with [cond]. Internal:
    used by the EverParse projection. *)
