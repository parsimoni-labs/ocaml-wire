(** Lowering of the Wire DSL for its EverParse 3D projection.

    The declarations a struct needs beyond its own typedef (enum types, casetype
    dispatch, wrapper structs for nested and repeated elements) and the rewrites
    a field refinement goes through before 3D can verify it (where extraction,
    signed and float comparisons, narrow-width arithmetic, struct-level [where]
    placement). {!Pp_3d} prints the result; {!Types} re-exports the public part.
*)

open Dsl

val claim_name :
  (string, string) Hashtbl.t -> kind:string -> string -> string -> bool
(** [claim_name seen ~kind name shape] records that a declaration of [shape] is
    emitted under [name]: [true] the first time, [false] when the same [shape]
    claims [name] again. Raises [Invalid_argument] when a different [shape]
    does, since every 3D reference to [name] would resolve to the first. [kind]
    names the declarations in the message. *)

val split_string_casetype_fields : struct_ -> struct_
(** [split_string_casetype_fields s] rewrites each {!Dsl.constructor-Casetype}
    field of [s] whose tag is not an int-shaped typ into two adjacent byte
    fields: the tag bytes and a trailing {!Dsl.all_bytes} body. Used by the 3D
    projection so string-tagged casetype dispatch becomes caller code instead of
    parser code, matching how real protocol implementations like OpenSSH split
    the parse and dispatch steps. *)

val module_ : ?doc:string -> decl list -> module_
(** Build a module from declarations. *)

(** {1 Synthesised declarations} *)

val decl_name : decl -> string option
(** [decl_name d] is the name [d] declares, if it declares a named type. *)

val closed_enum_elem : 'a typ -> (string * packed_typ * int list) option
(** [closed_enum_elem t] is the name, base and case values of the closed enum
    [t] is, seen through the transparent wrappers; [None] for an open enum or a
    non-enum. Such an element projects through a synthesised refined-element
    struct so its membership is enforced. *)

val enum_elem_struct_name : string -> string
(** [enum_elem_struct_name name] is the name of the refined-element struct
    synthesised for the closed enum [name]. *)

val list_elem_pp :
  'a typ -> fallback:(Format.formatter -> unit) -> Format.formatter -> unit
(** [list_elem_pp elem ~fallback ppf] prints the byte-budget list element [elem]
    on [ppf]: a closed enum through its refined-element struct, anything else
    with [fallback]. *)

val repeat_elem_struct : 'a typ -> string option
(** [repeat_elem_struct elem] is the wrapper struct a self-delimiting [repeat]
    element with no named 3D type is wrapped in, if any. *)

val wrap_formals : 'a typ -> param list
(** [wrap_formals t] is the formals a synthesised wrapper over [t] declares, in
    first-occurrence order. *)

val synth_ref : string -> param list -> 'a typ
(** [synth_ref name formals] references the synthesised declaration [name],
    applied to the [formals] it lifted. *)

val single_elem_struct : 'a typ -> string option
(** [single_elem_struct t] is the wrapper struct a [nested] inner [t] is lifted
    into; [None] when [t] renders inline. *)

val optional_casetype_name : 'a typ -> string
(** [optional_casetype_name inner] is the gate-dispatch casetype synthesised for
    an [optional] over the self-delimiting [inner]. *)

val optional_casetype_args : 'a typ -> packed_expr list
(** [optional_casetype_args inner] is what a use site passes to
    {!optional_casetype_name} after the gate. *)

(** {1 Field refinements} *)

val extract_where_constraint : 'a typ -> bool expr option * 'a typ
(** [extract_where_constraint t] lifts every [where] in [t], including one under
    an [enum] or a [map], into a single constraint, and returns [t] without it.
*)

val combine_constraints :
  bool expr option -> bool expr option -> bool expr option
(** [combine_constraints a b] is the conjunction of the constraints present. *)

val scalar_kind : 'a typ -> [ `Signed of int | `Float | `Other ]
(** [scalar_kind t] is the scalar a refinement over a field of type [t] compares
    against, seen through the transparent wrappers. *)

val project_refinement :
  name:string ->
  kind:[ `Signed of int | `Float | `Other ] ->
  bool expr ->
  bool expr
(** [project_refinement ~name ~kind cond] rewrites a signed field's refinement
    to the two's-complement form its unsigned projection reads, and raises
    [Invalid_argument] for a float or signed 64-bit ordering, which has no
    faithful projection. *)

val widenable_unsigned : 'a typ -> bool
(** [widenable_unsigned t] is [true] when [t] is an unsigned scalar at most 32
    bits wide, so an [Add] over it can be widened without overflow. *)

val project_field_arith : string -> string list -> 'a expr -> 'a expr
(** [project_field_arith name widenable cond] rejects a field subtraction or
    multiplication in [cond] (no sound narrow-width projection), then widens
    each [Add] operand that names a field of [widenable]. *)

val lower_where_to_field_constraint :
  bool expr option -> field list -> bool expr option * field list
(** [lower_where_to_field_constraint where fields] moves a struct-level [where]
    that references fields onto the last field it references, since 3D's [where]
    sees only parameters. *)

val collect_refs : 'a expr -> string list
(** Field-reference names occurring in an expression, including nested terms. *)

val primitive_width : 'a typ -> int option
(** [primitive_width t] is the width in bytes of [t] when it is a primitive
    scalar, whose sized region lowers to a guarded scalar plus padding. *)

val optional_region : 'a typ -> bool
(** [optional_region t] is [true] when [t] is a single-element region, which
    under an optional stays behind a gate and a named wrapper rather than
    becoming a byte-budget list. *)

val primitive_region : 'a typ -> (int expr * packed_typ * int * bool) option
(** [primitive_region t] is the size, element, element width and at-most flag of
    [t] when it is a region over one primitive scalar. *)

val primitive_region_name : 'a typ -> string
(** [primitive_region_name t] is the name of the wrapper struct of the scalar
    region [t]. Raises [Invalid_argument] when [t] is not a single-element
    region. *)
