(** EverParse 3D export derived from wire descriptions.

    The main path is {!struct_of_codec}, {!schema}, and {!write_3d}. For unusual
    3D constructs that have no codec equivalent yet, use {!Raw}. *)

type t = { name : string; module_ : Types.module_; wire_size : int }
(** A named 3D schema with its module and wire size. *)

type struct_ = Types.struct_
type decl = Types.decl
type decl_case = Types.decl_case
type module_ = Types.module_

val struct_of_codec : 'r Codec.t -> struct_
(** Project a codec to a 3D struct. *)

val schema_of_struct : struct_ -> t
(** Build a one-struct schema from a raw struct description.

    This uses the same EverParse output-types pattern as {!schema}: named fields
    get [WireSet*] extraction callbacks, while anonymous fields remain
    validation-only. *)

val schema : 'r Codec.t -> t
(** Builds a one-struct schema from a codec. The resulting module contains a
    single entrypoint typedef with the EverParse output-types pattern: extern
    callbacks ([WireSet*]) that extract all field values during validation. *)

val write_3d : outdir:string -> t list -> unit
(** [write_3d ~outdir ts] writes one [.3d] file per schema in [outdir]. *)

module Raw : sig
  type nonrec struct_ = struct_
  type field = Field.packed
  type nonrec decl = decl
  type nonrec decl_case = decl_case
  type nonrec module_ = module_
  type nonrec t = t

  val typedef :
    ?entrypoint:bool ->
    ?export:bool ->
    ?output:bool ->
    ?extern_:bool ->
    ?doc:string ->
    struct_ ->
    decl
  (** Top-level typedef declaration. *)

  val define : string -> int -> decl
  (** Top-level integer definition. *)

  val extern_fn : string -> Types.param list -> 'a Types.typ -> decl
  (** External function declaration used by 3D actions. *)

  val extern_probe : ?init:bool -> string -> decl
  (** External probe declaration. *)

  val enum_decl : string -> (string * int) list -> 'a Types.typ -> decl
  (** Top-level enum declaration. *)

  val decl_case : int -> 'a Types.typ -> decl_case
  (** One tagged case in a top-level casetype declaration. *)

  val decl_default : 'a Types.typ -> decl_case
  (** Default case in a top-level casetype declaration. *)

  val casetype_decl :
    string -> Types.param list -> 'a Types.typ -> decl_case list -> decl
  (** Top-level casetype declaration. *)

  val module_ : ?doc:string -> decl list -> module_
  (** Build a 3D module from declarations. *)

  val to_3d : module_ -> string
  (** Render a 3D module to text. *)

  val to_3d_file : string -> module_ -> unit
  (** Write a rendered 3D module to a file. *)

  val field :
    string ->
    ?constraint_:bool Types.expr ->
    ?action:Action.t ->
    'a Types.typ ->
    field
  (** Named field returning a packed field value. *)

  val anon_field : 'a Types.typ -> field
  (** Anonymous field in a 3D struct. *)

  val field_ref : field -> int Types.expr
  (** Expression referencing a named field by name. *)

  val struct_ : string -> field list -> struct_
  (** Non-parameterised 3D struct. *)

  val struct_name : struct_ -> string
  (** Name of a struct declaration. *)

  val field_names : struct_ -> string list
  (** Named field names in declaration order. *)

  val field_kinds : struct_ -> (string * Types.ocaml_kind) list
  (** Field names with their OCaml kind. *)

  val struct_params : struct_ -> Types.param list
  (** Formal parameters of a struct. *)

  val struct_typ : struct_ -> unit Types.typ
  (** View a 3D struct as a wire type. *)

  val param : string -> 'a Types.typ -> Types.param
  (** Immutable parameter declaration. *)

  val mutable_param : string -> 'a Types.typ -> Types.param
  (** Mutable parameter declaration. *)

  val param_struct :
    string ->
    Types.param list ->
    ?where:bool Types.expr ->
    field list ->
    struct_
  (** Parameterised 3D struct. *)

  val apply : 'a Types.typ -> int Types.expr list -> 'a Types.typ
  (** Apply a parameterised type to integer arguments. *)

  val type_ref : string -> 'a Types.typ
  (** Unqualified type reference. *)

  val qualified_ref : string -> string -> 'a Types.typ
  (** Qualified type reference. *)

  val pp_typ : Format.formatter -> 'a Types.typ -> unit
  (** Pretty-printer for 3D-facing type syntax. *)

  val pp_module : Format.formatter -> module_ -> unit
  (** Pretty-printer for 3D modules. *)

  val struct_size : struct_ -> int option
  (** Fixed wire size of a struct, if known statically. *)

  val of_module : name:string -> module_:module_ -> wire_size:int -> t
  (** Wrap an existing 3D module as a schema. *)
end
