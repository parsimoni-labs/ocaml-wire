(** EverParse 3D declarations derived from wire descriptions.

    This module is the 3D-facing side of the library. It provides:

    - a small abstract syntax tree for 3D declarations and modules;
    - constructors for hand-written 3D fragments when needed;
    - the bridge from {!Wire.Codec} to 3D structs and schemas.

    Most users reach for {!struct_of_codec} first: define a typed codec in
    {!Wire}, then project it to a 3D struct or schema here. *)

type schema
(** An opaque schema ready for 3D file generation. *)

type struct_ = Types.struct_
type field = Types.field
type decl = Types.decl
type decl_case = Types.decl_case
type module_ = Types.module_

val struct_of_codec : 'r Codec.t -> struct_
(** Projects a typed codec to a 3D struct declaration. *)

val schema : 'r Codec.t -> schema
(** Builds a one-struct schema from a codec. The resulting module contains a
    single entrypoint typedef. *)

val generate : outdir:string -> schema list -> unit
(** [generate ~outdir schemas] writes one [.3d] file per schema in [outdir]. *)

(** {1 3D Declarations} *)

val typedef : ?entrypoint:bool -> ?export:bool -> ?doc:string -> struct_ -> decl
(** Declare a typedef. *)

val define : string -> int -> decl
(** [define name value] declares a [#define] constant. *)

val extern_fn : string -> Types.param list -> 'a Types.typ -> decl
(** Declare an extern function. *)

val extern_probe : ?init:bool -> string -> decl
(** Declare an extern probe. *)

val enum_decl : string -> (string * int) list -> 'a Types.typ -> decl
(** Declare an enum type. *)

val decl_case : int -> 'a Types.typ -> decl_case
(** A case branch with a tag value. *)

val decl_default : 'a Types.typ -> decl_case
(** A default case branch. *)

val casetype_decl :
  string -> Types.param list -> 'a Types.typ -> decl_case list -> decl
(** Declare a casetype. *)

val module_ : ?doc:string -> decl list -> module_
(** Build a module from declarations. *)

val to_3d : module_ -> string
(** Render a module as a 3D source string. *)

val to_3d_file : string -> module_ -> unit
(** [to_3d_file path m] writes module [m] to a [.3d] file at [path]. *)

(** {1 Struct Construction} *)

val field :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:Action.t ->
  'a Types.typ ->
  field
(** Construct a named field. *)

val anon_field : 'a Types.typ -> field
(** Construct an anonymous (padding) field. *)

val field_ref : string -> int Types.expr
(** Reference a field by name in 3D constraint/size expressions. *)

val struct_ : string -> field list -> struct_
(** Construct a struct from fields. *)

val struct_name : struct_ -> string
(** Return the struct name. *)

val struct_params : struct_ -> Types.param list
(** Return the formal parameters (empty for non-parameterised structs). *)

val struct_typ : struct_ -> unit Types.typ
(** Return the struct as a type. *)

val param : string -> 'a Types.typ -> Types.param
(** Declare an immutable parameter. *)

val mutable_param : string -> 'a Types.typ -> Types.param
(** Declare a mutable parameter. *)

val param_struct :
  string -> Types.param list -> ?where:bool Types.expr -> field list -> struct_
(** Construct a parameterised struct. *)

val apply : 'a Types.typ -> int Types.expr list -> 'a Types.typ
(** Apply arguments to a parameterised type. *)

val type_ref : string -> 'a Types.typ
(** Reference a type by name. *)

val qualified_ref : string -> string -> 'a Types.typ
(** Reference a type by module and name. *)

(** {1 Pretty Printing} *)

val pp_typ : Format.formatter -> 'a Types.typ -> unit
(** Pretty-print a type. *)

val pp_module : Format.formatter -> module_ -> unit
(** Pretty-print a module. *)

(** {1 Struct Helpers} *)

val size : struct_ -> int option
(** Fixed wire size if determinable. *)

(** {1 Schema from Module} *)

val of_module : name:string -> module_:module_ -> wire_size:int -> schema
(** Build a schema from a pre-built module. *)
