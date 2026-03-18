(** EverParse 3D declarations derived from wire descriptions.

    This module is the 3D-facing side of the library. It provides:

    - a small abstract syntax tree for 3D declarations and modules;
    - constructors for hand-written 3D fragments when needed;
    - the bridge from {!Wire.Codec} to 3D structs and schemas.

    Most users reach for {!struct_of_codec} first: define a typed codec in
    {!Wire}, then project it to a 3D struct or schema here. *)

type schema
type struct_ = Types.struct_
type field = Types.field
type param = Types.param
type action = Action.t
type action_stmt = Action.stmt
type decl = Types.decl
type decl_case = Types.decl_case
type module_ = Types.module_

val struct_of_codec : 'r Codec.t -> struct_
(** Projects a typed codec to a 3D struct declaration. *)

val schema : 'r Codec.t -> schema
(** Builds a one-struct schema from a codec. The resulting module contains a
    single entrypoint typedef. *)

val generate : outdir:string -> schema list -> unit
(** Writes one [.3d] file per schema in [outdir]. *)

(** {1 3D Declarations} *)

val typedef : ?entrypoint:bool -> ?export:bool -> ?doc:string -> struct_ -> decl
val define : string -> int -> decl
val extern_fn : string -> param list -> 'a Types.typ -> decl
val extern_probe : ?init:bool -> string -> decl
val enum_decl : string -> (string * int) list -> 'a Types.typ -> decl
val decl_case : int -> 'a Types.typ -> decl_case
val decl_default : 'a Types.typ -> decl_case

val casetype_decl :
  string -> param list -> 'a Types.typ -> decl_case list -> decl

val module_ : ?doc:string -> decl list -> module_
val to_3d : module_ -> string
val to_3d_file : string -> module_ -> unit

(** {1 Struct Construction} *)

val field :
  string ->
  ?constraint_:bool Types.expr ->
  ?action:action ->
  'a Types.typ ->
  field

val anon_field : 'a Types.typ -> field
val struct_ : string -> field list -> struct_
val struct_name : struct_ -> string
val struct_typ : struct_ -> unit Types.typ
val param : string -> 'a Types.typ -> param
val mutable_param : string -> 'a Types.typ -> param

val param_struct :
  string -> param list -> ?where:bool Types.expr -> field list -> struct_

val apply : 'a Types.typ -> int Types.expr list -> 'a Types.typ
val type_ref : string -> 'a Types.typ
val qualified_ref : string -> string -> 'a Types.typ

(** {1 Pretty Printing} *)

val pp_typ : Format.formatter -> 'a Types.typ -> unit
val pp_module : Format.formatter -> module_ -> unit

(** {1 Struct Helpers} *)

val wire_size : struct_ -> int option
val ml_type_of : 'a Types.typ -> string

(** {1 Schema from Module} *)

val of_module : name:string -> module_:module_ -> wire_size:int -> schema
