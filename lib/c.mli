(** EverParse 3D export derived from wire descriptions.

    The main path is {!struct_of_codec}, {!schema}, and {!generate}. For unusual
    3D constructs that have no codec equivalent yet, use {!Raw}. *)

type t = { name : string; module_ : Types.module_; wire_size : int }
(** A named 3D schema with its module and wire size. *)

type struct_ = Types.struct_
type decl = Types.decl
type decl_case = Types.decl_case
type module_ = Types.module_

val struct_of_codec : 'r Codec.t -> struct_

val with_output : struct_ -> decl list
(** [with_output s] rewrites struct [s] into the EverParse output-types pattern:
    an [output typedef struct] with plain fields, plus a parsing struct with a
    mutable output pointer and [:on-success] actions that assign each field to
    the output struct. *)

val schema : ?output:bool -> 'r Codec.t -> t
(** Builds a one-struct schema from a codec. The resulting module contains a
    single entrypoint typedef. *)

val generate : outdir:string -> t list -> unit
(** [generate ~outdir ts] writes one [.3d] file per schema in [outdir]. *)

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
    ?doc:string ->
    struct_ ->
    decl

  val define : string -> int -> decl
  val extern_fn : string -> Types.param list -> 'a Types.typ -> decl
  val extern_probe : ?init:bool -> string -> decl
  val enum_decl : string -> (string * int) list -> 'a Types.typ -> decl
  val decl_case : int -> 'a Types.typ -> decl_case
  val decl_default : 'a Types.typ -> decl_case

  val casetype_decl :
    string -> Types.param list -> 'a Types.typ -> decl_case list -> decl

  val module_ : ?doc:string -> decl list -> module_
  val to_3d : module_ -> string
  val to_3d_file : string -> module_ -> unit

  val field :
    string ->
    ?constraint_:bool Types.expr ->
    ?action:Action.t ->
    'a Types.typ ->
    field

  val anon_field : 'a Types.typ -> field
  val field_ref : field -> int Types.expr
  val struct_ : string -> field list -> struct_
  val struct_name : struct_ -> string
  val field_names : struct_ -> string list
  val field_kinds : struct_ -> (string * Types.ocaml_kind) list
  val struct_params : struct_ -> Types.param list
  val struct_typ : struct_ -> unit Types.typ
  val param : string -> 'a Types.typ -> Types.param
  val mutable_param : string -> 'a Types.typ -> Types.param

  val param_struct :
    string ->
    Types.param list ->
    ?where:bool Types.expr ->
    field list ->
    struct_

  val apply : 'a Types.typ -> int Types.expr list -> 'a Types.typ
  val type_ref : string -> 'a Types.typ
  val qualified_ref : string -> string -> 'a Types.typ
  val pp_typ : Format.formatter -> 'a Types.typ -> unit
  val pp_module : Format.formatter -> module_ -> unit
  val struct_size : struct_ -> int option
  val of_module : name:string -> module_:module_ -> wire_size:int -> t
end
