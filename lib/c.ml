(** 3D code generation from Wire codecs. *)

(* Schema from Codec *)

type schema = { name : string; module_ : Types.module_; wire_size : int }

let schema (type r) (codec : r Codec.t) : schema =
  let s = Codec.to_struct codec in
  let name = Types.struct_name s in
  let wire_size = Codec.wire_size codec in
  let m = Types.module_ [ Types.typedef ~entrypoint:true s ] in
  { name; module_ = m; wire_size }

let generate ~outdir schemas =
  List.iter
    (fun s ->
      Types.to_3d_file (Filename.concat outdir (s.name ^ ".3d")) s.module_)
    schemas

(* 3D Declarations *)

type struct_ = Types.struct_
type field = Field.packed
type decl = Types.decl
type decl_case = Types.decl_case
type module_ = Types.module_

let typedef = Types.typedef
let define = Types.define
let extern_fn = Types.extern_fn
let extern_probe = Types.extern_probe
let enum_decl = Types.enum_decl
let decl_case = Types.decl_case
let decl_default = Types.decl_default
let casetype_decl = Types.casetype_decl
let module_ = Types.module_
let to_3d = Types.to_3d
let to_3d_file = Types.to_3d_file

(* Struct construction *)

let struct_of_codec = Codec.to_struct

let field name ?constraint_ ?action typ =
  Field.Named (Field.v name ?constraint_ ?action typ)

let anon_field typ = Field.Anon (Field.anon typ)

let field_ref = function
  | Field.Named f -> Field.ref f
  | Field.Anon _ -> invalid_arg "C.field_ref: anonymous field"

let unpack_fields fields = List.map Field.to_decl fields
let struct_ name fields = Types.struct_ name (unpack_fields fields)
let struct_name = Types.struct_name
let struct_params (s : Types.struct_) = s.params
let struct_typ = Types.struct_typ
let param = Types.param
let mutable_param = Types.mutable_param

let param_struct name params ?where fields =
  Types.param_struct name params ?where (unpack_fields fields)

let apply = Types.apply
let type_ref = Types.type_ref
let qualified_ref = Types.qualified_ref

(* Pretty printing *)

let pp_typ = Types.pp_typ
let pp_module = Types.pp_module

(* Struct helpers *)

let size (s : Types.struct_) =
  List.fold_left
    (fun acc (Types.Field f) ->
      match (acc, Types.field_wire_size f.field_typ) with
      | Some a, Some b -> Some (a + b)
      | _ -> None)
    (Some 0) s.fields

(* Schema from module *)

let of_module ~name ~module_ ~wire_size = { name; module_; wire_size }
