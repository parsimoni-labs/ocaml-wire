(** 3D code generation from Wire codecs. *)

type t = { name : string; module_ : Types.module_; wire_size : int }

(* Rewrite a struct to add the EverParse output-types pattern *)
let is_bitfield : type a. a Types.typ -> bool = function
  | Types.Bits _ -> true
  | Types.Map { inner = Types.Bits _; _ } -> true
  | _ -> false

let with_output (s : Types.struct_) : Types.decl list =
  let out_name = "O" ^ s.name in
  (* Output struct: non-bitfield named fields only (EverParse rejects
     per-field actions on bitfield sub-fields) *)
  let out_fields =
    List.filter_map
      (fun (Types.Field f) ->
        match f.field_name with
        | Some name when not (is_bitfield f.field_typ) ->
            Some (Types.field name f.field_typ)
        | _ -> None)
      s.fields
  in
  let has_output_fields = out_fields <> [] in
  if not has_output_fields then
    (* No non-bitfield fields — fall back to plain typedef *)
    [ Types.typedef ~entrypoint:true s ]
  else
    let out_struct = Types.struct_ out_name out_fields in
    let out_decl = Types.typedef ~output:true out_struct in
    let out_param = Types.mutable_param "out" (Types.struct_typ out_struct) in
    let parse_fields =
      List.map
        (fun (Types.Field f) ->
          match f.field_name with
          | Some name when not (is_bitfield f.field_typ) ->
              let assign = Types.Field_assign ("out", name, Types.Ref name) in
              let new_action =
                match f.action with
                | None ->
                    Some (Types.On_success [ assign; Types.Return Types.true_ ])
                | Some (Types.On_success stmts) ->
                    Some
                      (Types.On_success
                         (stmts @ [ assign; Types.Return Types.true_ ]))
                | Some (Types.On_act stmts) ->
                    Some (Types.On_act (stmts @ [ assign ]))
              in
              Types.Field
                {
                  field_name = Some name;
                  field_typ = f.field_typ;
                  constraint_ = f.constraint_;
                  action = new_action;
                }
          | _ -> Types.Field f)
        s.fields
    in
    let parse_struct =
      Types.param_struct s.name (s.params @ [ out_param ]) ?where:s.where
        parse_fields
    in
    let parse_decl = Types.typedef ~entrypoint:true parse_struct in
    [ out_decl; parse_decl ]

let schema ?(output = false) (type r) (codec : r Codec.t) : t =
  let s = Codec.to_struct codec in
  let name = Types.struct_name s in
  let wire_size = Codec.wire_size codec in
  let decls =
    if output then with_output s else [ Types.typedef ~entrypoint:true s ]
  in
  let m = Types.module_ decls in
  { name; module_ = m; wire_size }

let generate ~outdir schemas =
  List.iter
    (fun s ->
      Types.to_3d_file (Filename.concat outdir (s.name ^ ".3d")) s.module_)
    schemas

(* Public C-facing types *)

type struct_ = Types.struct_
type decl = Types.decl
type decl_case = Types.decl_case
type module_ = Types.module_

let struct_of_codec = Codec.to_struct

module Raw = struct
  type nonrec struct_ = struct_
  type field = Field.packed
  type nonrec decl = decl
  type nonrec decl_case = decl_case
  type nonrec module_ = module_
  type nonrec t = t

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

  let field name ?constraint_ ?action typ =
    Field.Named (Field.v name ?constraint_ ?action typ)

  let anon_field typ = Field.Anon (Field.anon typ)

  let field_ref = function
    | Field.Named f -> Field.ref f
    | Field.Anon _ -> invalid_arg "C.Raw.field_ref: anonymous field"

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
  let pp_typ = Types.pp_typ
  let pp_module = Types.pp_module

  let struct_size (s : Types.struct_) =
    List.fold_left
      (fun acc (Types.Field f) ->
        match (acc, Types.field_wire_size f.field_typ) with
        | Some a, Some b -> Some (a + b)
        | _ -> None)
      (Some 0) s.fields

  let of_module ~name ~module_ ~wire_size = { name; module_; wire_size }
end
