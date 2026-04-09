(** 3D code generation from Wire codecs. *)

type t = { name : string; module_ : Types.module_; wire_size : int }

let pp ppf t = Fmt.pf ppf "%s(%d)" t.name t.wire_size

let rec is_bitfield : type a. a Types.typ -> bool = function
  | Types.Bits _ -> true
  | Types.Map { inner; _ } -> is_bitfield inner
  | Types.Enum { base; _ } -> is_bitfield base
  | Types.Where { inner; _ } -> is_bitfield inner
  | _ -> false

let rec is_byte_field : type a. a Types.typ -> bool = function
  | Types.Byte_array _ | Types.Byte_slice _ -> true
  | Types.Map { inner; _ } -> is_byte_field inner
  | _ -> false

type setter_info = { setter_name : string; setter_val_typ : Types.packed_typ }

(* 3D type suffix for unique extern function names *)
let rec type_suffix : type a. a Types.typ -> string = function
  | Types.Uint8 -> "U8"
  | Types.Uint16 Types.Little -> "U16"
  | Types.Uint16 Types.Big -> "U16BE"
  | Types.Uint32 Types.Little -> "U32"
  | Types.Uint32 Types.Big -> "U32BE"
  | Types.Uint63 Types.Little -> "U63"
  | Types.Uint63 Types.Big -> "U63BE"
  | Types.Uint64 Types.Little -> "U64"
  | Types.Uint64 Types.Big -> "U64BE"
  | Types.Bits { base = Types.BF_U8; _ } -> "U8"
  | Types.Bits { base = Types.BF_U16 Types.Little; _ } -> "U16"
  | Types.Bits { base = Types.BF_U16 Types.Big; _ } -> "U16BE"
  | Types.Bits { base = Types.BF_U32 Types.Little; _ } -> "U32"
  | Types.Bits { base = Types.BF_U32 Types.Big; _ } -> "U32BE"
  | Types.Map { inner; _ } -> type_suffix inner
  | Types.Enum { base; _ } -> type_suffix base
  | Types.Where { inner; _ } -> type_suffix inner
  | _ -> "Bytes"

let rec setter_of : type a. a Types.typ -> setter_info = function
  | Types.Byte_array _ | Types.Byte_slice _ ->
      {
        setter_name = "WireSetBytes";
        setter_val_typ = Types.Pack_typ (Types.Uint32 Types.Little);
      }
  | Types.Map { inner; _ } -> setter_of inner
  | Types.Enum { base; _ } -> setter_of base
  | Types.Where { inner; _ } -> setter_of inner
  | t ->
      let suffix = type_suffix t in
      { setter_name = "WireSet" ^ suffix; setter_val_typ = Types.Pack_typ t }

let map_field_action idx (Types.Field f) =
  match f.field_name with
  | Some name ->
      let field_idx = !idx in
      incr idx;
      let new_action =
        let { setter_name = setter; _ } = setter_of f.field_typ in
        let value =
          if is_byte_field f.field_typ then "(UINT32) field_pos" else name
        in
        let call =
          Types.Extern_call
            (setter, [ "ctx"; Fmt.str "(UINT32) %d" field_idx; value ])
        in
        if is_bitfield f.field_typ then
          (* Bitfields: :act (non-failing, required for coalescing) *)
          match f.action with
          | None -> Some (Types.On_act [ call ])
          | Some (Types.On_act stmts) -> Some (Types.On_act (stmts @ [ call ]))
          | Some (Types.On_success stmts) ->
              Some (Types.On_act (stmts @ [ call ]))
        else
          (* Non-bitfields: :on-success (runs after validation) *)
          match f.action with
          | None -> Some (Types.On_success [ call; Types.Return Types.true_ ])
          | Some (Types.On_success stmts) ->
              Some
                (Types.On_success (stmts @ [ call; Types.Return Types.true_ ]))
          | Some (Types.On_act stmts) -> Some (Types.On_act (stmts @ [ call ]))
      in
      Types.Field
        {
          field_name = Some name;
          field_typ = f.field_typ;
          constraint_ = f.constraint_;
          action = new_action;
        }
  | None -> Types.Field f

(* Conjoin a list of constraint expressions, skipping [None]s. *)
let conjoin_constraints constraints =
  List.fold_left
    (fun acc c ->
      match (acc, c) with
      | acc, None -> acc
      | None, Some c -> Some c
      | Some a, Some b -> Some (Types.And (a, b)))
    None constraints

(* Collapse all constraints in a reversed bit group onto the last field,
   where every referenced field has already been parsed. Backward
   references to other fields in the group would otherwise break under
   reversal, because the reversed field is parsed before its referents.

   The combined constraint is semantically equivalent for validation
   (accept/reject) — EverParse's per-field constraints are pure boolean
   predicates, so moving them later in the parse still produces the same
   overall verdict. Bitfield actions use 3D's [:act] form, which fires
   regardless of validation outcome, so moving constraints does not
   affect callback behaviour. *)
let collapse_constraints_to_last group =
  let constraints = List.map (fun (Types.Field f) -> f.constraint_) group in
  let combined = conjoin_constraints constraints in
  let rec walk = function
    | [] -> []
    | [ Types.Field f ] -> [ Types.Field { f with constraint_ = combined } ]
    | Types.Field f :: rest ->
        Types.Field { f with constraint_ = None } :: walk rest
  in
  walk group

(* Reorder consecutive bitfield groups so every pairing of [bitfield_base] and
   [bit_order] projects to a valid EverParse 3D struct while keeping the same
   byte layout. EverParse couples bit order to the base's byte order
   (LE -> LSB-first, BE -> MSB-first). When the user's [bit_order] differs
   from that native choice, we reverse the group's declaration order and
   prepend [total_bits - used_bits] of anonymous padding; in EverParse's
   native packing this produces the identical bit layout. Fields outside
   bit groups are left untouched. Extern-call indices embedded in actions
   are stamped before reordering, so WireSet callbacks still write into the
   original (wire-declaration) slots — the stub generator never sees the
   reordered struct. *)
let reorder_bit_groups_for_3d fields =
  let is_same_bit_group base bit_order = function
    | Types.Field
        { field_typ = Types.Bits { base = b2; bit_order = bo2; _ }; _ } ->
        b2 = base && bo2 = bit_order
    | _ -> false
  in
  let bit_width = function
    | Types.Field { field_typ = Types.Bits { width; _ }; _ } -> width
    | _ -> 0
  in
  let rec go acc = function
    | [] -> List.rev acc
    | (Types.Field { field_typ = Types.Bits { base; bit_order; _ }; _ } as f0)
      :: rest ->
        let total = Bitfield.total_bits base in
        let native = Bitfield.native_bit_order base in
        (* Greedy: collect consecutive Bits with the same (base, bit_order)
           that still fit in one base word. *)
        let rec collect used group = function
          | f :: rest' when is_same_bit_group base bit_order f ->
              let w = bit_width f in
              if used + w <= total then collect (used + w) (f :: group) rest'
              else (used, List.rev group, f :: rest')
          | rest' -> (used, List.rev group, rest')
        in
        let used, group, rest' = collect (bit_width f0) [ f0 ] rest in
        if bit_order = native then go (List.rev_append group acc) rest'
        else begin
          let reversed = List.rev group in
          (* Backward references in reversed order would break: fields now
             come before the values their constraints read. Collapse all
             constraints onto the last reversed field. *)
          let reversed = collapse_constraints_to_last reversed in
          let padded =
            let padding = total - used in
            if padding > 0 then
              let pad_typ =
                Types.Bits { width = padding; base; bit_order = native }
              in
              Types.Field
                {
                  field_name = None;
                  field_typ = pad_typ;
                  constraint_ = None;
                  action = None;
                }
              :: reversed
            else reversed
          in
          go (List.rev_append padded acc) rest'
        end
    | other :: rest -> go (other :: acc) rest
  in
  go [] fields

let with_output (s : Types.struct_) : Types.decl list =
  (* Extern declarations for the callback mechanism *)
  let ctx_struct = Types.struct_ "WireCtx" [] in
  let ctx_decl = Types.typedef ~extern_:true ctx_struct in
  let ctx_param = Types.mutable_param "ctx" (Types.struct_typ ctx_struct) in
  (* Extern setter functions *)
  let u32 = Types.Uint32 Types.Little in
  (* Count named fields to assign indices in the ORIGINAL declaration order.
     The idx baked into each Extern_call is preserved through the reorder
     below, so WireSet callbacks still populate the original field slot. *)
  let idx = ref 0 in
  let parse_fields = List.map (map_field_action idx) s.fields in
  let parse_fields = reorder_bit_groups_for_3d parse_fields in
  let parse_struct =
    Types.param_struct s.name (s.params @ [ ctx_param ]) ?where:s.where
      parse_fields
  in
  let parse_decl = Types.typedef ~entrypoint:true parse_struct in
  (* Collect unique extern function declarations *)
  let seen = Hashtbl.create 8 in
  let extern_decls =
    List.filter_map
      (fun (Types.Field f) ->
        match f.field_name with
        | None -> None
        | Some _ ->
            let si = setter_of f.field_typ in
            if Hashtbl.mem seen si.setter_name then None
            else begin
              Hashtbl.add seen si.setter_name ();
              let (Types.Pack_typ val_typ) = si.setter_val_typ in
              Some
                (Types.extern_fn si.setter_name
                   [
                     Types.mutable_param "ctx" (Types.struct_typ ctx_struct);
                     Types.param "idx" u32;
                     Types.param "v" val_typ;
                   ]
                   Types.Unit)
            end)
      s.fields
  in
  [ ctx_decl ] @ extern_decls @ [ parse_decl ]

let schema_of_struct (s : Types.struct_) : t =
  let name = Types.struct_name s in
  let wire_size =
    List.fold_left
      (fun acc (Types.Field f) ->
        match (acc, Types.field_wire_size f.field_typ) with
        | Some a, Some b -> Some (a + b)
        | _ -> None)
      (Some 0) s.fields
    |> function
    | Some n -> n
    | None -> Fmt.failwith "schema %s has variable-length fields" name
  in
  let decls = with_output s in
  let m = Types.module_ decls in
  { name; module_ = m; wire_size }

let schema (type r) (codec : r Codec.t) : t =
  schema_of_struct (Codec.to_struct codec)

let write_3d ~outdir schemas =
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
    | Field.Named f -> Types.Ref (Field.name f)
    | Field.Anon _ -> invalid_arg "Everparse.Raw.field_ref: anonymous field"

  let unpack_fields fields = List.map Field.to_decl fields
  let struct_ name fields = Types.struct_ name (unpack_fields fields)
  let struct_name = Types.struct_name
  let field_names = Types.field_names

  let struct_project s ~name ~keep =
    Types.struct_project s ~name ~keep:(List.map Field.to_decl keep)

  type ocaml_kind = Types.ocaml_kind =
    | K_int
    | K_int64
    | K_bool
    | K_string
    | K_unit

  let field_kinds = Types.field_kinds
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
