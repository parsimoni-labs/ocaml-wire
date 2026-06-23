(** 3D code generation from Wire codecs. *)

type t = {
  name : string;
  module_ : Types.module_;
  wire_size : int option;
  source : Types.struct_ option;
}

let pp ppf t =
  match t.wire_size with
  | Some n -> Fmt.pf ppf "%s(%d)" t.name n
  | None -> Fmt.pf ppf "%s(var)" t.name

let rec is_bitfield : type a. a Types.typ -> bool = function
  | Types.Bits _ -> true
  | Types.Map { inner; _ } -> is_bitfield inner
  | Types.Enum { base; _ } -> is_bitfield base
  | Types.Where { inner; _ } -> is_bitfield inner
  | _ -> false

let rec is_byte_field : type a. a Types.typ -> bool = function
  | Types.Byte_array _ | Types.Byte_array_where _ | Types.Byte_slice _
  | Types.Uint_var _ ->
      true
  | Types.All_bytes | Types.All_zeros -> true
  | Types.Zeroterm | Types.Zeroterm_at_most _ -> true
  (* A statically-present optional is transparent: it projects as its inner
     (see [field_suffix]), so a byte-span / composite inner keeps its offset
     callback. A statically-absent one is a zero-byte region. *)
  | Types.Optional { present = Types.Bool true; inner } -> is_byte_field inner
  | Types.Optional { present = Types.Bool false; _ } -> false
  | Types.Optional _ -> true
  | Types.Optional_or { present = Types.Bool true; inner; _ } ->
      is_byte_field inner
  | Types.Optional_or { present = Types.Bool false; _ } -> false
  | Types.Optional_or _ -> true
  | Types.Map { inner; _ } -> is_byte_field inner
  (* Casetype projects to a struct value, which is not "readable" in 3D
     actions. Treat it like a byte span: the SetBytes setter receives an
     offset into the buffer, and the C side decodes if it wants to. *)
  | Types.Casetype _ -> true
  (* A repeat-into-list field has no single value 3D can read; expose its
     bytes via the [SetBytes] offset just like other variable-size fields. *)
  | Types.Repeat _ -> true
  (* An array projects to a byte region (its elements laid out contiguously);
     like a repeat, expose it through the [SetBytes] offset rather than passing
     a C array by value. *)
  | Types.Array _ -> true
  (* An embedded sub-codec projects to a struct value, which (like a casetype)
     is not "readable" in a 3D action: passing it by value to a [WireSet*]
     setter makes EverParse fail with "Parse_with_dep_action: tag not
     readable". Expose its bytes via the [SetBytes] offset instead. *)
  | Types.Codec _ -> true
  (* A [nested ~size] is a fixed-size byte region holding one inner value. 3D
     parses it with a [:byte-size-single-element-array] suffix, which is not a
     readable value in an action; expose the region through the [SetBytes]
     offset like other byte spans, so the extern setter takes an offset rather
     than the (unrepresentable) array-typed value. *)
  | Types.Single_elem _ -> true
  | _ -> false

type setter_info = { name : string; val_typ : Types.packed_typ }

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
  (* Signed integers and floats project to the same-width [UINT*] (the
     reinterpretation lives in the OCaml decoder), so they route to the matching
     width setter rather than the generic [SetBytes], whose single value type
     cannot hold two scalar fields of different widths. *)
  | Types.Int8 -> "U8"
  | Types.Int16 Types.Little -> "U16"
  | Types.Int16 Types.Big -> "U16BE"
  | Types.Int32 Types.Little -> "U32"
  | Types.Int32 Types.Big -> "U32BE"
  | Types.Int64 Types.Little -> "U64"
  | Types.Int64 Types.Big -> "U64BE"
  | Types.Float32 Types.Little -> "U32"
  | Types.Float32 Types.Big -> "U32BE"
  | Types.Float64 Types.Little -> "U64"
  | Types.Float64 Types.Big -> "U64BE"
  | Types.Bits { base = Types.U8; _ } -> "U8"
  | Types.Bits { base = Types.U16 Types.Little; _ } -> "U16"
  | Types.Bits { base = Types.U16 Types.Big; _ } -> "U16BE"
  | Types.Bits { base = Types.U32 Types.Little; _ } -> "U32"
  | Types.Bits { base = Types.U32 Types.Big; _ } -> "U32BE"
  | Types.Map { inner; _ } -> type_suffix inner
  | Types.Enum { base; _ } -> type_suffix base
  | Types.Where { inner; _ } -> type_suffix inner
  | _ -> "Bytes"

(* Each schema gets its own namespace of [WireSet*] setters, prefixed with
   the schema name, so multiple schemas can be linked into a single binary
   without symbol collisions. E.g. [SsidSetU8] and [MbrPartitionSetU8]
   coexist, each calling into its own [Fields] struct. *)
let rec setter_of : type a. string -> a Types.typ -> setter_info =
 fun schema_name t ->
  match t with
  | Types.Byte_array _ | Types.Byte_array_where _ | Types.Byte_slice _
  | Types.Uint_var _ ->
      {
        name = schema_name ^ "SetBytes";
        val_typ = Types.Pack_typ (Types.Uint32 Types.Little);
      }
  | Types.Optional { inner; _ } -> setter_of schema_name inner
  | Types.Optional_or { inner; _ } -> setter_of schema_name inner
  | Types.Map { inner; _ } -> setter_of schema_name inner
  | Types.Enum { base; _ } -> setter_of schema_name base
  | Types.Where { inner; _ } -> setter_of schema_name inner
  | _ ->
      let suffix = type_suffix t in
      { name = schema_name ^ "Set" ^ suffix; val_typ = Types.Pack_typ t }

let setter_call : type a.
    string -> a Types.typ -> string -> int -> int option -> Types.action_stmt =
 fun schema_name typ name field_idx byte_off ->
  let setter, value =
    if is_byte_field typ then
      let off =
        match byte_off with
        | Some off -> Fmt.str "(UINT32) %d" off
        | None -> Fmt.str "(UINT32) 0"
      in
      (schema_name ^ "SetBytes", off)
    else
      let { name = setter_name; _ } = setter_of schema_name typ in
      (setter_name, Types.escape_3d name)
  in
  Types.Extern_call (setter, [ "ctx"; Fmt.str "(UINT32) %d" field_idx; value ])

(* Build the statements of an [:act] block from a user action plus the auto
   setter call. An [:act] block is unit, so a trailing [return] is dropped: in
   OCaml [on_act] and [on_success] evaluate identically and a trailing
   [return true] is a no-op success, while the setter (also unit) runs last. *)
let act_stmts stmts call =
  let stmts =
    match List.rev stmts with
    | Types.Return _ :: rest -> List.rev rest
    | _ -> stmts
  in
  stmts @ [ call ]

let ends_in_return stmts =
  match List.rev stmts with Types.Return _ :: _ -> true | _ -> false

(* Build the statements of an [:on-success] block (which returns a Bool) from a
   user action plus the auto setter call. 3D is stricter than wire's action
   model: a [return] may only appear as the terminal statement or in the
   branches of a terminal [if/else] whose branches both return Bool. So the
   setter (which must always fire on success) is moved to the front, and:
   - a user action ending in a conditional [return] keeps that [if] terminal,
     with an [else { return true }] synthesised when the user gave none;
   - one ending in a plain [return] keeps it terminal;
   - otherwise the setter and a [return true] are appended as before. *)
let on_success_stmts stmts call =
  match List.rev stmts with
  | Types.If (cond, then_, else_opt) :: before when ends_in_return then_ ->
      let else_ =
        match else_opt with Some e -> e | None -> [ Types.Return Types.true_ ]
      in
      (call :: List.rev before) @ [ Types.If (cond, then_, Some else_) ]
  | Types.Return _ :: _ -> call :: stmts
  | _ -> stmts @ [ call; Types.Return Types.true_ ]

let map_field_action schema_name idx byte_off (Types.Field f) =
  let field_size = Types.field_wire_size f.field_typ in
  let next_off =
    match (byte_off, field_size) with
    | Some o, Some s -> Some (o + s)
    | _ -> None
  in
  let result =
    match f.field_name with
    | Some name ->
        let field_idx = !idx in
        incr idx;
        let call =
          setter_call schema_name f.field_typ name field_idx byte_off
        in
        let new_action =
          if is_bitfield f.field_typ then
            (* Bitfields: :act fires per sub-field during coalesced parsing *)
            match f.action with
            | None -> Some (Types.Act [ call ])
            | Some (Types.Act stmts) -> Some (Types.Act (act_stmts stmts call))
            | Some (Types.Success stmts) ->
                Some (Types.Act (act_stmts stmts call))
          else
            (* Scalars and byte-size fields: :on-success *)
            match f.action with
            | None -> Some (Types.Success [ call; Types.Return Types.true_ ])
            | Some (Types.Success stmts) ->
                Some (Types.Success (on_success_stmts stmts call))
            | Some (Types.Act stmts) -> Some (Types.Act (act_stmts stmts call))
        in
        Types.Field
          {
            field_name = Some name;
            field_typ = f.field_typ;
            constraint_ = f.constraint_;
            action = new_action;
            field_doc = f.field_doc;
          }
    | None -> Types.Field f
  in
  (result, next_off)

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
   (accept/reject) -- EverParse's per-field constraints are pure boolean
   predicates, so moving them later in the parse still produces the same
   overall verdict. Bitfield actions use 3D's [:act] form, which fires
   regardless of validation outcome, so moving constraints does not
   affect callback behaviour. *)
let collapse_constraints_into_last group =
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
   original (wire-declaration) slots -- the stub generator never sees the
   reordered struct. *)
(* Extract bitfield info through any number of [Map]/[Enum]/[Where] wrappers.
   [bit (bits ~width:1 U8)] is [Map { inner = Bits _ }] at the outer level;
   without unwrapping, grouping logic would treat it as a non-bitfield and
   break apart consecutive bit groups, producing wrong .3d layouts. *)
let rec unwrap_bits : type a.
    a Types.typ -> (Types.bitfield_base * Types.bit_order * int) option =
  function
  | Types.Bits { base; bit_order; width } -> Some (base, bit_order, width)
  | Types.Map { inner; _ } -> unwrap_bits inner
  | Types.Enum { base; _ } -> unwrap_bits base
  | Types.Where { inner; _ } -> unwrap_bits inner
  | _ -> None

let is_same_bit_group base bit_order (Types.Field f) =
  match unwrap_bits f.field_typ with
  | Some (b2, bo2, _) -> b2 = base && bo2 = bit_order
  | None -> false

let bit_width (Types.Field f) =
  match unwrap_bits f.field_typ with Some (_, _, w) -> w | None -> 0

(* Greedy: collect consecutive Bits with the same (base, bit_order)
   that still fit in one base word. *)
let collect_bit_group base bit_order total f0 rest =
  let rec collect used group = function
    | f :: rest' when is_same_bit_group base bit_order f ->
        let w = bit_width f in
        if used + w <= total then collect (used + w) (f :: group) rest'
        else (used, List.rev group, f :: rest')
    | rest' -> (used, List.rev group, rest')
  in
  collect (bit_width f0) [ f0 ] rest

let pad_reversed_group total used base native reversed =
  let padding = total - used in
  if padding > 0 then
    let pad_typ = Types.Bits { width = padding; base; bit_order = native } in
    Types.Field
      {
        field_name = None;
        field_typ = pad_typ;
        constraint_ = None;
        action = None;
        field_doc = None;
      }
    :: reversed
  else reversed

let reorder_bit_group base bit_order f0 rest =
  let total = Bitfield.total_bits base in
  let native = Bitfield.native_bit_order base in
  let used, group, rest' = collect_bit_group base bit_order total f0 rest in
  let emitted =
    if bit_order = native then group
    else
      (* Backward references in reversed order would break: fields now
         come before the values their constraints read. Collapse all
         constraints onto the last reversed field. *)
      let reversed = collapse_constraints_into_last (List.rev group) in
      pad_reversed_group total used base native reversed
  in
  (emitted, rest')

let reorder_bit_groups_for_3d fields =
  let rec go acc = function
    | [] -> List.rev acc
    | (Types.Field f as f0) :: rest -> (
        match unwrap_bits f.field_typ with
        | Some (base, bit_order, _) ->
            let emitted, rest' = reorder_bit_group base bit_order f0 rest in
            go (List.rev_append emitted acc) rest'
        | None -> go (f0 :: acc) rest)
  in
  go [] fields

let bytes_setter schema_name : setter_info =
  {
    name = schema_name ^ "SetBytes";
    val_typ = Types.Pack_typ (Types.Uint32 Types.Little);
  }

let collect_extern_setters schema_name ctx_struct u32 fields =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun (Types.Field f) ->
      match f.field_name with
      | None -> None
      | Some _ ->
          let si =
            if is_byte_field f.field_typ then bytes_setter schema_name
            else setter_of schema_name f.field_typ
          in
          if Hashtbl.mem seen si.name then None
          else begin
            Hashtbl.add seen si.name ();
            let (Types.Pack_typ val_typ) = si.val_typ in
            Some
              (Types.extern_fn si.name
                 [
                   Types.mutable_param "ctx" (Types.struct_typ ctx_struct);
                   Types.param "idx" u32;
                   Types.param "v" val_typ;
                 ]
                 Types.Unit)
          end)
    fields

(* For each [Byte_array_where] field in [s], synthesise a 1-byte struct that
   names the element [elt_var] and applies [cond] as a field constraint.
   3D's syntax does not allow per-element refinement on the byte-size array
   itself, so we lift the refinement into a wrapper struct and reference it
   from the parent field. The naming convention shared with
   [Types.synth_name_of_elt_var] keeps the field rendering and the typedef
   name in sync without threading state. *)
let refined_byte_typedefs (s : Types.struct_) : Types.decl list =
  (* The refined-byte element a field needs synthesised, if any: an explicit
     [byte_array_where], or an [array] / [repeat] whose 1-byte element carries a
     lookup index bound (which projects the same way). Look through the
     transparent wrappers a field can put it under (a statically-present
     [optional], [map] / [where]) so the typedef is still emitted. *)
  let rec synth_of_typ : type a.
      a Types.typ -> (string * bool Types.expr) option = function
    | Types.Byte_array_where { elt_var; cond; _ } -> Some (elt_var, cond)
    | Types.Array { elem; _ } -> Types.index_bound_elt elem
    | Types.Repeat { elem; _ } -> Types.index_bound_elt elem
    | Types.Optional { present = Types.Bool true; inner } -> synth_of_typ inner
    | Types.Optional_or { present = Types.Bool true; inner; _ } ->
        synth_of_typ inner
    | Types.Map { inner; _ } -> synth_of_typ inner
    | Types.Where { inner; _ } -> synth_of_typ inner
    | _ -> None
  in
  let synth_of (Types.Field f) = synth_of_typ f.field_typ in
  (* Equal index bounds share one synthesised typedef, so emit each name once. *)
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun field ->
      match synth_of field with
      | Some (elt_var, cond) ->
          let synth = Types.synth_name_of_elt_var elt_var in
          if Hashtbl.mem seen synth then None
          else begin
            Hashtbl.add seen synth ();
            let elt_field = Types.field elt_var ~constraint_:cond Types.uint8 in
            Some (Types.typedef (Types.struct_ synth [ elt_field ]))
          end
      | None -> None)
    s.fields

(* A statically-absent optional ([~present:false]) contributes no bytes: its
   inner is never parsed. Project it as a [unit] field, the 0-byte form
   [Wire.empty] uses and EverParse verifies. (A statically-present optional is
   handled transparently elsewhere; only the absent case needs this rewrite.) *)
let rewrite_absent_optional (Types.Field f) =
  match f.field_typ with
  | Types.Optional { present = Types.Bool false; _ }
  | Types.Optional_or { present = Types.Bool false; _ } ->
      Types.Field { f with field_typ = Types.Unit; constraint_ = None }
  | _ -> Types.Field f

let ffi_decls (s : Types.struct_) : Types.decl list =
  let s = { s with fields = List.map rewrite_absent_optional s.fields } in
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
  let parse_fields =
    let off = ref (Some 0) in
    List.map
      (fun f ->
        let f', next = map_field_action s.name idx !off f in
        off := next;
        f')
      s.fields
  in
  let parse_fields = reorder_bit_groups_for_3d parse_fields in
  let parse_struct =
    Types.param_struct s.name (s.params @ [ ctx_param ]) ?where:s.where
      parse_fields
  in
  let parse_decl = Types.typedef ~entrypoint:true parse_struct in
  let extern_decls = collect_extern_setters s.name ctx_struct u32 s.fields in
  let refined_decls = refined_byte_typedefs s in
  [ ctx_decl ] @ extern_decls @ refined_decls @ [ parse_decl ]

(* Documentation / pure-validator projection: the same structural 3D as
   [ffi_decls] but without the FFI scaffolding. No [WireCtx] extern, no
   [WireSet*] setters, no extraction action injected on each field. Keeps the
   struct, bitfields, [where] clause, refined-byte typedefs, enums, and
   casetypes -- everything that describes the wire format. Reads as a protocol
   spec, and 3d.exe still compiles it to a real (validator-only) C parser with
   no FFI. *)
let standalone_decls ?doc (s : Types.struct_) : Types.decl list =
  let s = { s with fields = List.map rewrite_absent_optional s.fields } in
  let parse_fields = reorder_bit_groups_for_3d s.fields in
  let parse_struct =
    Types.param_struct s.name s.params ?where:s.where parse_fields
  in
  refined_byte_typedefs s @ [ Types.typedef ?doc ~entrypoint:true parse_struct ]

(* Byte size of a struct after bitfield coalescing, mirroring Codec.compile_bits'
   logic: consecutive same-base, same-bit_order bitfields pack into one base
   word if their widths sum within the word; they roll over to a new base word
   otherwise. Matches what EverParse's validator actually consumes. Returns
   [None] for schemas with variable-size fields. *)
let coalesced_wire_size fields =
  let exception Bail in
  let close_bit_group total = function
    | None -> total
    | Some base -> total + Bitfield.byte_size base
  in
  try
    let total, open_base, _, _ =
      List.fold_left
        (fun (total, open_base, bits_used, bit_order) (Types.Field f) ->
          match unwrap_bits f.field_typ with
          | Some (base, order, width) -> (
              match open_base with
              | Some b
                when b = base && bit_order = Some order
                     && bits_used + width <= Bitfield.total_bits base ->
                  (total, open_base, bits_used + width, bit_order)
              | _ ->
                  let total' = close_bit_group total open_base in
                  (total', Some base, width, Some order))
          | None -> (
              let total' = close_bit_group total open_base in
              match Types.field_wire_size f.field_typ with
              | Some n -> (total' + n, None, 0, None)
              | None -> raise Bail))
        (0, None, 0, None) fields
    in
    Some (close_bit_group total open_base)
  with Bail -> None

let ffi_of_struct (s : Types.struct_) : t =
  (* Split string-tagged casetype fields up-front so [source] and
     [ffi_decls] see the same field list. Downstream codegen
     (plug fields, stubs) walks [source] to expose every named field --
     it must include the synthesised body field of each casetype. *)
  let s = Types.split_string_casetype_fields s in
  let name = Types.struct_name s in
  let wire_size = coalesced_wire_size s.fields in
  let decls = ffi_decls s in
  let m = Types.module_ decls in
  (* [schema] is the authoritative projectability gate: a constraint with no 3D
     projection (a [field_pos], a subtraction or multiplication over a field) is
     rejected here, when the codec is projected, rather than slipping through to
     an unguarded [to_3d] later. The rejection lives in the renderer, so validate
     by rendering once; [pp_struct] resets its counters, so this does not affect
     a subsequent render. *)
  ignore (Types.to_3d m : string);
  { name; module_ = m; wire_size; source = Some s }

let standalone_of_struct ?doc (s : Types.struct_) : t =
  let s = Types.split_string_casetype_fields s in
  let name = Types.struct_name s in
  let wire_size = coalesced_wire_size s.fields in
  let m = Types.module_ (standalone_decls ?doc s) in
  (* Authoritative projectability gate, as in [ffi_of_struct]; the doc
     projection renders enums as named types. *)
  ignore (Types.to_3d ~enum_as_type:true m : string);
  { name; module_ = m; wire_size; source = Some s }

type mode = [ `Ffi | `Standalone ]

(* [`Ffi] emits the [WireCtx] extern plus a per-field setter callback, so the
   generated C validator is callable from OCaml and extracts fields through the
   plug (the bridge used by benchmarks and differential testing). [`Standalone]
   emits a clean [.3d] with no FFI scaffolding, which EverParse compiles to a
   standalone verified C parser; it also reads as a protocol specification. *)
let project : type r. ?mode:mode -> r Codec.t -> t =
 fun ?(mode = `Standalone) codec ->
  match mode with
  | `Ffi -> ffi_of_struct (Codec.to_struct codec)
  | `Standalone ->
      standalone_of_struct ?doc:(Codec.doc codec) (Codec.to_struct codec)

let filename (s : t) = String.capitalize_ascii s.name ^ ".3d"

let uses_wire_ctx s =
  List.exists
    (function
      | Types.Typedef { extern_ = true; struct_ = { name = "WireCtx"; _ }; _ }
        ->
          true
      | _ -> false)
    s.module_.decls

type plug_field = {
  name : string;
  idx : int;
  c_type : string;
  setter : string;
  val_c_type : string;
}

let plug_field (s : t) idx (Types.Field f) =
  match f.field_name with
  | None -> None
  | Some name ->
      let i = !idx in
      incr idx;
      let setter =
        if is_byte_field f.field_typ then bytes_setter s.name
        else setter_of s.name f.field_typ
      in
      let (Types.Pack_typ val_typ) = setter.val_typ in
      Some
        {
          name;
          idx = i;
          c_type = Types.c_type_of f.field_typ;
          setter = setter.name;
          val_c_type = Types.c_type_of val_typ;
        }

let plug_fields s =
  match s.source with
  | None -> []
  | Some src ->
      let idx = ref 0 in
      List.filter_map (plug_field s idx) src.fields

let plug_setters s =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun f ->
      if Hashtbl.mem seen f.setter then None
      else begin
        Hashtbl.add seen f.setter ();
        Some (f.setter, f.val_c_type)
      end)
    (plug_fields s)

let entrypoint_struct s =
  List.find_map
    (function
      | Types.Typedef { entrypoint = true; extern_ = false; struct_ = st; _ } ->
          Some st
      | _ -> None)
    s.module_.decls

let extern_fn_names s =
  List.filter_map
    (function Types.Extern_fn { name; _ } -> Some name | _ -> None)
    s.module_.decls

type field_action_form = No_action | On_act | On_success

let field_action_forms (st : Types.struct_) =
  List.map
    (fun (Types.Field f) ->
      let form =
        match f.action with
        | None -> No_action
        | Some (Types.Act _) -> On_act
        | Some (Types.Success _) -> On_success
      in
      (f.field_name, is_bitfield f.field_typ, form))
    st.fields

let write_ffi ~outdir schemas =
  List.iter
    (fun s -> Types.to_3d_file (Filename.concat outdir (filename s)) s.module_)
    schemas

(* Merge several clean (doc) schemas into one module: union their decls, keeping
   the first definition of each named typedef / enum / casetype so a type shared
   across codecs (a common enum, a refined-byte element) is emitted once.
   Dependency order survives because each input module already lists a type
   before the typedef that uses it, and the shared type keeps its first slot. *)
let merge ~name (ts : t list) : t =
  let decl_name = function
    | Types.Typedef { struct_ = { name; _ }; _ } -> Some name
    | Types.Enum_decl { name; _ } | Types.Casetype_decl { name; _ } -> Some name
    | Types.Define { name; _ } | Types.Extern_fn { name; _ } -> Some name
    | _ -> None
  in
  let seen = Hashtbl.create 16 in
  let keep d =
    match decl_name d with
    | None -> true
    | Some n when Hashtbl.mem seen n -> false
    | Some n ->
        Hashtbl.add seen n ();
        true
  in
  let decls =
    List.fold_left
      (fun acc t ->
        List.fold_left
          (fun acc d -> if keep d then d :: acc else acc)
          acc t.module_.decls)
      [] ts
    |> List.rev
  in
  { name; module_ = Types.module_ decls; wire_size = None; source = None }

let write_standalone ~outdir ~name (ts : t list) =
  Types.to_3d_file ~enum_as_type:true
    (Filename.concat outdir (String.capitalize_ascii name ^ ".3d"))
    (merge ~name ts).module_

(* [`Ffi] writes one [.3d] per schema (file name from each schema). [`Standalone]
   merges the schemas into one [<name>.3d] so a protocol family reads as a single
   spec, and so requires [~name]. *)
let write ?(mode = `Standalone) ~outdir ?name (ts : t list) =
  match (mode, name) with
  | `Ffi, _ -> write_ffi ~outdir ts
  | `Standalone, Some name -> write_standalone ~outdir ~name ts
  | `Standalone, None ->
      invalid_arg "Everparse.write: ~mode:`Standalone requires ~name"

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
  let struct_of_codec = struct_of_codec

  let project_struct ?(mode = `Standalone) s =
    match mode with
    | `Ffi -> ffi_of_struct s
    | `Standalone -> standalone_of_struct s

  let field name ?constraint_ ?action typ =
    Field.Named (Field.v name ?constraint_ ?action typ)

  let anon_field typ = Field.Anon (Field.anon typ)

  let field_ref = function
    | Field.Named f -> Types.ref (Field.name f)
    | Field.Anon _ -> invalid_arg "Everparse.Raw.field_ref: anonymous field"

  let unpack_fields fields = List.map Field.decl_of_packed fields
  let struct_ name fields = Types.struct_ name (unpack_fields fields)
  let struct_name = Types.struct_name
  let field_names = Types.field_names

  let struct_project s ~name ~keep =
    Types.struct_project s ~name ~keep:(List.map Field.decl_of_packed keep)

  type ocaml_kind = Types.ocaml_kind =
    | Int
    | Int64
    | Float32
    | Float64
    | Bool
    | String
    | Unit

  let field_kinds = Types.field_kinds
  let struct_params (s : Types.struct_) = s.params

  let input_param_names (s : Types.struct_) =
    List.filter_map
      (fun (p : Types.param) -> if p.mutable_ then None else Some p.param_name)
      s.params

  let input_param_c_types (s : Types.struct_) =
    List.filter_map
      (fun (p : Types.param) ->
        if p.mutable_ then None
        else
          let (Types.Pack_typ t) = p.param_typ in
          Some (Types.c_type_of t))
      s.params

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

  let of_module ~name ~module_ ~wire_size =
    { name; module_; wire_size = Some wire_size; source = None }
end
