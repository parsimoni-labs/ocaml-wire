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
  | Types.Casetype _ | Types.Union _ -> true
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
   setter calls. An [:act] block is unit, so a trailing [return] is dropped: in
   OCaml [on_act] and [on_success] evaluate identically and a trailing
   [return true] is a no-op success, while the setters (also unit) run last. *)
let act_stmts stmts calls =
  let stmts =
    match List.rev stmts with
    | Types.Return _ :: rest -> List.rev rest
    | _ -> stmts
  in
  stmts @ calls

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
let on_success_stmts stmts calls =
  match List.rev stmts with
  | Types.If (cond, then_, else_opt) :: before when ends_in_return then_ ->
      let else_ =
        match else_opt with Some e -> e | None -> [ Types.Return Types.true_ ]
      in
      calls @ List.rev before @ [ Types.If (cond, then_, Some else_) ]
  | Types.Return _ :: _ -> calls @ stmts
  | _ -> stmts @ calls @ [ Types.Return Types.true_ ]

(* Attach the setter [calls] to a field's action, keeping any user action. *)
let with_setter_calls (Types.Field f) calls =
  let action =
    if is_bitfield f.field_typ then
      (* Bitfields: :act fires per sub-field during coalesced parsing *)
      match f.action with
      | None -> Types.Act calls
      | Some (Types.Act stmts) | Some (Types.Success stmts) ->
          Types.Act (act_stmts stmts calls)
    else
      (* Scalars and byte-size fields: :on-success *)
      match f.action with
      | None -> Types.Success (calls @ [ Types.Return Types.true_ ])
      | Some (Types.Success stmts) ->
          Types.Success (on_success_stmts stmts calls)
      | Some (Types.Act stmts) -> Types.Act (act_stmts stmts calls)
  in
  Types.Field { f with action = Some action }

let bytes_setter schema_name : setter_info =
  {
    name = schema_name ^ "SetBytes";
    val_typ = Types.Pack_typ (Types.Uint32 Types.Little);
  }

(* {2 The plug}

   One slot per named field of the entrypoint. A codec that embeds another
   codec (as a field, under an [optional], or in a [nested] region, seen
   through [map] / [where]) also gets one slot per scalar of every embedded
   codec and one [<path>_present] flag per optional, at any depth, each named by
   its path: the field names from the entrypoint down, joined with underscores,
   the names [Wire_ocaml.scanner] gives its outputs. The embedded structs and
   the optional casetypes over them take the [ctx] parameter, and their fields
   call the entrypoint's setters with the slot's index, exactly as the
   entrypoint's own fields do. A codec reached inside an array, a repeat or a
   casetype body is not flattened: it keeps its own scope and is validated, its
   fields are not extracted. An optional integer or float of an embedded codec,
   under [map] or [enum] or bare, has a slot after its flag. An embedded byte
   span has no slot.

   The validator writes a slot only for what it parses: a presence flag is
   written on every validation, 1 or 0, but the fields under an absent optional
   are not written at all and keep the value the caller gave the plug. 3D gives
   a literal no big-endian type ([(UINT16BE) 0] is a [UINT16]), so the
   validator cannot pass a zero to a big-endian setter. Initialising the plug
   to zero before each validation, as the generated stubs do, makes them read 0,
   the value [Wire_ocaml.scanner] gives an absent field. *)

type slot = {
  member : string;
  index : int;
  typ : Types.packed_typ;
  bytes : bool;
}

type walk = {
  schema : string;
  next : int ref;
  slots : slot list ref;
  uses : (string, int) Hashtbl.t;
}

let plug_member name = Names.escape Names.c_keywords name
let ctx_struct = Types.struct_ "WireCtx" []
let ctx_param = Types.mutable_param "ctx" (Types.struct_typ ctx_struct)

let new_slot w name typ ~bytes =
  let i = !(w.next) in
  incr w.next;
  w.slots := { member = name; index = i; typ; bytes } :: !(w.slots);
  i

(* How many fields a struct has in 3D: consecutive bitfields packed into one
   word are one, as in [coalesced_wire_size]. *)
let projected_field_count fields =
  let count, _ =
    List.fold_left
      (fun (n, group) (Types.Field f) ->
        match Types.unwrap_bits f.field_typ with
        | Some (base, order, width) -> (
            match group with
            | Some (b, o, used)
              when Bitfield.equal b base
                   && Types.equal_bit_order o order
                   && used + width <= Bitfield.total_bits base ->
                (n, Some (b, o, used + width))
            | _ -> (n + 1, Some (base, order, width)))
        | None -> (n + 1, None))
      (0, None) fields
  in
  count

(* EverParse reads a struct of one field as that field, and then refuses a
   field of that struct's type that carries an action when the struct's field
   carries one too ("this field cannot be read because it has multiple
   actions"). Such a codec is flattened only where its field carries no
   action of its own: under an optional or in a sized region, or as a nested
   field with no user action. *)
let flattens ~direct_ok (c : Types.struct_) =
  direct_ok || projected_field_count c.fields > 1

(* Whether a field's type embeds a codec on a path the plug flattens.
   [direct_ok] says whether a codec directly at this position may be
   flattened (see [flattens]). *)
let rec embeds : type a. direct_ok:bool -> a Types.typ -> bool =
 fun ~direct_ok -> function
  | Types.Codec { codec_struct; _ } -> flattens ~direct_ok codec_struct
  | Types.Optional { present = Types.Bool false; _ } -> false
  | Types.Optional { inner; _ } -> embeds ~direct_ok:true inner
  | Types.Single_elem { elem; _ } -> embeds ~direct_ok:true elem
  | Types.Map { inner; _ } -> embeds ~direct_ok inner
  | Types.Where { inner; _ } -> embeds ~direct_ok inner
  | _ -> false

(* Every codec a type reaches, flattened or not, counted by name. A codec
   reached more than once is renamed per path when it is flattened, since each
   copy calls the setters with its own indices. *)
let rec count_codecs : type a. (string, int) Hashtbl.t -> a Types.typ -> unit =
 fun uses -> function
  | Types.Codec { codec_name; codec_struct; _ } ->
      let n = Option.value ~default:0 (Hashtbl.find_opt uses codec_name) in
      Hashtbl.replace uses codec_name (n + 1);
      List.iter
        (fun (Types.Field f) -> count_codecs uses f.field_typ)
        codec_struct.fields
  | Types.Optional { inner; _ } -> count_codecs uses inner
  | Types.Optional_or { inner; _ } -> count_codecs uses inner
  | Types.Single_elem { elem; _ } -> count_codecs uses elem
  | Types.Array { elem; _ } -> count_codecs uses elem
  | Types.Repeat { elem; _ } -> count_codecs uses elem
  | Types.Map { inner; _ } -> count_codecs uses inner
  | Types.Where { inner; _ } -> count_codecs uses inner
  | Types.Enum { base; _ } -> count_codecs uses base
  | Types.Apply { typ; _ } -> count_codecs uses typ
  | Types.Casetype { cases; _ } ->
      List.iter
        (fun (Types.Case_branch { cb_inner; _ }) -> count_codecs uses cb_inner)
        cases
  | _ -> ()

let presence_call w idx present =
  let value = Types.If_then_else (present, Types.Int 1, Types.Int 0) in
  Types.Extern_call
    ( w.schema ^ "SetU8",
      [ "ctx"; Fmt.str "(UINT32) %d" idx; Fmt.str "%a" Types.pp_expr value ] )

(* A scalar a setter takes by value, seen through [map] and [enum]: a
   whole-byte integer or a float. *)
let rec boxable : type a. a Types.typ -> bool = function
  | Types.Uint8 | Types.Uint16 _ | Types.Uint32 _ | Types.Uint64 _ -> true
  | Types.Int8 | Types.Int16 _ | Types.Int32 _ | Types.Int64 _ -> true
  | Types.Float32 _ | Types.Float64 _ -> true
  | Types.Map { inner; _ } -> boxable inner
  | Types.Enum { base; _ } -> boxable base
  | _ -> false

(* The scalar [t] of the optional field at [path] as a codec of that one
   field, whose setter fills the slot [path]. 3D reads an optional scalar as a
   byte region, which no action can read; a region of this codec parses its
   field, and so sets it, where the optional is present. *)
let boxed : type a. walk -> path:string -> a Types.typ -> a Types.typ =
 fun w ~path t ->
  let idx = new_slot w path (Types.Pack_typ t) ~bytes:false in
  let c = Codec.v ("Some_" ^ path) Fun.id Codec.[ Field.v "v" t $ Fun.id ] in
  let s = Codec.to_struct c in
  let calls = [ setter_call w.schema t "v" idx None ] in
  let codec_struct =
    {
      s with
      params = s.params @ [ ctx_param ];
      fields = List.map (fun f -> with_setter_calls f calls) s.fields;
    }
  in
  let size = Codec.min_wire_size c in
  Types.Codec
    {
      codec_name = s.name;
      codec_decode = Codec.embed_decode_ctx c;
      codec_validate = Codec.embed_validate_ctx c;
      codec_validate_sized = Codec.embed_validate_sized_ctx c;
      codec_encode = Codec.embed_encode_ctx c;
      codec_fixed_size = Some size;
      codec_min_size = size;
      codec_size_of = (fun _ _ _ _ -> size);
      codec_size_of_value = Codec.size_of_value_ctx c;
      codec_field_readers = Codec.field_readers_ctx c;
      codec_returns_slice = false;
      codec_struct;
    }

let box_optional : type a.
    walk -> path:string -> a Types.typ -> a Types.typ option =
 fun w ~path -> function
  | Types.Optional ({ inner; _ } as o) when boxable inner ->
      Some (Types.Optional { o with inner = boxed w ~path inner })
  | _ -> None

(* One field of the entrypoint ([top]) or of an embedded codec at [prefix]: its
   own slot (an embedded byte span has none), its presence flag, then the
   slot of the scalar an embedded optional holds, or the slots of the codecs
   it embeds, threaded through its type. *)
let rec flatten_field w ~top ~nested ~prefix byte_off (Types.Field f as field) =
  match f.field_name with
  | None -> field
  | Some name ->
      let bytes = is_byte_field f.field_typ in
      let own =
        if top || not bytes then
          let idx =
            new_slot w (prefix ^ name) (Types.Pack_typ f.field_typ) ~bytes
          in
          [ setter_call w.schema f.field_typ name idx byte_off ]
        else []
      in
      let presence =
        match f.field_typ with
        | Types.Optional { present = Types.Bool false; _ } -> []
        | Types.Optional { present; _ } when nested ->
            let idx =
              new_slot w
                (prefix ^ name ^ "_present")
                (Types.Pack_typ Types.Uint8) ~bytes:false
            in
            [ presence_call w idx present ]
        | _ -> []
      in
      let calls = own @ presence in
      let direct_ok = calls = [] && Option.is_none f.action in
      let path = prefix ^ name in
      let field_typ =
        match if own = [] then box_optional w ~path f.field_typ else None with
        | Some t -> t
        | None when nested -> thread w ~direct_ok ~path f.field_typ
        | None -> f.field_typ
      in
      let field = Types.Field { f with field_typ } in
      if calls = [] then field else with_setter_calls field calls

and thread : type a.
    walk -> direct_ok:bool -> path:string -> a Types.typ -> a Types.typ =
 fun w ~direct_ok ~path typ ->
  match typ with
  | Types.Codec c when flattens ~direct_ok c.codec_struct ->
      let codec_name =
        if Hashtbl.find_opt w.uses c.codec_name = Some 1 then c.codec_name
        else c.codec_name ^ "_" ^ path
      in
      let s = c.codec_struct in
      let fields =
        List.map
          (flatten_field w ~top:false ~nested:true ~prefix:(path ^ "_") None)
          s.fields
      in
      let codec_struct =
        { s with name = codec_name; params = s.params @ [ ctx_param ]; fields }
      in
      Types.Codec { c with codec_name; codec_struct }
  | Types.Optional ({ present = Types.Bool false; _ } as o) -> Types.Optional o
  | Types.Optional o ->
      Types.Optional { o with inner = thread w ~direct_ok:true ~path o.inner }
  | Types.Single_elem e ->
      Types.Single_elem { e with elem = thread w ~direct_ok:true ~path e.elem }
  | Types.Map m ->
      Types.Map { m with inner = thread w ~direct_ok ~path m.inner }
  | Types.Where x ->
      Types.Where { x with inner = thread w ~direct_ok ~path x.inner }
  | t -> t

(* The entrypoint's fields with their setter calls, and the plug's slots in
   index order. Byte offsets are static while every field before them has a
   fixed size, as before. Every named field of the entrypoint has a setter,
   so a codec directly in one flattens only if [flattens] allows it. *)
let flatten schema (s : Types.struct_) =
  let nested =
    List.exists
      (fun (Types.Field f) ->
        Option.is_some f.field_name && embeds ~direct_ok:false f.field_typ)
      s.fields
  in
  let uses = Hashtbl.create 8 in
  List.iter (fun (Types.Field f) -> count_codecs uses f.field_typ) s.fields;
  let w = { schema; next = ref 0; slots = ref []; uses } in
  let off = ref (Some 0) in
  let fields =
    List.map
      (fun (Types.Field f as field) ->
        let field' = flatten_field w ~top:true ~nested ~prefix:"" !off field in
        (off :=
           match (!off, Types.field_wire_size f.field_typ) with
           | Some o, Some n -> Some (o + n)
           | _ -> None);
        field')
      s.fields
  in
  let slots = List.rev !(w.slots) in
  (* A path can spell a field's own name ([ip_src] beside [ip.src]): refuse it,
     as the plug cannot hold both. Spellings that differ but escape alike are
     refused where the plug is written ([Wire_3d.check_plug_names]). *)
  let seen = Hashtbl.create 16 in
  List.iter
    (fun sl ->
      if Hashtbl.mem seen sl.member then
        Fmt.invalid_arg
          "Everparse: in %S, two fields flatten to the plug member %S; rename \
           one of them"
          s.name sl.member;
      Hashtbl.add seen sl.member ())
    slots;
  (nested, fields, slots)

let slot_setter schema sl =
  if sl.bytes then bytes_setter schema
  else
    let (Types.Pack_typ t) = sl.typ in
    setter_of schema t

let collect_extern_setters schema_name slots =
  let seen = Hashtbl.create 8 in
  let u32 = Types.Uint32 Types.Little in
  List.filter_map
    (fun sl ->
      let si = slot_setter schema_name sl in
      if Hashtbl.mem seen si.name then None
      else begin
        Hashtbl.add seen si.name ();
        let (Types.Pack_typ val_typ) = si.val_typ in
        Some
          (Types.extern_fn si.name
             [ ctx_param; Types.param "idx" u32; Types.param "v" val_typ ]
             Types.Unit)
      end)
    slots

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
  (* Equal index bounds share one synthesised typedef, so emit each name once;
     two different conditions under one name are refused. *)
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun field ->
      match synth_of field with
      | Some (elt_var, cond) ->
          let synth = Types.synth_name_of_elt_var elt_var in
          if
            Types.claim_name seen ~kind:"refined bytes" synth
              (Fmt.str "%a" Types.pp_expr cond)
          then
            let elt_field = Types.field elt_var ~constraint_:cond Types.uint8 in
            Some (Types.typedef (Types.struct_ synth [ elt_field ]))
          else None
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

(* The FFI declarations, and whether embedded codecs were flattened into the
   plug: their structs then name [WireCtx] and the setters, which must be
   declared ahead of them. *)
let ffi_decls (s : Types.struct_) : bool * Types.decl list =
  let s = { s with fields = List.map rewrite_absent_optional s.fields } in
  (* Extern declarations for the callback mechanism *)
  let ctx_decl = Types.typedef ~extern_:true ctx_struct in
  (* Indices follow the ORIGINAL declaration order. The idx baked into each
     Extern_call is preserved through the bit-group reorder the 3D printer
     applies ([Types.pp_struct]), so WireSet callbacks still populate the
     original field slot. *)
  let nested, parse_fields, slots = flatten s.name s in
  let parse_struct =
    Types.param_struct s.name (s.params @ [ ctx_param ]) ?where:s.where
      parse_fields
  in
  let parse_decl = Types.typedef ~entrypoint:true parse_struct in
  let extern_decls = collect_extern_setters s.name slots in
  let refined_decls = refined_byte_typedefs s in
  (nested, [ ctx_decl ] @ extern_decls @ refined_decls @ [ parse_decl ])

(* Documentation / pure-validator projection: the same structural 3D as
   [ffi_decls] but without the FFI scaffolding. No [WireCtx] extern, no
   [WireSet*] setters, no extraction action injected on each field. Keeps the
   struct, bitfields, [where] clause, refined-byte typedefs, enums, and
   casetypes -- everything that describes the wire format. Reads as a protocol
   spec, and 3d.exe still compiles it to a real (validator-only) C parser with
   no FFI. *)
let standalone_decls ?doc (s : Types.struct_) : Types.decl list =
  let s = { s with fields = List.map rewrite_absent_optional s.fields } in
  let parse_struct =
    Types.param_struct s.name s.params ?where:s.where s.fields
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
          match Types.unwrap_bits f.field_typ with
          | Some (base, order, width) -> (
              match open_base with
              | Some b
                when Bitfield.equal b base
                     && Option.equal Types.equal_bit_order bit_order
                          (Some order)
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
  let nested, decls = ffi_decls s in
  let m = Types.module_ decls in
  (* [Types.module_] declares the sub-codecs and casetypes ahead of every
     declaration; flattened ones take [ctx] and call the setters, so those go
     first. *)
  let m =
    if not nested then m
    else
      let is_ffi = function
        | Types.Typedef { extern_ = true; _ } | Types.Extern_fn _ -> true
        | _ -> false
      in
      let ffi, rest = List.partition is_ffi m.decls in
      { m with decls = ffi @ rest }
  in
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

let ascii_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let ascii_digit c = c >= '0' && c <= '9'

(* Even with an extension, these basenames name devices rather than ordinary
   files on Windows. Reject them case-insensitively so generated sources remain
   portable and a checkout cannot behave differently from the machine that
   produced it. *)
let reserved_device_name name =
  let name = String.uppercase_ascii name in
  match name with
  | "CON" | "PRN" | "AUX" | "NUL" -> true
  | _ when String.length name = 4 ->
      let prefix = String.sub name 0 3 in
      (prefix = "COM" || prefix = "LPT") && name.[3] >= '1' && name.[3] <= '9'
  | _ -> false

let validate_output_name name =
  let len = String.length name in
  let valid_start c = ascii_letter c || c = '_' in
  let valid_tail c = valid_start c || ascii_digit c in
  let rec valid_from i =
    i = len || (valid_tail name.[i] && valid_from (i + 1))
  in
  if
    len = 0
    || (not (valid_start name.[0]))
    || (not (valid_from 1))
    || reserved_device_name name
  then
    Fmt.invalid_arg
      "Everparse: invalid output name %S; expected an ASCII identifier \
       matching [A-Za-z_][A-Za-z0-9_]* and not a reserved device name"
      name

let filename (s : t) =
  validate_output_name s.name;
  String.capitalize_ascii s.name ^ ".3d"

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

(* A member holds what its setter is passed: a value as wide as the integer
   under any [map], [where], [enum] or optional, or the 32-bit offset
   [<Name>SetBytes] hands out. *)
let plug_field (s : t) sl =
  let setter = slot_setter s.name sl in
  let (Types.Pack_typ val_typ) = setter.val_typ in
  let c_type = Types.c_type_of val_typ in
  {
    name = sl.member;
    idx = sl.index;
    c_type;
    setter = setter.name;
    val_c_type = c_type;
  }

let plug_fields s =
  match s.source with
  | None -> []
  | Some src ->
      let _, _, slots = flatten s.name src in
      List.map (plug_field s) slots

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

let equal_field_action_form a b =
  match (a, b) with
  | No_action, No_action | On_act, On_act | On_success, On_success -> true
  | (No_action | On_act | On_success), _ -> false

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
  let outputs = List.map (fun s -> (s, filename s)) schemas in
  List.iter
    (fun (s, file) -> Types.to_3d_file (Filename.concat outdir file) s.module_)
    outputs

(* The entrypoint marker and the doc comment say how a typedef is used, not what
   it is: the same struct projects with [entrypoint] set when it is packed as a
   codec of its own and clear when it is reached through another codec's field,
   and only the codec-derived projection carries the doc. Strip both before
   comparing, so a sub-codec shared between a packed codec and its parent is one
   declaration rather than a conflict. [output] and [extern_] stay in: they
   change what the declaration is, not what it is for. *)
let decl_shape = function
  | Types.Typedef t -> Types.Typedef { t with entrypoint = false; doc = None }
  | d -> d

(* Identity of a declaration for merge purposes: the 3D its shape renders to on
   its own. [Types.decl] holds closures (codec and map encode/decode pairs), so
   polymorphic equality raises on it, and the rendered text is exactly what the
   merged file would say about the type. Rendering is deterministic: its only
   render-time global, the anonymous-field counter, is reset per struct.
   A declaration with no 3D projection raises the same message every time, so
   two identical ones still compare equal and the projection error surfaces at
   emit time as before, not as a spurious collision. *)
let decl_identity d =
  match Types.to_3d ~enum_as_type:true (Types.module_ [ decl_shape d ]) with
  | text -> Ok text
  | exception Invalid_argument msg -> Error msg

let decl_name = function
  | Types.Typedef { struct_ = { name; _ }; _ } -> Some name
  | Types.Enum_decl { name; _ } | Types.Casetype_decl { name; _ } -> Some name
  | Types.Define { name; _ } | Types.Extern_fn { name; _ } -> Some name
  | _ -> None

(* The entrypoint marker and the doc comment a merged typedef should end up
   with: the union over every copy of it. A sub-codec that one schema packs as
   its own codec and another reaches through a field is declared once, and this
   is what keeps that one declaration's validator and prose whichever schema
   happened to contribute it first. *)
let typedef_roles (ts : t list) =
  let roles = Hashtbl.create 16 in
  let note d =
    match (decl_name d, d) with
    | Some n, Types.Typedef { entrypoint; doc; _ } ->
        let seen_entry, seen_doc =
          Option.value (Hashtbl.find_opt roles n) ~default:(false, None)
        in
        let doc = match seen_doc with None -> doc | kept -> kept in
        Hashtbl.replace roles n (seen_entry || entrypoint, doc)
    | _ -> ()
  in
  List.iter (fun (t : t) -> List.iter note t.module_.decls) ts;
  roles

let with_role roles d =
  match (decl_name d, d) with
  | Some n, Types.Typedef t ->
      let entrypoint, doc =
        Option.value (Hashtbl.find_opt roles n) ~default:(t.entrypoint, t.doc)
      in
      Types.Typedef { t with entrypoint; doc }
  | _ -> d

(* Merge several clean (doc) schemas into one module: union their decls, keeping
   the first definition of each named typedef / enum / casetype so a type shared
   across codecs (a common enum, a refined-byte element) is emitted once.
   Dependency order survives because each input module already lists a type
   before the typedef that uses it, and the shared type keeps its first slot.
   Two schemas declaring different types under one name cannot both be honoured
   by a single merged spec, so that is rejected rather than resolved silently. *)
let merge ~name (ts : t list) : t =
  let seen = Hashtbl.create 16 in
  let roles = typedef_roles ts in
  let keep owner d =
    match decl_name d with
    | None -> true
    | Some n -> (
        let identity = decl_identity d in
        match Hashtbl.find_opt seen n with
        | None ->
            Hashtbl.add seen n (owner, identity);
            true
        | Some (first_owner, first_identity) ->
            if first_identity = identity then false
            else
              Fmt.invalid_arg
                "Everparse.write: schemas %S and %S both declare %S with \
                 different definitions; rename one of them"
                first_owner owner n)
  in
  let decls =
    List.fold_left
      (fun acc (t : t) ->
        List.fold_left
          (fun acc d -> if keep t.name d then d :: acc else acc)
          acc t.module_.decls)
      [] ts
    |> List.rev
    |> List.map (with_role roles)
  in
  { name; module_ = Types.module_ decls; wire_size = None; source = None }

let write_standalone ~outdir ~name (ts : t list) =
  validate_output_name name;
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

  type int_slot = Types.int_slot = { width : int; endian : Types.endian }

  type field_seed = Types.field_seed = {
    field : string;
    slot : int_slot;
    values : int64 list;
  }

  let int_slots = Types.int_slots
  let field_seeds = Types.field_seeds
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
