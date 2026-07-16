type 'a t = {
  name : string;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
  doc : string option;
}

type 'a anon = { anon_typ : 'a Types.typ }

let pp ppf f = Fmt.pf ppf "%s" f.name

let combine a b =
  match (a, b) with
  | None, c | c, None -> c
  | Some a, Some b -> Some (Types.And (a, b))

let rec has_int64_slot : type a. a Types.typ -> bool =
 fun typ ->
  let open Types in
  match typ with
  | Uint64 _ | Int64 _ -> true
  | Where { inner; _ } -> has_int64_slot inner
  | Optional { inner; _ } -> has_int64_slot inner
  | Optional_or { inner; _ } -> has_int64_slot inner
  (* [build_populate] fills the int64 slot with the raw pre-map wire value
     (via [encode]) and recurses [Enum] through its base, so a constraint over
     the raw uint64 of a [map]-decoded or enum field is well defined. *)
  | Map { inner; _ } -> has_int64_slot inner
  | Enum { base; _ } -> has_int64_slot base
  | _ -> false

let reject_no_int64_slot ~combinator name typ =
  if not (has_int64_slot typ) then
    Fmt.invalid_arg
      "Wire.Field.%s: field %S does not have a full-width int64 validation slot"
      combinator name

let v name ?constraint_ ?self_constraint ?self_int64 ?action ?doc typ =
  if Option.is_some self_int64 then
    reject_no_int64_slot ~combinator:"v ?self_int64" name typ;
  let constraint_ =
    constraint_
    |> combine
         (Option.map (fun f -> f (Types.Ref (Types.I, name))) self_constraint)
    |> combine
         (Option.map (fun f -> f (Types.Ref (Types.I64, name))) self_int64)
  in
  { name; typ; constraint_; action; doc }

(* Field decorations: produce a field directly from a typ + optional/
   repeat metadata. Exposing these only at the field level keeps
   [Wire.optional]/[Wire.repeat] off the typ-level surface so the
   resulting decoration cannot be nested inside [array]/[where]/etc.
   where 3D has no projection for it. *)
let optional name ?constraint_ ?self_constraint ?self_int64 ?action ~present typ
    =
  v name ?constraint_ ?self_constraint ?self_int64 ?action
    (Types.optional present typ)

let optional_or name ?constraint_ ?self_constraint ?self_int64 ?action ~present
    ~default typ =
  v name ?constraint_ ?self_constraint ?self_int64 ?action
    (Types.optional_or present ~default typ)

(* A sub-codec ending in a greedy field ([all_bytes] / [all_zeros]) reads "the
   rest of the buffer" as its tail, so it cannot be iterated as a repeat element
   (the first element would consume everything). *)
let codec_ends_greedy (s : Types.struct_) =
  match List.rev s.fields with
  | Types.Field f :: _ -> Types.is_greedy f.field_typ
  | [] -> false

(* Types decodable as a casetype case body inside a repeat: exactly the set
   [Codec.read_elem] handles. Unlike a bare repeat element, a lone [bits] field
   and a bounded [zeroterm_at_most] are allowed here, because the enclosing
   casetype packs them after the tag with a fixed footprint. A nested [array] /
   [nested] region or an [optional] has no fixed footprint and is rejected. *)
let rec is_repeat_case_body : type a. a Types.typ -> bool =
 fun typ ->
  let open Types in
  match typ with
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ | Int8 | Int16 _ | Int32 _
  | Int64 _ | Float32 _ | Float64 _ | Unit | Zeroterm | Bits _ ->
      true
  | Uint_var { size = Int _; _ } -> true
  | Byte_array { size = Int _ } | Byte_slice { size = Int _ } -> true
  | Zeroterm_at_most { size = Int _ } -> true
  | Codec { codec_struct; _ } -> not (codec_ends_greedy codec_struct)
  | Casetype { cases; _ } ->
      List.for_all
        (fun (Case_branch { cb_inner; _ }) -> is_repeat_case_body cb_inner)
        cases
  | Map { inner; _ } -> is_repeat_case_body inner
  | Where { inner; _ } -> is_repeat_case_body inner
  | Enum { base; _ } -> is_repeat_case_body base
  | _ -> false

(* An element [repeat]/[repeat_seq] can both project to 3D and decode one
   element at a time: a fixed-width scalar / byte span, a NUL-terminated
   string, or a self-bounded sub-codec / casetype. [Map] / [Where] / [Enum]
   are transparent wrappers, so look through them. A casetype is repeatable
   only when every case body is itself decodable as a repeat element
   ([is_repeat_case_body]). A sub-codec must also be [nz] (have a fixed-size
   field): EverParse projects [repeat] as a byte-budget list of the codec's
   named struct, and a list over a possibly-empty element does not extract.
   Everything else (a sub-byte [bits] field, a refined or at-most byte span
   whose per-element validation the byte-budget loop does not run, greedy
   [all_zeros], a nested [array] / [nested]) has no clean per-element 3D
   projection. *)
let rec is_repeat_element : type a. a Types.typ -> bool =
 fun typ ->
  let open Types in
  match typ with
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ | Int8 | Int16 _ | Int32 _
  | Int64 _ | Float32 _ | Float64 _ | Uint_var _ | Zeroterm ->
      true
  (* [Unit] is 0-width: a byte-budget list of it carries no bytes and projects to
     a zero-size element EverParse refuses to extract, like the [array] case. *)
  | Byte_array { size = Int _ } | Byte_slice { size = Int _ } -> true
  | Codec { codec_struct; _ } ->
      (not (codec_ends_greedy codec_struct)) && Types.struct_nz codec_struct
  | Casetype { cases; _ } ->
      List.for_all
        (fun (Case_branch { cb_inner; _ }) -> is_repeat_case_body cb_inner)
        cases
  | Map { inner; _ } -> is_repeat_element inner
  | Where { inner; _ } -> is_repeat_element inner
  | Enum { base; _ } -> is_repeat_element base
  | _ -> false

let reject_unprojectable_repeat ~combinator typ =
  if not (is_repeat_element typ) then
    Fmt.invalid_arg
      "Wire.%s: element type does not project to 3D as a repeat element -- the \
       byte-budget loop only supports fixed-width scalars and byte spans, \
       NUL-terminated strings, sub-codecs, and casetypes."
      combinator

let repeat name ?constraint_ ?self_constraint ?self_int64 ?action ~size typ =
  reject_unprojectable_repeat ~combinator:"repeat" typ;
  v name ?constraint_ ?self_constraint ?self_int64 ?action
    (Types.repeat ~size typ)

let repeat_seq name ?constraint_ ?self_constraint ?self_int64 ?action ~seq ~size
    typ =
  reject_unprojectable_repeat ~combinator:"repeat_seq" typ;
  v name ?constraint_ ?self_constraint ?self_int64 ?action
    (Types.repeat_seq seq ~size typ)

let anon typ = { anon_typ = typ }
let ref f = Types.Ref (Types.I, f.name)
let int f = Types.Ref (Types.I, f.name)

let int64 f =
  reject_no_int64_slot ~combinator:"int64" f.name f.typ;
  Types.Ref (Types.I64, f.name)

let name f = f.name
let typ f = f.typ
let constraint_ f = f.constraint_
let action f = f.action
let doc f = f.doc

type packed = Named : 'a t -> packed | Anon : 'a anon -> packed

let decl_of_packed = function
  | Named f ->
      Types.field f.name ?constraint_:f.constraint_ ?action:f.action ?doc:f.doc
        f.typ
  | Anon a -> Types.anon_field a.anon_typ
