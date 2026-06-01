type 'a t = {
  name : string;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
}

type 'a anon = { anon_typ : 'a Types.typ }

let pp ppf f = Fmt.pf ppf "%s" f.name

let v name ?constraint_ ?self_constraint ?action typ =
  let constraint_ =
    match (constraint_, self_constraint) with
    | c, None -> c
    | None, Some f -> Some (f (Types.Ref name))
    | Some c, Some f -> Some (Types.And (c, f (Types.Ref name)))
  in
  { name; typ; constraint_; action }

(* Field decorations: produce a field directly from a typ + optional/
   repeat metadata. Exposing these only at the field level keeps
   [Wire.optional]/[Wire.repeat] off the typ-level surface so the
   resulting decoration cannot be nested inside [array]/[where]/etc.
   where 3D has no projection for it. *)
let optional name ?constraint_ ?self_constraint ?action ~present typ =
  v name ?constraint_ ?self_constraint ?action (Types.optional present typ)

let optional_or name ?constraint_ ?self_constraint ?action ~present ~default typ
    =
  v name ?constraint_ ?self_constraint ?action
    (Types.optional_or present ~default typ)

(* An element [repeat]/[repeat_seq] can both project to 3D and decode one
   element at a time: a fixed-width scalar / byte span, a NUL-terminated
   string, or a self-bounded sub-codec / casetype. [Map] / [Where] / [Enum]
   are transparent wrappers, so look through them. Everything else (a sub-byte
   [bits] field, a refined or at-most byte span whose per-element validation
   the byte-budget loop does not run, greedy [all_zeros], a nested [array] /
   [nested]) has no clean per-element 3D projection. *)
let rec is_repeat_element : type a. a Types.typ -> bool =
 fun typ ->
  let open Types in
  match typ with
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ | Int8 | Int16 _ | Int32 _
  | Int64 _ | Float32 _ | Float64 _ | Uint_var _ | Unit | Zeroterm ->
      true
  | Byte_array { size = Int _ } | Byte_slice { size = Int _ } -> true
  | Codec _ | Casetype _ -> true
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

let repeat name ?constraint_ ?self_constraint ?action ~size typ =
  reject_unprojectable_repeat ~combinator:"repeat" typ;
  v name ?constraint_ ?self_constraint ?action (Types.repeat ~size typ)

let repeat_seq name ?constraint_ ?self_constraint ?action ~seq ~size typ =
  reject_unprojectable_repeat ~combinator:"repeat_seq" typ;
  v name ?constraint_ ?self_constraint ?action (Types.repeat_seq seq ~size typ)

let anon typ = { anon_typ = typ }
let ref f = Types.Ref f.name
let name f = f.name
let typ f = f.typ
let constraint_ f = f.constraint_
let action f = f.action

type packed = Named : 'a t -> packed | Anon : 'a anon -> packed

let decl_of_packed = function
  | Named f ->
      Types.field f.name ?constraint_:f.constraint_ ?action:f.action f.typ
  | Anon a -> Types.anon_field a.anon_typ
