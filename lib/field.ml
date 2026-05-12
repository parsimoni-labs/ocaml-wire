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

let repeat name ?constraint_ ?self_constraint ?action ~size typ =
  v name ?constraint_ ?self_constraint ?action (Types.repeat ~size typ)

let repeat_seq name ?constraint_ ?self_constraint ?action ~seq ~size typ =
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
