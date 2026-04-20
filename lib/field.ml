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

let anon typ = { anon_typ = typ }
let ref f = Types.Ref f.name
let name f = f.name
let typ f = f.typ
let constraint_ f = f.constraint_
let action f = f.action

type packed = Named : 'a t -> packed | Anon : 'a anon -> packed

let to_decl = function
  | Named f ->
      Types.field f.name ?constraint_:f.constraint_ ?action:f.action f.typ
  | Anon a -> Types.anon_field a.anon_typ
