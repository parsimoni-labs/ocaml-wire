type 'a t = {
  name : string;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
}

type 'a anon = { anon_typ : 'a Types.typ }

let pp ppf f = Fmt.pf ppf "%s" f.name
let v name ?constraint_ ?action typ = { name; typ; constraint_; action }
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
