type 'a t = {
  name : string;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
}

let v name ?constraint_ ?action typ = { name; typ; constraint_; action }
let ref f = Types.Ref f.name
let name f = f.name
let typ f = f.typ
let constraint_ f = f.constraint_
let action f = f.action

let to_decl f =
  Types.field f.name ?constraint_:f.constraint_ ?action:f.action f.typ
