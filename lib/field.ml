type 'a t = {
  name : string option;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
}

let v name ?constraint_ ?action typ =
  { name = Some name; typ; constraint_; action }

let anon ?action typ = { name = None; typ; constraint_ = None; action }

let ref f =
  match f.name with
  | Some name -> Types.Ref name
  | None -> invalid_arg "Field.ref: anonymous field has no name"

let name f =
  match f.name with
  | Some n -> n
  | None -> invalid_arg "Field.name: anonymous field"

let name_opt f = f.name
let typ f = f.typ
let constraint_ f = f.constraint_
let action f = f.action

type packed = Pack : 'a t -> packed

let to_decl f =
  match f.name with
  | Some name ->
      Types.field name ?constraint_:f.constraint_ ?action:f.action f.typ
  | None -> Types.anon_field f.typ
