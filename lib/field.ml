type 'a t = {
  name : string;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
  mutable reader : (bytes -> int -> 'a) option;
  mutable writer : (bytes -> int -> 'a -> unit) option;
}

type 'a anon = { anon_typ : 'a Types.typ }

let v name ?constraint_ ?action typ =
  { name; typ; constraint_; action; reader = None; writer = None }

let anon typ = { anon_typ = typ }
let ref f = Types.Ref f.name
let name f = f.name
let typ f = f.typ
let constraint_ f = f.constraint_
let action f = f.action
let set_reader f r = f.reader <- Some r
let set_writer f w = f.writer <- Some w
let get_reader f = f.reader
let get_writer f = f.writer

type packed = Named : 'a t -> packed | Anon : 'a anon -> packed

let to_decl = function
  | Named f ->
      Types.field f.name ?constraint_:f.constraint_ ?action:f.action f.typ
  | Anon a -> Types.anon_field a.anon_typ
