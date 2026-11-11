open Wire.Private
open Layout
module T = Types
open Planner
open Record_module
open Scan_module

(* The generator is built in phases, one module each, in dependency order:
   [Planner] (a codec read as a plan of nodes), [Emitter] (the checks and reads
   of a plan as OCaml text), [Record_module] (the record, its decoder, encoder
   and accessors) and [Scan_module] (the scanner over structured codecs). This
   module holds the interface. *)

type target = [ `OCaml | `OxCaml ]
type typ = Planner.typ = Typ : 'a Wire.typ -> typ

let ident = ident
let ml = ml
let bridge = bridge
let scanner_fields = scanner_fields
let scanner = scanner

(* {1 The plan, for other backends} *)

module Plan = struct
  type nonrec kind = kind = Constraint | Enum of int list | Tag
  type nonrec check = check = { cond : bool T.expr; kind : kind }

  type nonrec output = output = {
    id : string;
    name : string;
    is_bool : bool;
    wire_width : int;
    signed : bool;
  }

  type nonrec field = field = {
    id : string;
    name : string;
    label : string;
    offset : int;
    store : store;
    checks : check list;
    enum : (string * int list) option;
    map : string option;
    output : output option;
  }

  type nonrec node = node =
    | Prefix of field list * int
    | Span of int T.expr
    | Gate of bool T.expr * output * output list * node list
    | Region of int T.expr * bool * node list
    | Test of bool T.expr

  type t = plan

  module Facts = struct
    type t = facts

    let empty = no_facts
    let assume = assume
    let simplify = simplify
  end

  let v c =
    match scan_plan c with
    | plan -> plan
    | exception Unsupported why -> refuse ~codec:(Gen.to_struct c).name why

  let nodes t = t.nodes
  let outputs t = t.outputs
  let conditions f = List.map (fun c -> c.cond) f.checks
  let range (t : t) e = range t.ranges e
end
