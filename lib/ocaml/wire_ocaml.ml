open Wire.Private
open Layout
module T = Types
open Planner
open Emitter
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
    | exception Unsupported why -> invalid_arg why

  let nodes t = t.nodes
  let outputs t = t.outputs

  let select t names =
    match selected t names with
    | outputs -> outputs
    | exception Unsupported why -> invalid_arg why

  let live t outputs =
    let live = live t ~outputs:(List.map (fun (o : output) -> o.id) outputs) in
    fun v -> Sset.mem v live

  module Scope = struct
    type nonrec t = { ranges : (int * int) Smap.t; facts : facts }

    let v (plan : plan) = { ranges = plan.ranges; facts = no_facts }
    let facts s = s.facts

    let refine s conditions =
      let ranges, facts = refined s.ranges s.facts conditions in
      { ranges; facts }

    let range s e = range s.ranges e

    let checked s (e : int T.expr) =
      match e with
      | T.Add (a, b) -> not (fits s.ranges T.checked_add a b)
      | T.Sub (a, b) -> not (fits s.ranges T.checked_sub a b)
      | T.Mul (a, b) -> not (fits s.ranges T.checked_mul a b)
      | _ -> false

    let nonnegative s e = nonnegative_in s.ranges s.facts e
  end

  let conditions f = List.map (fun c -> c.cond) f.checks
  let range (t : t) e = range t.ranges e

  type nonrec tree = tree = { tid : int; node : tnode }
  and tnode = tnode = Leaf of int option | Branch of bool T.expr * tree * tree

  let gates facts nodes = gate_run ~lower:(simplify facts) facts nodes
  let decide = decide

  let shared tree =
    let shared, case_shared, _ = reach tree in
    (shared, case_shared)
end
