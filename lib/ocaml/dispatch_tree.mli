(** The dispatch over a run of optional fields.

    A run of optional fields whose conditions exclude each other, such as the
    cases of a tag, is one dispatch: a decision tree over the atoms of their
    conditions, each tested at most once on a path, whose leaves run one case or
    none. A subtree reached twice is written once, in a local function, as is a
    case reached twice; the code after the run is written once too. Internal to
    {!Wire_ocaml}. *)

open Planner

type tree = { tid : int; node : tnode }
(** A decision tree over the atoms of the conditions of a run, whose leaves run
    one case or none. *)

and tnode = Leaf of int option | Branch of bool T.expr * tree * tree

val gate_run :
  lower:(bool T.expr -> bool T.expr) ->
  facts ->
  node list ->
  (bool T.expr * output * output list * node list) list * node list
(** [gate_run ~lower facts nodes] is the optional fields at the head of [nodes]
    whose conditions, as [lower] writes them, exclude each other, and the nodes
    after them. *)

val decide : facts -> (int * bool T.expr) list -> tree
(** [decide facts cases] is the decision tree that picks the first of [cases]
    whose condition holds, or none. A subtree reached from two paths is one
    value. *)

val reach : tree -> (tree -> bool) * (int -> bool) * int
(** [reach tree] tells which subtrees of [tree] are reached more than once,
    which cases are, and how many leaves are written, a shared case once. *)

val tree_depth : (tree -> bool) -> tree -> int
(** [tree_depth shared tree] is the conditions a path through [tree] opens, a
    subtree [shared] holds as one call. *)

val runs_none : tree -> bool
(** [runs_none tree] is [true] when some path through [tree] runs no case. *)

val hoisted :
  (bool T.expr * output * output list * node list) list ->
  tree ->
  int T.expr option
(** [hoisted gates tree] is the size of the span every case of [gates] starts
    with, when a case always runs: its bound is then checked once before the
    dispatch. *)

val choice : tree -> Decision.choice
(** [choice tree] is [tree] as a decision. *)
