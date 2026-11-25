open Wire.Private
module T = Types
open Planner

type tree = { tid : int; node : tnode }
and tnode = Leaf of int option | Branch of bool T.expr * tree * tree

let decide facts conds =
  let memo = Hashtbl.create 16 in
  let count = ref 0 in
  let mk key node =
    match Hashtbl.find_opt memo key with
    | Some t -> t
    | None ->
        incr count;
        let t = { tid = !count; node = node () } in
        Hashtbl.replace memo key t;
        t
  in
  let leaf i = mk (Fmt.str "leaf %a" Fmt.(option int) i) (fun () -> Leaf i) in
  let rec choose facts c yes no =
    match simplify facts c with
    | T.Bool true -> yes facts
    | T.Bool false -> no facts
    | T.And (a, b) -> choose facts a (fun f -> choose f b yes no) no
    | T.Or (a, b) -> choose facts a yes (fun f -> choose f b yes no)
    | T.Not a -> choose facts a no yes
    | atom ->
        let y = yes (assume facts atom true) in
        let n = no (assume facts atom false) in
        incr count;
        { tid = !count; node = Branch (atom, y, n) }
  in
  let rec among facts conds =
    let residual =
      List.filter_map
        (fun (i, c) ->
          match simplify facts c with T.Bool false -> None | c -> Some (i, c))
        conds
    in
    let k =
      String.concat ";"
        (List.map (fun (i, c) -> Fmt.str "%d:%s" i (key c)) residual)
    in
    match residual with
    | [] -> leaf None
    | (i, c) :: rest ->
        mk k (fun () ->
            (choose facts c (fun _ -> leaf (Some i)) (fun f -> among f rest))
              .node)
  in
  among facts conds

(* Whether the conditions [a] and [b], [b] after [a], exclude each other, so
   [b] need not be evaluated where [a] holds. *)
let exclusive facts a b =
  simplify (assume facts a true) b = T.Bool false
  || simplify (assume facts b true) a = T.Bool false

(* The optional fields at the head of [nodes] whose conditions, as [lower]
   writes them, exclude each other, and the nodes after them. *)
let gate_run ~lower facts nodes =
  let rec go acc = function
    | Gate (c, p, o, body) :: rest
      when let c = lower c in
           List.for_all (fun (c', _, _, _) -> exclusive facts c' c) acc ->
        go ((lower c, p, o, body) :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  go [] nodes

(* Whether a subtree of [tree] is reached more than once, and a case; and
   how many leaves are written, a shared case once. *)
let reach tree =
  let refs = Hashtbl.create 16 and cases = Hashtbl.create 16 in
  let rec count t =
    let n = Option.value ~default:0 (Hashtbl.find_opt refs t.tid) in
    Hashtbl.replace refs t.tid (n + 1);
    match t.node with
    | Leaf (Some i) ->
        Hashtbl.replace cases i
          (1 + Option.value ~default:0 (Hashtbl.find_opt cases i))
    | Leaf None -> ()
    | Branch (_, a, b) ->
        if n = 0 then begin
          count a;
          count b
        end
  in
  count tree;
  let shared t =
    match t.node with
    | Branch _ -> Hashtbl.find refs t.tid > 1
    | Leaf _ -> false
  in
  let rec leaves t =
    match t.node with
    | Leaf None -> 1
    | Leaf (Some _) -> 0
    | Branch (_, a, b) -> leaves a + leaves b
  in
  ( shared,
    (fun i -> Option.value ~default:0 (Hashtbl.find_opt cases i) > 1),
    leaves tree + Hashtbl.length cases )

(* The conditions a path through [tree] opens, a shared subtree as one
   call. *)
let rec tree_depth shared t =
  match t.node with
  | Leaf _ -> 0
  | Branch (_, a, b) ->
      let sub t = if shared t then 0 else tree_depth shared t in
      1 + max (sub a) (sub b)

(* Whether some path through [tree] runs no case. *)
let rec runs_none t =
  match t.node with
  | Leaf None -> true
  | Leaf (Some _) -> false
  | Branch (_, a, b) -> runs_none a || runs_none b

(* The size of the span every case of [gates] starts with, when a case
   always runs: its bound is then checked once before the dispatch. *)
let hoisted gates tree =
  let first (_, _, _, body) =
    match body with
    | (Region (size, _, _) | Span size) :: _ -> Some size
    | _ -> None
  in
  match List.map first gates with
  | Some size :: rest
    when List.for_all (( = ) (Some (key size))) (List.map (Option.map key) rest)
         && not (runs_none tree) ->
      Some size
  | _ -> None

(* [tree] as a decision. *)
let rec choice t =
  match t.node with
  | Leaf i -> Decision.Run i
  | Branch (atom, yes, no) -> Decision.If (atom, choice yes, choice no)
