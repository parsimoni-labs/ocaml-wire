module Plan.Tree

(* The decision tree of a dispatch (Dispatch_tree.decide, Decision.choice):
   a run of optional fields whose conditions exclude each other is written as
   one tree of tests of the atoms of their conditions, each leaf running one
   case or none. The codec evaluates each field's condition in turn; the
   tree tests atoms. [tree_ok] is the check make plans runs on each
   dispatch's tree against the conditions the emitter decided it from
   (Decision.Dispatch's [conditions]), and [tree_first] proves that a tree
   it accepts runs, for every value of the atoms, the first case whose
   condition holds, and none exactly where none holds. *)

open FStar.List.Tot

(* An atom of a condition: a test the code evaluates as a boolean, such as
   [f14_ipv4_protocol = 6], numbered by its text. *)
type atom = nat

(* A condition, over its atoms: Dispatch_tree.decide splits [&&], [||] and
   [not] and tests anything else as an atom. *)
type formula =
  | FTrue
  | FFalse
  | FAtom : atom -> formula
  | FNot : formula -> formula
  | FAnd : formula -> formula -> formula
  | FOr : formula -> formula -> formula

(* Decision.choice: the case of that index, or none; or a test of an atom
   and where it leads when it holds and when it does not. *)
type choice =
  | Run : option nat -> choice
  | If : atom -> choice -> choice -> choice

let rec eval (s:atom -> bool) (f:formula) : bool =
  match f with
  | FTrue -> true
  | FFalse -> false
  | FAtom a -> s a
  | FNot g -> not (eval s g)
  | FAnd g h -> eval s g && eval s h
  | FOr g h -> eval s g || eval s h

(* The case the code runs when the atoms have the values [s]. *)
let rec run (s:atom -> bool) (t:choice) : option nat =
  match t with
  | Run x -> x
  | If a y n -> if s a then run s y else run s n

(* The case the codec runs: the first, from index [j], whose condition
   holds, or none. *)
let rec first (s:atom -> bool) (cs:list formula) (j:nat) : Tot (option nat) (decreases cs) =
  match cs with
  | [] -> None
  | c :: r -> if eval s c then Some j else first s r (j + 1)

(* {1 The check} *)

(* What the tests on a path decide of a condition: the atoms tested so far,
   with their outcome, decide it when every completion gives it the same
   value; [None] when they do not. *)
let rec decided (r:list (atom & bool)) (f:formula) : option bool =
  match f with
  | FTrue -> Some true
  | FFalse -> Some false
  | FAtom a -> assoc a r
  | FNot g -> (match decided r g with Some b -> Some (not b) | None -> None)
  | FAnd g h ->
    (match decided r g, decided r h with
     | Some false, _ | _, Some false -> Some false
     | Some true, Some true -> Some true
     | _ -> None)
  | FOr g h ->
    (match decided r g, decided r h with
     | Some true, _ | _, Some true -> Some true
     | Some false, Some false -> Some false
     | _ -> None)

(* A leaf at the end of the path [r] is right for the conditions [cs], from
   index [j]: a case [i] where the path decides its condition holds and every
   condition before it fails, none where it decides every condition fails. *)
let rec leaf_ok (r:list (atom & bool)) (cs:list formula) (x:option nat) (j:nat)
  : Tot bool (decreases cs) =
  match cs, x with
  | [], None -> true
  | [], Some _ -> false
  | c :: rest, Some i ->
    if j = i then decided r c = Some true
    else j < i && decided r c = Some false && leaf_ok r rest x (j + 1)
  | c :: rest, None -> decided r c = Some false && leaf_ok r rest x (j + 1)

(* Every leaf of [t], reached by the path [r], is right, and no atom is
   tested twice on a path. *)
let rec tree_ok (r:list (atom & bool)) (cs:list formula) (t:choice) : Tot bool (decreases t) =
  match t with
  | Run x -> leaf_ok r cs x 0
  | If a y n -> None? (assoc a r) && tree_ok ((a, true) :: r) cs y && tree_ok ((a, false) :: r) cs n

(* Some leaf of [t] runs no case. *)
let rec runs_none (t:choice) : bool =
  match t with
  | Run x -> None? x
  | If _ y n -> runs_none y || runs_none n

(* {1 What the check ensures} *)

(* The values [s] of the atoms agree with the outcomes on the path [r]. *)
let rec agrees (s:atom -> bool) (r:list (atom & bool)) : prop =
  match r with [] -> True | (a, b) :: r' -> s a == b /\ agrees s r'

let rec assoc_agrees (s:atom -> bool) (r:list (atom & bool)) (a:atom)
  : Lemma (requires agrees s r) (ensures (match assoc a r with Some b -> s a == b | None -> True))
  = match r with [] -> () | (a', _) :: r' -> if a = a' then () else assoc_agrees s r' a

(* What a path decides of a condition is its value. *)
let rec decided_sound (s:atom -> bool) (r:list (atom & bool)) (f:formula)
  : Lemma (requires agrees s r)
          (ensures (match decided r f with Some b -> eval s f == b | None -> True))
  = match f with
    | FTrue | FFalse -> ()
    | FAtom a -> assoc_agrees s r a
    | FNot g -> decided_sound s r g
    | FAnd g h | FOr g h -> decided_sound s r g; decided_sound s r h

let rec leaf_first (s:atom -> bool) (r:list (atom & bool)) (cs:list formula) (x:option nat) (j:nat)
  : Lemma (requires agrees s r /\ leaf_ok r cs x j) (ensures first s cs j == x) (decreases cs)
  = match cs with
    | [] -> ()
    | c :: rest ->
      decided_sound s r c;
      (match x with
       | Some i -> if j = i then () else leaf_first s r rest x (j + 1)
       | None -> leaf_first s r rest x (j + 1))

let rec tree_first_from (s:atom -> bool) (r:list (atom & bool)) (cs:list formula) (t:choice)
  : Lemma (requires agrees s r /\ tree_ok r cs t) (ensures run s t == first s cs 0) (decreases t)
  = match t with
    | Run x -> leaf_first s r cs x 0
    | If a y n -> if s a then tree_first_from s ((a, true) :: r) cs y else tree_first_from s ((a, false) :: r) cs n

(* The tree a dispatch writes runs, for every value of the atoms, the case
   the codec's walk of the conditions in order runs first: a case exactly
   where its condition holds and none before it does, none exactly where no
   condition holds. *)
let tree_first (s:atom -> bool) (cs:list formula) (t:choice)
  : Lemma (requires tree_ok [] cs t) (ensures run s t == first s cs 0)
  = tree_first_from s [] cs t

let rec first_none (s:atom -> bool) (cs:list formula) (j:nat)
  : Lemma (first s cs j == None <==> (forall c. memP c cs ==> not (eval s c))) (decreases cs)
  = match cs with [] -> () | _ :: r -> first_none s r (j + 1)

let rec first_some (s:atom -> bool) (cs:list formula) (j i:nat)
  : Lemma (requires first s cs j == Some i)
          (ensures j <= i /\ i < j + length cs /\ eval s (index cs (i - j)) /\
                   (forall (k:nat). k < i - j ==> not (eval s (index cs k))))
          (decreases cs)
  = match cs with
    | [] -> ()
    | c :: r -> if eval s c then () else first_some s r (j + 1) i

(* A tree with no leaf that runs none runs a case for every value of the
   atoms: some condition always holds, so a bound every case starts with may
   be checked once before the dispatch (Dispatch_tree.hoisted). *)
let rec no_none (s:atom -> bool) (t:choice)
  : Lemma (requires not (runs_none t)) (ensures Some? (run s t))
  = match t with Run _ -> () | If _ y n -> no_none s y; no_none s n

let always_runs (s:atom -> bool) (cs:list formula) (t:choice)
  : Lemma (requires tree_ok [] cs t /\ not (runs_none t))
          (ensures exists c. memP c cs /\ eval s c)
  = tree_first s cs t; no_none s t; first_none s cs 0

(* The atoms the code tests, in order, when the atoms have the values [s]. *)
let rec tested (s:atom -> bool) (t:choice) : list atom =
  match t with
  | Run _ -> []
  | If a y n -> a :: tested s (if s a then y else n)

let rec tested_fresh (s:atom -> bool) (r:list (atom & bool)) (cs:list formula) (t:choice) (a:atom)
  : Lemma (requires tree_ok r cs t /\ Some? (assoc a r)) (ensures not (mem a (tested s t))) (decreases t)
  = match t with
    | Run _ -> ()
    | If b y n ->
      let r' = (b, s b) :: r in
      tested_fresh s r' cs (if s b then y else n) a

let rec tested_once_from (s:atom -> bool) (r:list (atom & bool)) (cs:list formula) (t:choice)
  : Lemma (requires tree_ok r cs t) (ensures noRepeats (tested s t)) (decreases t)
  = match t with
    | Run _ -> ()
    | If a y n ->
      let r' = (a, s a) :: r in
      tested_fresh s r' cs (if s a then y else n) a;
      tested_once_from s r' cs (if s a then y else n)

(* The code evaluates each atom at most once, whatever the values. *)
let tested_once (s:atom -> bool) (cs:list formula) (t:choice)
  : Lemma (requires tree_ok [] cs t) (ensures noRepeats (tested s t))
  = tested_once_from s [] cs t
