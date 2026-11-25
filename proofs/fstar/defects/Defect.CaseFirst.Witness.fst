module Defect.CaseFirst.Witness

(* Two cases, the first on an atom and the second always: a tree that runs
   the second case without testing the atom passes tree-case-first.sed's
   check, and runs it where the codec runs the first. This module only holds
   against that check. *)

open Plan.Tree

let cs : list formula = [ FAtom 0; FTrue ]
let t : choice = Run (Some 1)

let _ = assert_norm (tree_ok [] cs t)
let _ = assert_norm (run (fun _ -> true) t == Some 1 /\ first (fun _ -> true) cs 0 == Some 0)
