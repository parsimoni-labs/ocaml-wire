module Defect.SumConst.Fixed

(* Planner.sum_add with the constant added inside the handler, as
   Plan.Sum.sum_add has it:

     match
       let terms = List.fold_left ... a.terms b.terms in
       { terms; const = T.checked_add a.const b.const }
     with
     | s -> Some s
     | exception Wire.Parse_error _ -> None *)

open FStar.List.Tot
open Plan.Expr
open Plan.Sum

let checked_add (a b:int) : Pure int (requires in_int (a + b)) (ensures fun r -> r == a + b) = a + b

let sum_add (a b:sum) : option sum =
  match add_terms a.terms b.terms with
  | None -> None
  | Some ts -> if in_int (a.const + b.const) then Some { terms = ts; const = checked_add a.const b.const } else None

let same (a b:sum) : Lemma (sum_add a b == Plan.Sum.sum_add a b) = ()
