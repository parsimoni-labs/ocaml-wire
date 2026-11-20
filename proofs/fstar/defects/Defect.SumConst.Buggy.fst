module Defect.SumConst.Buggy

(* Planner.sum_add as the O1 lane writes it (bf2c2866):

     match List.fold_left (... T.checked_add c c' ...) a.terms b.terms with
     | terms -> Some { terms; const = T.checked_add a.const b.const }
     | exception Wire.Parse_error _ -> None

   The handler of a [match ... with exception] covers the scrutinee only, so
   the constant's [checked_add] raises out of [sum_add], out of
   Emitter.cancelled and out of Wire_ocaml.scanner. Here [checked_add] is a
   function that cannot return on overflow, which is what raising means to
   a total caller: F* must prove the constant does not overflow, and cannot.
   A codec of uint8 fields [A] and [B], then byte slices of [A + 2^61] and
   [B + 2^61] bytes, then a uint8, makes Wire_ocaml.scanner raise
   Wire_runtime.Errors.Parse_error at bf2c2866, where 1e318ce8 generates the
   scanner: the second span's cancelled bound adds the two constants. *)

open FStar.Mul
open FStar.List.Tot
open Plan.Expr
open Plan.Sum

(* T.checked_add: raises on overflow. *)
let checked_add (a b:int) : Pure int (requires in_int (a + b)) (ensures fun r -> r == a + b) = a + b

let sum_add (a b:sum) : option sum =
  match add_terms a.terms b.terms with
  | None -> None
  | Some ts -> Some { terms = ts; const = checked_add a.const b.const }
