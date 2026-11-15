open Hardcaml
module S = Signal
module H = Wire_hardcaml

let same_result a b =
  S.reduce ~f:S.( &: )
    (S.(H.in_scope a ==: H.in_scope b)
    :: S.(H.accepted a ==: H.accepted b)
    :: S.(H.consumed a ==: H.consumed b)
    :: List.map2
         (fun (name, x) (other, y) ->
           if name <> other then invalid_arg "mismatched hardware scalar names";
           S.(x ==: y))
         (H.fields a) (H.fields b))
