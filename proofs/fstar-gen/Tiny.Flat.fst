module Tiny.Flat

friend EverParse3d.Prelude
friend Tiny

open WireGen.Lib
open WireGen.Lib3d
module P = WireGen.OCaml
module LP = LowParse.Spec.Base

let spec (s:bytes) : GTot (option value) =
  match LP.parse (Tiny.parser____WireTiny ()) s with
  | None -> None
  | Some (v, _) -> Some v

let answer #r invalid valid o =
  match o with
  | None -> invalid ()
  | Some v -> let (a, b) = v in valid (P.of_u8 a) (P.of_u16 b)

#push-options "--z3rlimit 200 --fuel 1 --ifuel 1"
let flat_is_3d #r invalid valid s =
  assert (answer invalid valid (spec s) == flat invalid valid s)
    by (WireGen.Tac.unfold_flat [`%spec; `%answer; `%flat;
          `%Tiny.parser____WireTiny; `%Tiny.def'____WireTiny; `%Tiny.def____WireTiny])
#pop-options
