module Ethernet.Flat

friend EverParse3d.Prelude
friend Ethernet

open WireGen.Lib
open WireGen.Lib3d
module LP = LowParse.Spec.Base
module P = WireGen.OCaml

let spec (s:bytes) : GTot (option value) =
  match LP.parse (Ethernet.parser____WireEthernet ()) s with
  | None -> None
  | Some (v, _) -> Some v

let answer #r invalid valid o =
  match o with
  | None -> invalid ()
  | Some v -> let (_dst, (_src, ether_type)) = v in valid (P.of_u16 ether_type)

#push-options "--z3rlimit 200 --fuel 1 --ifuel 1"
let flat_is_3d #r invalid valid s =
  assert (answer invalid valid (spec s) == flat invalid valid s)
    by (WireGen.Tac.unfold_flat [`%spec; `%answer; `%flat;
          `%Ethernet.parser____WireEthernet; `%Ethernet.def'____WireEthernet; `%Ethernet.def____WireEthernet])
#pop-options
