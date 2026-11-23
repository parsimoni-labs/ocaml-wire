module Ethernet.Proof

friend EverParse3d.Prelude
friend Ethernet

open WireGen.Lib
open WireGen.Lib3d
module LP = LowParse.Spec.Base
module P = WireGen.OCaml
module U16 = FStar.UInt16

let outputs (v:Ethernet.type____WireEthernet ()) : P.int =
  let (_dst, (_src, ether_type)) = v in U16.v ether_type

let spec (s:bytes) : GTot (option P.int) =
  match LP.parse (Ethernet.parser____WireEthernet ()) s with
  | None -> None
  | Some (v, _) -> Some (outputs v)

let answer (#r:Type) (invalid:unit -> r) (valid:P.int -> r) (o:option P.int) : r =
  match o with None -> invalid () | Some ether_type -> valid ether_type

#push-options "--z3rlimit 200 --fuel 1 --ifuel 1"
let record_is_3d (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{Ethernet.Scan.window buf base stop /\ stop >= 0})
    (invalid:unit -> r) (valid:P.int -> r)
  : Lemma (Ethernet.Scan.fold_record buf base stop invalid valid ==
           answer invalid valid (spec (Seq.slice buf base (base + stop))))
  = assert (Ethernet.Scan.fold_record buf base stop invalid valid ==
            answer invalid valid (spec (Seq.slice buf base (base + stop))))
      by (WireGen.Tac.unfold_3d [`%Ethernet.Scan.fold_record; `%spec; `%answer;
            `%Ethernet.parser____WireEthernet; `%Ethernet.def'____WireEthernet; `%Ethernet.def____WireEthernet])
#pop-options

let fold_is_3d #r buf off len invalid valid =
  if off < 0 || len < 0 || off + len > P.length buf then ()
  else record_is_3d buf off len invalid valid
