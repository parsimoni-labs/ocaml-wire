module Tiny.Proof

friend EverParse3d.Prelude
friend Tiny

open WireGen.Lib
open WireGen.Lib3d
module LP = LowParse.Spec.Base
module P = WireGen.OCaml
module U8 = FStar.UInt8
module U16 = FStar.UInt16

(* Printed from the 3D projection: where each output lies in the value. *)
let outputs (v:Tiny.type____WireTiny ()) : P.int & P.int =
  let (a, b) = v in (U8.v a, U16.v b)

let spec (s:bytes) : GTot (option (P.int & P.int)) =
  match LP.parse (Tiny.parser____WireTiny ()) s with
  | None -> None
  | Some (v, _) -> Some (outputs v)

let answer (#r:Type) (invalid:unit -> r) (valid:P.int -> P.int -> r) (o:option (P.int & P.int)) : r =
  match o with None -> invalid () | Some (a, b) -> valid a b

#push-options "--z3rlimit 200 --fuel 1 --ifuel 1"
let record_is_3d (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{Tiny.Scan.window buf base stop /\ stop >= 0})
    (invalid:unit -> r) (valid:P.int -> P.int -> r)
  : Lemma (Tiny.Scan.fold_record buf base stop invalid valid ==
           answer invalid valid (spec (Seq.slice buf base (base + stop))))
  = assert (Tiny.Scan.fold_record buf base stop invalid valid ==
            answer invalid valid (spec (Seq.slice buf base (base + stop))))
      by (WireGen.Tac.unfold_3d [`%Tiny.Scan.fold_record; `%spec; `%answer;
            `%Tiny.parser____WireTiny; `%Tiny.def'____WireTiny; `%Tiny.def____WireTiny])
#pop-options

let fold_is_3d #r buf off len invalid valid =
  if off < 0 || len < 0 || off + len > P.length buf then ()
  else record_is_3d buf off len invalid valid
