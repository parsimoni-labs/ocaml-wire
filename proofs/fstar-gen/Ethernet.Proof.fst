module Ethernet.Proof

open WireGen.Lib
module P = WireGen.OCaml

#push-options "--z3rlimit 200 --fuel 1 --ifuel 1"
let record_is_flat (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{Ethernet.Scan.window buf base stop /\ stop >= 0})
    (invalid:unit -> r) (valid:P.int -> r)
  : Lemma (Ethernet.Scan.fold_record buf base stop invalid valid ==
           Ethernet.Flat.flat invalid valid (Seq.slice buf base (base + stop)))
  = assert (Ethernet.Scan.fold_record buf base stop invalid valid ==
            Ethernet.Flat.flat invalid valid (Seq.slice buf base (base + stop)))
      by (WireGen.Tac.unfold_scan [`%Ethernet.Scan.fold_record; `%Ethernet.Flat.flat])
#pop-options

let fold_is_3d #r buf off len invalid valid =
  if off < 0 || len < 0 || off + len > P.length buf then ()
  else begin
    record_is_flat buf off len invalid valid;
    Ethernet.Flat.flat_is_3d invalid valid (Seq.slice buf off (off + len))
  end
