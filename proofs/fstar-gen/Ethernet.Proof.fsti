module Ethernet.Proof

module P = WireGen.OCaml

(* The scanner's fold rejects a bad window, and on a good one answers what
   3d.exe's parser answers on the window's bytes. *)
val fold_is_3d (#r:Type) (buf:P.buf) (off len:P.int) (invalid:unit -> r) (valid:P.int -> r)
  : Lemma (Ethernet.Scan.fold buf off len invalid valid ==
           (if off < 0 || len < 0 || off + len > P.length buf then invalid ()
            else Ethernet.Flat.answer invalid valid (Ethernet.Flat.spec (Seq.slice buf off (off + len)))))
