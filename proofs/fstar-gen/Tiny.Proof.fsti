module Tiny.Proof

module P = WireGen.OCaml

(* The scanner's fold rejects a bad window, and on a good one answers what
   3d.exe's parser answers on the window's bytes. *)
val fold_is_3d (#r:Type) (buf:P.buf) (off len:P.int) (invalid:unit -> r) (valid:P.int -> P.int -> r)
  : Lemma (Tiny.Scan.fold buf off len invalid valid ==
           (if off < 0 || len < 0 || off + len > P.length buf then invalid ()
            else Tiny.Flat.answer invalid valid (Tiny.Flat.spec (Seq.slice buf off (off + len)))))
