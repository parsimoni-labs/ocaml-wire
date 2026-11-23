module Ethernet.Proof

module P = WireGen.OCaml

val spec (s:WireGen.Lib.bytes) : GTot (option P.int)

val fold_is_3d (#r:Type) (buf:P.buf) (off len:P.int) (invalid:unit -> r) (valid:P.int -> r)
  : Lemma (Ethernet.Scan.fold buf off len invalid valid ==
           (if off < 0 || len < 0 || off + len > P.length buf then invalid ()
            else match spec (Seq.slice buf off (off + len)) with
                 | None -> invalid ()
                 | Some ether_type -> valid ether_type))
