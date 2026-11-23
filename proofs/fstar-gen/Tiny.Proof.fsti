module Tiny.Proof

module P = WireGen.OCaml

(* The outputs of the value the parser 3d.exe generated for Tiny.3d
   parses at the front of [s], none where it rejects. *)
val spec (s:WireGen.Lib.bytes) : GTot (option (P.int & P.int))

(* The scanner's fold rejects a bad window, and on a good one answers what
   3d.exe's parser answers on the window's bytes. *)
val fold_is_3d (#r:Type) (buf:P.buf) (off len:P.int) (invalid:unit -> r) (valid:P.int -> P.int -> r)
  : Lemma (Tiny.Scan.fold buf off len invalid valid ==
           (if off < 0 || len < 0 || off + len > P.length buf then invalid ()
            else match spec (Seq.slice buf off (off + len)) with
                 | None -> invalid ()
                 | Some (a, b) -> valid a b))
