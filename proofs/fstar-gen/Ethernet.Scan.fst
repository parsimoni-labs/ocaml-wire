module Ethernet.Scan

(* fold_record and fold of the scanner Wire_ocaml writes for
   Headers.eth (ethernet_scan.ml), line for line. *)

module P = WireGen.OCaml

let window (buf:P.buf) (base:P.int) (stop:P.int) : prop =
  0 <= base /\ stop <= P.length buf - base

unfold
let fold_record (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{window buf base stop})
    (invalid:unit -> r) (valid:P.int -> r) : r =
  if P.lt stop 14 then invalid () else
  let f3_ether_type = P.u16be buf (P.add base 12) in
  valid f3_ether_type

let fold (#r:Type) (buf:P.buf) (off:P.int) (len:P.int)
    (invalid:unit -> r) (valid:P.int -> r) : r =
  if P.lt off 0 || P.gt len (P.sub (P.length buf) off) then invalid () else
  let base = off in
  let stop = len in
  fold_record buf base stop invalid valid
