module Tiny.Scan

(* fold_record and fold of the scanner Wire_ocaml writes for Tiny
   (gen/gen.exe; tiny_scan.ml), line for line, the OCaml operators and
   runtime calls written as calls into WireGen.OCaml. The window a walk
   reads is the one refinement a printer adds: [stop] bytes from [base] lie
   in the buffer, once [stop] is not negative. *)

module P = WireGen.OCaml

let window (buf:P.buf) (base:P.int) (stop:P.int) : prop =
  0 <= base /\ stop <= P.length buf - base

unfold
let fold_record (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{window buf base stop})
    (invalid:unit -> r) (valid:P.int -> P.int -> r) : r =
  if P.lt stop 3 then invalid () else
  let f1_a = P.u8 buf base in
  let f2_b = P.u16be buf (P.add base 1) in
  if not (P.ge f2_b 4) then invalid () else
  valid f1_a f2_b

let fold (#r:Type) (buf:P.buf) (off:P.int) (len:P.int)
    (invalid:unit -> r) (valid:P.int -> P.int -> r) : r =
  if P.lt off 0 || P.gt len (P.sub (P.length buf) off) then invalid () else
  let base = off in
  let stop = len in
  fold_record buf base stop invalid valid
