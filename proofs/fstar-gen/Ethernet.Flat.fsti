module Ethernet.Flat

(* The parser 3d.exe generated for Ethernet.3d, read field by field. *)

open WireGen.Lib
module P = WireGen.OCaml

let value = EverParse3d.Interpreter.as_type (Ethernet.def'____WireEthernet ())

val spec (s:bytes) : GTot (option value)

val answer (#r:Type) (invalid:unit -> r) (valid:P.int -> r) (o:option value) : r

let flat (#r:Type) (invalid:unit -> r) (valid:P.int -> r) (s:bytes) : r =
  let len = Seq.length s in
  if len < 6 then invalid () else
  if len < 12 then invalid () else
  if len < 14 then invalid () else
  let ether_type = be s 12 2 in
  valid ether_type

val flat_is_3d (#r:Type) (invalid:unit -> r) (valid:P.int -> r) (s:bytes)
  : Lemma (answer invalid valid (spec s) == flat invalid valid s)
