module Tiny.Flat

(* The parser 3d.exe generated for Tiny.3d, read field by field. *)

open WireGen.Lib
module P = WireGen.OCaml

let value = EverParse3d.Interpreter.as_type (Tiny.def'____WireTiny ())

val spec (s:bytes) : GTot (option value)

val answer (#r:Type) (invalid:unit -> r) (valid:P.int -> P.int -> r) (o:option value) : r

let flat (#r:Type) (invalid:unit -> r) (valid:P.int -> P.int -> r) (s:bytes) : r =
  let len = Seq.length s in
  if len < 1 then invalid () else
  let a = be s 0 1 in
  if len < 3 then invalid () else
  let b = be s 1 2 in
  if not (b >= 4) then invalid () else
  valid a b

val flat_is_3d (#r:Type) (invalid:unit -> r) (valid:P.int -> P.int -> r) (s:bytes)
  : Lemma (answer invalid valid (spec s) == flat invalid valid s)
