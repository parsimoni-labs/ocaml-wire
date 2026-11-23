module IPv4.Flat

(* The parser 3d.exe generated for IPv4.3d, read field by field: [flat]
   is printed from the 3D projection alone (field order, widths, bit
   positions, refinements), never from the emitter, so this lemma holds
   until the schema changes. *)

open WireGen.Lib
module P = WireGen.OCaml

(* The value 3d.exe's parser builds. *)
let value = EverParse3d.Interpreter.as_type (IPv4.def'____WireIPv4 ())

val spec (s:bytes) : GTot (option value)

val answer (#r:Type) (invalid:unit -> r) (valid:IPv4.Scan.valid_t r) (o:option value) : r

let flat (#r:Type) (invalid:unit -> r) (valid:IPv4.Scan.valid_t r) (s:bytes) : r =
  let len = Seq.length s in
  if len < 1 then invalid () else
  let bf0 = be s 0 1 in
  if not (P.bits bf0 0 4 >= 5 && P.bits bf0 4 4 = 4) then invalid () else
  if len < 2 then invalid () else
  let bf1 = be s 1 1 in
  if len < 4 then invalid () else
  let total_length = be s 2 2 in
  if not (total_length >= 20) then invalid () else
  if len < 6 then invalid () else
  let identification = be s 4 2 in
  if len < 8 then invalid () else
  let bf2 = be s 6 2 in
  if len < 9 then invalid () else
  let ttl = be s 8 1 in
  if len < 10 then invalid () else
  let protocol = be s 9 1 in
  if len < 12 then invalid () else
  let checksum = be s 10 2 in
  if len < 16 then invalid () else
  let src = be s 12 4 in
  if len < 20 then invalid () else
  let dst = be s 16 4 in
  valid (P.bits bf0 4 4) (P.bits bf0 0 4) (P.bits bf1 2 6) (P.bits bf1 0 2)
    total_length identification (P.bits bf2 13 3) (P.bits bf2 0 13)
    ttl protocol checksum src dst

val flat_is_3d (#r:Type) (invalid:unit -> r) (valid:IPv4.Scan.valid_t r) (s:bytes)
  : Lemma (answer invalid valid (spec s) == flat invalid valid s)
