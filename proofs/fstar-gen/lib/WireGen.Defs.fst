module WireGen.Defs

(* The meaning of each primitive WireGen.OCaml keeps opaque to Z3, as a
   rewrite rule on its application. A proof that a scanner is the 3D parser
   loads this module; the check of the scanner itself does not, and so
   keeps to the primitives' types. *)

open WireGen.Lib
module P = WireGen.OCaml
module U = FStar.UInt

let land_eq (a b:P.int)
  : Lemma (P.land a b == P.of_pattern (U.logand #63 (P.pattern a) (P.pattern b)))
          [SMTPat (P.land a b)]
  = P.land_def a b

let lsr_eq (a:P.int) (k:nat{k < 63})
  : Lemma (P.lsr a k == P.of_pattern (P.pattern a / pow2 k)) [SMTPat (P.lsr a k)]
  = P.lsr_def a k

let u8_eq (b:P.buf) (o:P.int{0 <= o /\ o + 1 <= P.length b})
  : Lemma (P.u8 b o == be b o 1) [SMTPat (P.u8 b o)]
  = P.u8_def b o

let u16be_eq (b:P.buf) (o:P.int{0 <= o /\ o + 2 <= P.length b})
  : Lemma (P.u16be b o == be b o 2) [SMTPat (P.u16be b o)]
  = P.u16be_def b o

let i32be_eq (b:P.buf) (o:P.int{0 <= o /\ o + 4 <= P.length b})
  : Lemma (P.i32be b o == be b o 4) [SMTPat (P.i32be b o)]
  = P.i32be_def b o

let int32_to_int_eq (x:P.int32)
  : Lemma (P.int32_to_int x == (if x >= pow2 31 then x - pow2 32 else x))
          [SMTPat (P.int32_to_int x)]
  = P.int32_to_int_def x

let i64be_eq (b:P.buf) (o:P.int{0 <= o /\ o + 8 <= P.length b})
  : Lemma (P.i64be b o == be b o 8) [SMTPat (P.i64be b o)]
  = P.i64be_def b o

let shift_right_logical_eq (w:P.int64) (k:nat{k < 64})
  : Lemma (P.shift_right_logical w k == w / pow2 k) [SMTPat (P.shift_right_logical w k)]
  = P.shift_right_logical_def w k

let to_int_eq (w:P.int64)
  : Lemma (P.to_int w == P.of_pattern (w % pow2 63)) [SMTPat (P.to_int w)]
  = P.to_int_def w
