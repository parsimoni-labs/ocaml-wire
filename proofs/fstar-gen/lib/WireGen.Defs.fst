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

let int64_mul_eq (a b:P.int64)
  : Lemma (P.int64_mul a b == (a * b) % pow2 64) [SMTPat (P.int64_mul a b)]
  = P.int64_mul_def a b

let int64_logand_eq (a b:P.int64)
  : Lemma (P.int64_logand a b == U.logand #64 a b) [SMTPat (P.int64_logand a b)]
  = P.int64_logand_def a b

(* The [int64] primitives are those of Plan.Values, which the plan model's
   proofs of the step and OxCaml walks use: what is assumed of OCaml's
   [Int64] is stated once. *)
module V = Plan.Values

let signed64_is_plan (x:P.int64) : Lemma (P.signed64 x == V.signed x)
  = P.pow2_64 ()

let int64_of_int_is_plan (x:P.int) : Lemma (P.int64_of_int x == V.of_int x)
  = P.pow2_64 ();
    if x >= 0 then FStar.Math.Lemmas.small_mod x (pow2 64)
    else begin
      FStar.Math.Lemmas.lemma_mod_plus x 1 (pow2 64);
      FStar.Math.Lemmas.small_mod (x + pow2 64) (pow2 64)
    end

let int64_add_is_plan (a b:P.int64) : Lemma (P.int64_add a b == V.add64 a b)
  = P.pow2_64 ();
    if a + b >= pow2 64 then begin
      FStar.Math.Lemmas.lemma_mod_plus (a + b - pow2 64) 1 (pow2 64);
      FStar.Math.Lemmas.small_mod (a + b - pow2 64) (pow2 64)
    end
    else FStar.Math.Lemmas.small_mod (a + b) (pow2 64)

let int64_sub_is_plan (a b:P.int64) : Lemma (P.int64_sub a b == V.sub64 a b)
  = P.pow2_64 ();
    if a - b < 0 then begin
      FStar.Math.Lemmas.lemma_mod_plus (a - b) 1 (pow2 64);
      FStar.Math.Lemmas.small_mod (a - b + pow2 64) (pow2 64)
    end
    else FStar.Math.Lemmas.small_mod (a - b) (pow2 64)

let int64_mul_is_plan (a b:P.int64) : Lemma (P.int64_mul a b == V.mul64 a b)
  = P.int64_mul_def a b

(* A value of the 63-bit int held as an [int64] is itself
   (Plan.Values.signed_of_int). *)
let signed64_of_int (x:P.int)
  : Lemma (P.signed64 (P.int64_of_int x) == x) [SMTPat (P.signed64 (P.int64_of_int x))]
  = P.int_range ();
    Plan.Expr.wrap_id x;
    int64_of_int_is_plan x;
    signed64_is_plan (P.int64_of_int x);
    V.signed_of_int x

let shift_left_eq (w:P.int64) (k:nat{k < 64})
  : Lemma (P.shift_left w k == (w * pow2 k) % pow2 64) [SMTPat (P.shift_left w k)]
  = P.shift_left_def w k

let shift_left_is_plan (w:P.int64) (k:nat{k < 64}) : Lemma (P.shift_left w k == V.shift_left64 w k)
  = P.shift_left_def w k
