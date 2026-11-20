module Defect.WordUnchecked.Fixed

(* 6a0fce0b after the fix: Bitfield.check_word raises before any read when
   [off < 0 || off > Bytes.length buf - n]. The test subtracts, so it does
   not overflow for any offset and length in the native int. *)

open Plan.Expr

noeq type outcome =
  | Value
  | Raised
  | Fault

let unchecked_word (len:nat) (o:int) (n:nat) : outcome =
  if o < 0 || o + n > len then Fault else Value

let check_word (len:nat) (o:int) (n:nat) : bool = o < 0 || o > len - n

let get (len:nat) (off byte_off:int) (n:nat) : outcome =
  let o = off + byte_off in
  if check_word len o n then Raised else unchecked_word len o n

let _ = assert_norm (get 4 2 0 4 == Raised)

let get_safe (len:nat) (off byte_off:int) (n:nat) : Lemma (~ (Fault? (get len off byte_off n))) = ()

(* [len - n] is in the native int for a buffer length and a word size. *)
let check_no_overflow (len:nat) (n:nat) : Lemma (requires len <= max_int /\ n <= 8) (ensures in_int (len - n)) = ()
