module Defect.RegionWalk.Fixed

(* 57535e21 after the fix: every buffer walk carries where its input stops
   (Input_end), narrowed to a region's end and never past the buffer's. *)

open FStar.Mul
open Plan.Expr

type region = { buflen : nat; start : nat; n : nat }

(* Input_end.narrow: whichever limit comes first. *)
let narrow (limit:nat) (start n:nat) : nat = if start + n < limit then start + n else limit

let input_end (r:region) : nat = narrow r.buflen r.start r.n

let read_allowed (r:region) (o:int) (w:nat) : bool = 0 <= o && o + w <= input_end r

let r0 : region = { buflen = 16; start = 0; n = 4 }
let _ = assert_norm (read_allowed r0 6 2 = false)

let read_in_region (r:region) (o:int) (w:nat)
  : Lemma (requires r.start + r.n <= r.buflen /\ r.start <= o /\ read_allowed r o w)
          (ensures o + w <= r.start + r.n)
  = ()

(* The two properties test/test_input_end.ml pins: a limit is never past the
   buffer, and narrowing never moves it outwards. *)
let narrow_ok (limit start n:nat)
  : Lemma (narrow limit start n <= limit /\ narrow limit start n <= start + n) = ()
