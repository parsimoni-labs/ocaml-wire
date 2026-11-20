module Defect.RegionWalk.Buggy

(* 57535e21 "codec: bound a region's size walk by the bytes it was handed",
   before the fix: the walk that sizes a record inside a region read a
   length field the data places by checking it against the buffer's end, so
   it read bytes past the region's end, which the parse was never handed.
   Nothing was accepted on them, but the size reported came from them. *)

open FStar.Mul
open Plan.Expr

(* A region of [n] bytes at [start] in a buffer of [buflen] bytes. *)
type region = { buflen : nat; start : nat; n : nat }

(* Where the walk's bound check stops it. *)
let input_end (r:region) : nat = r.buflen

(* A read of [w] bytes at [o] the bound check lets through. *)
let read_allowed (r:region) (o:int) (w:nat) : bool = 0 <= o && o + w <= input_end r

let r0 : region = { buflen = 16; start = 0; n = 4 }
let _ = assert_norm (read_allowed r0 6 2 = true)

(* Every read the walk makes is inside the region it was handed, which lies
   in the buffer. *)
let read_in_region (r:region) (o:int) (w:nat)
  : Lemma (requires r.start + r.n <= r.buflen /\ r.start <= o /\ read_allowed r o w)
          (ensures o + w <= r.start + r.n)
  = ()
