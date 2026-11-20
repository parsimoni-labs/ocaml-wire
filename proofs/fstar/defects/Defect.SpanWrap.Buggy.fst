module Defect.SpanWrap.Buggy

(* e4be2c6f "codec: reject expression arithmetic overflow", before the fix:
   Eval computed a size expression with OCaml's wrapping [+], [-] and [*].
   The test of 4170c4d1 sizes a span by [len * len] and sets [len] to
   [max_int]: the product wraps to 1, so a 9-byte buffer held the 8-byte
   length and "the" span. *)

open FStar.Mul
open Plan.Expr

(* OCaml's 63-bit wrap-around. *)
let wrap (x:int) : int = ((x + pow2 62) % pow2 63) - pow2 62

let rec ev_wrap (en:env) (e:expr) : int =
  match e with
  | Lit n -> wrap n
  | V v -> wrap (en v)
  | Add a b -> wrap (ev_wrap en a + ev_wrap en b)
  | Sub a b -> wrap (ev_wrap en a - ev_wrap en b)
  | Mul a b -> wrap (ev_wrap en a * ev_wrap en b)
  | Shl a k -> wrap (ev_wrap en a * pow2 k)

(* The codec's span check at offset [p] of [len] bytes. *)
let span_accepts (len p:int) (en:env) (s:expr) : bool =
  let k = ev_wrap en s in
  k >= 0 && p + k <= len

(* The counterexample: [len = max_int], [len * len] wraps to 1. *)
let cex_env : env = fun _ -> max_int
let cex_size : expr = Mul (V 0) (V 0)
let _ = assert_norm (span_accepts 9 8 cex_env cex_size = true)
let _ = assert_norm (eval cex_env cex_size > 9)

(* An accepted span's bytes are in the buffer. *)
let span_in_buffer (len p:int) (en:env) (s:expr)
  : Lemma (requires span_accepts len p en s)
          (ensures 0 <= eval en s /\ p + eval en s <= len)
  = ()
