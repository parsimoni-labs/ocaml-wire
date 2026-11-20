module Defect.SpanWrap.Fixed

(* e4be2c6f after the fix: a size expression is evaluated with checked
   arithmetic and an overflow rejects the record, as Plan.Scan.spec does. *)

open Plan.Expr

let span_accepts (len p:int) (en:env) (s:expr) : bool =
  match ev en s with
  | Some k -> k >= 0 && p + k <= len
  | None -> false

let cex_env : env = fun _ -> max_int
let cex_size : expr = Mul (V 0) (V 0)
let _ = assert_norm (span_accepts 9 8 cex_env cex_size = false)

let span_in_buffer (len p:int) (en:env) (s:expr)
  : Lemma (requires span_accepts len p en s)
          (ensures 0 <= eval en s /\ p + eval en s <= len)
  = ev_eval en s
