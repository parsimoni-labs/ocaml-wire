module Defect.PrefixWrap.Fixed

(* The plan of Defect.PrefixWrap.Buggy is not well formed: its fused run is
   longer than Sys.max_string_length, the bound a record's fixed bytes keep
   (Codec_seal.add_min_size). A planner that refuses such a codec, or does
   not fuse past that bound, only makes plans [wf] accepts, and for those
   Plan.Proof.scanner_correct holds. *)

open FStar.Mul
open Plan.Expr
open Plan.Scan
open Plan.Proof

let run : nat = 33 * (pow2 57 - 10) + 1

let lim : var = 0
let plan : list node = [
  Prefix [ { id = 1; off = 0; width = 1 } ] 1;
  Span 2 3 (V 1);
  Prefix [ { id = 4; off = run - 1; width = 1 } ] run;
]

let refused () : Lemma (not (wf (start lim).sc plan)) = assert_norm (not (wf (start lim).sc plan))

(* Every plan [wf] accepts is read inside its window. *)
let scanner_safe (o:opts) (len:nat) (rd:reader) (en:env) (l:list node)
  : Lemma (requires len <= max_string_length /\ en lim == len /\ wf (start lim).sc l)
          (ensures ~ (Fault? (exec o rd (top len lim) (start lim) en l)))
  = scanner_correct o len lim rd en l
