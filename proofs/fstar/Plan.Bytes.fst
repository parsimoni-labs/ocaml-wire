module Plan.Bytes

(* A buffer of bytes read big-endian is a reader the shared words of the
   int64 step scanner may serve: a field taken out of a word of 8 bytes that
   holds it is the field read on its own, the premise
   Plan.Proof.scanner_correct takes of the reader when [words] is on. *)

open FStar.Mul
open Plan.Expr
open Plan.Scan
open Plan.Proof

module W = Plan.Word

type mem = int -> b:nat{b < 256}

(* The bytes from [o] on. *)
let from (b:mem) (o:int) : W.bytes = fun i -> b (o + i)

let read (b:mem) (o:int) (w:nat) : v:nat{v < pow2 (8 * w)} =
  W.be_bound (from b o) 0 w;
  W.be (from b o) 0 w

let reader_of (b:mem) : reader = read b

(* Reading from [a] at [d] is reading from [a + d]. *)
let rec be_from (b:mem) (a:int) (d:nat) (m:nat)
  : Lemma (ensures W.be (from b a) d m == W.be (from b (a + d)) 0 m) (decreases m)
  = if m = 0 then () else be_from b a d (m - 1)

let consistent_at (b:mem) (a k:int) (m:nat{1 <= m /\ m <= 7})
  : Lemma (requires a <= k /\ k + m <= a + 8)
          (ensures word_field (reader_of b a 8) (k - a) m == reader_of b k m)
  = W.word_field (from b a) 0 (k - a) m;
    be_from b a (k - a) m;
    assert (word_field (W.be (from b a) 0 8) (k - a) m == W.extract (W.be (from b a) 0 8) 0 (k - a) m)

let be_consistent (b:mem) : Lemma (consistent (reader_of b)) =
  introduce forall (a k:int) (m:nat{1 <= m /\ m <= 7}).
    (a <= k /\ k + m <= a + 8) ==> word_field (reader_of b a 8) (k - a) m == reader_of b k m
  with introduce _ ==> _ with _. consistent_at b a k m
