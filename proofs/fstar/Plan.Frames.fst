module Plan.Frames

(* The frame loop of fold_frames (Scan_module.pp_frames): after the guard

     stride <= 0 || off < 0 || len < 0 || off > Bytes.length buf
     || len > Bytes.length buf - off  ->  invalid_arg

   it walked [for _ = 1 to len / stride] with a cursor besides, and since
   73591041 walks [while !cursor <= last] with [last = off + len - stride].
   Each frame is checked as a window of [stride] bytes at its base, so
   Plan.Proof applies to it with [len = stride]. *)

open FStar.Mul
open Plan.Expr

let guard (buflen off len stride:int) : bool =
  not (stride <= 0 || off < 0 || len < 0 || off > buflen || len > buflen - off)

(* The bases the counted loop visits. *)
let rec counted (off stride:int) (i n:nat) : Tot (list int) (decreases (if i < n then n - i else 0)) =
  if i < n then (off + i * stride) :: counted off stride (i + 1) n else []

(* The bases the cursor loop visits. *)
let rec cursor (c last:int) (stride:pos) : Tot (list int) (decreases (if c <= last then last - c + 1 else 0)) =
  if c <= last then c :: cursor (c + stride) last stride else []

let step_iff (i:nat) (len:nat) (stride:pos)
  : Lemma ((i + 1) * stride <= len <==> i < len / stride)
  = FStar.Math.Lemmas.lemma_div_mod len stride;
    if (i + 1) * stride <= len then
      FStar.Math.Lemmas.lemma_div_le ((i + 1) * stride) len stride
    else if i < len / stride then
      FStar.Math.Lemmas.lemma_mult_le_right stride (i + 1) (len / stride)

let rec same_frames (off len:nat) (stride:pos) (i:nat)
  : Lemma (ensures cursor (off + i * stride) (off + len - stride) stride == counted off stride i (len / stride))
          (decreases (if i < len / stride then len / stride - i else 0))
  = step_iff i len stride;
    if i < len / stride then same_frames off len stride (i + 1) else ()

(* The new loop visits the frames the old one did, [len / stride] of them. *)
let cursor_loop (off len:nat) (stride:pos)
  : Lemma (cursor off (off + len - stride) stride == counted off stride 0 (len / stride))
  = same_frames off len stride 0

let rec counted_in (off len:nat) (stride:pos) (i:nat) (b:int)
  : Lemma (requires List.Tot.memP b (counted off stride i (len / stride)))
          (ensures off <= b /\ b + stride <= off + len)
          (decreases (if i < len / stride then len / stride - i else 0))
  = step_iff i len stride;
    if i < len / stride then
      (if b = off + i * stride then () else counted_in off len stride (i + 1) b)
    else ()

(* Behind the guard, every frame lies in the batch and so in the buffer, the
   cursor's update [base + stride] and [last] stay in the native int, and a
   frame is a window of exactly [stride] bytes: a short frame cannot borrow
   bytes from the next. *)
let frames_in_buffer (buflen:nat) (off len:int) (stride:int) (b:int)
  : Lemma (requires buflen <= max_int /\ guard buflen off len stride /\
                    List.Tot.memP b (cursor off (off + len - stride) stride))
          (ensures 0 <= b /\ b + stride <= buflen /\ in_int (b + stride) /\ in_int (off + len - stride))
  = cursor_loop off len stride;
    counted_in off len stride 0 b

(* [last] is computed before the loop, frames or not. *)
let last_in_int (buflen:nat) (off len stride:int)
  : Lemma (requires buflen <= max_int /\ guard buflen off len stride /\ in_int stride)
          (ensures in_int (off + len - stride))
  = ()
