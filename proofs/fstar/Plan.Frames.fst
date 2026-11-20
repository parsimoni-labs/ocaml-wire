module Plan.Frames

(* The frame loop of fold_frames (Scan_module.pp_frames): after the guard

     stride <= 0 || off < 0 || len < 0 || off > Bytes.length buf
     || len > Bytes.length buf - off  ->  invalid_arg

   it walked [for _ = 1 to len / stride] with a cursor besides, and since
   73591041 walks [while !cursor <= last] with [last = off + len - stride].
   Each frame is checked as a window of [stride] bytes at its base, so
   Plan.Proof applies to it with [len = stride]. *)

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

(* {1 Four frames an iteration (fb3cb405)}

   When [stride <= len / 4] the loop first walks four frames an iteration,
   at [c], [c + stride], [c + stride_2] and [c + stride_3], while
   [c <= last_4 = off + len - 4 * stride], then the rest one by one from
   where it stopped. *)

let rec quad (c last4:int) (s:pos) : Tot (list int) (decreases (if c <= last4 then last4 - c + 1 else 0)) =
  if c <= last4 then c :: (c + s) :: (c + 2 * s) :: (c + 3 * s) :: quad (c + 4 * s) last4 s else []

(* Where the four-frame loop leaves the cursor. *)
let rec quad_end (c last4:int) (s:pos) : Tot int (decreases (if c <= last4 then last4 - c + 1 else 0)) =
  if c <= last4 then quad_end (c + 4 * s) last4 s else c

let four_frames (off len:int) (s:pos) : list int =
  let last = off + len - s in
  if s <= len / 4 then
    let last4 = off + len - 4 * s in
    quad off last4 s `List.Tot.append` cursor (quad_end off last4 s) last s
  else cursor off last s

let quad_iff (i:nat) (len:nat) (s:pos)
  : Lemma ((i + 4) * s <= len <==> i + 3 < len / s)
  = step_iff (i + 3) len s

#push-options "--fuel 5 --ifuel 1 --z3rlimit 40"
let rec same_quads (off len:nat) (s:pos) (i:nat)
  : Lemma (ensures (let last4 = off + len - 4 * s in
                    let c = off + i * s in
                    quad c last4 s `List.Tot.append` cursor (quad_end c last4 s) (off + len - s) s ==
                    counted off s i (len / s)))
          (decreases (if i < len / s then len / s - i else 0))
  = quad_iff i len s;
    let n = len / s in
    if i + 3 < n then begin
      same_quads off len s (i + 4);
      assert (off + (i + 4) * s == off + i * s + 4 * s);
      assert (counted off s i n ==
              (off + i * s) :: (off + (i + 1) * s) :: (off + (i + 2) * s) :: (off + (i + 3) * s) ::
              counted off s (i + 4) n)
    end
    else same_frames off len s i
#pop-options

(* The four-frame loop, then the one-frame loop, visit the frames the counted
   loop did, in the same order. *)
let four_frames_loop (off len:nat) (s:pos)
  : Lemma (four_frames off len s == counted off s 0 (len / s))
  = if s <= len / 4 then same_quads off len s 0 else cursor_loop off len s

(* The values computed for the four-frame loop stay in the native int:
   [stride_2], [stride_3], [stride_4], and [last_4], which is at least
   [off]. *)
let four_frames_ints (buflen:nat) (off len:int) (s:int)
  : Lemma (requires buflen <= max_int /\ guard buflen off len s /\ s <= len / 4)
          (ensures in_int (2 * s) /\ in_int (3 * s) /\ in_int (4 * s) /\
                   off <= off + len - 4 * s /\ in_int (off + len - 4 * s))
  = FStar.Math.Lemmas.lemma_div_mod len 4

(* Every frame either loop walks lies in the buffer, its base plus [stride]
   in the native int. *)
let four_frames_in_buffer (buflen:nat) (off len:int) (s:int) (b:int)
  : Lemma (requires buflen <= max_int /\ guard buflen off len s /\
                    List.Tot.memP b (four_frames off len s))
          (ensures 0 <= b /\ b + s <= buflen /\ in_int (b + s))
  = four_frames_loop off len s;
    counted_in off len s 0 b
