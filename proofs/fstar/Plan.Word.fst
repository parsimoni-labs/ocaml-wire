module Plan.Word

(* Fields read from a word another load already holds: Emitter.unsigned (a
   field inside a shared 64-bit big-endian load), Emitter.u16_in_word (a
   16-bit field through the 32-bit load that starts or ends on it), and
   Planner.word_range (two stacked bitfields tested as one range of their
   word). *)

open FStar.Mul
open FStar.Math.Lemmas

(* The bytes of the buffer. *)
type bytes = nat -> b:nat{b < 256}

(* [m] bytes at [k], big-endian and little-endian. *)
let rec be (b:bytes) (k:nat) (m:nat) : Tot nat (decreases m) =
  if m = 0 then 0 else be b k (m - 1) * 256 + b (k + m - 1)

let rec le (b:bytes) (k:nat) (m:nat) : Tot nat (decreases m) =
  if m = 0 then 0 else b k + 256 * le b (k + 1) (m - 1)

let rec be_bound (b:bytes) (k m:nat) : Lemma (be b k m < pow2 (8 * m)) =
  if m = 0 then () else begin
    be_bound b k (m - 1);
    pow2_plus (8 * (m - 1)) 8;
    assert_norm (pow2 8 == 256)
  end

let rec le_bound (b:bytes) (k m:nat) : Lemma (ensures le b k m < pow2 (8 * m)) (decreases m) =
  if m = 0 then () else begin
    le_bound b (k + 1) (m - 1);
    pow2_plus 8 (8 * (m - 1));
    assert_norm (pow2 8 == 256)
  end

(* A big-endian run splits at any byte. *)
let rec be_split (b:bytes) (k m1 m2:nat)
  : Lemma (ensures be b k (m1 + m2) == be b k m1 * pow2 (8 * m2) + be b (k + m1) m2) (decreases m2)
  = if m2 = 0 then ()
    else begin
      be_split b k m1 (m2 - 1);
      pow2_plus (8 * (m2 - 1)) 8;
      assert_norm (pow2 8 == 256)
    end

let rec le_split (b:bytes) (k m1 m2:nat)
  : Lemma (ensures le b k (m1 + m2) == le b k m1 + pow2 (8 * m1) * le b (k + m1) m2) (decreases m1)
  = if m1 = 0 then ()
    else begin
      le_split b (k + 1) (m1 - 1) m2;
      pow2_plus 8 (8 * (m1 - 1));
      assert_norm (pow2 8 == 256)
    end

(* [(hi * 2^r + lo) / 2^r = hi] and [(x * 2^m + y) mod 2^m = y] for small
   [lo] and [y]. *)
let div_shift (hi lo r:nat) : Lemma (requires lo < pow2 r) (ensures (hi * pow2 r + lo) / pow2 r == hi) =
  lemma_div_plus lo hi (pow2 r); small_div lo (pow2 r)

let mod_shift (x y m:nat) : Lemma (requires y < pow2 m) (ensures (x * pow2 m + y) % pow2 m == y) =
  lemma_mod_plus y x (pow2 m); small_mod y (pow2 m)

(* {1 A field inside a shared 64-bit load}

   The code loads [w = be b a 8], then reads the [m] bytes at [k] as
   [Int64.to_int (Int64.shift_right_logical w s) land (2^(8m) - 1)], with
   [s = 8 * (a + 8 - k - m)], leaving the mask out when [k = a]. On a 64-bit
   host [Int64.to_int] keeps the low 63 bits, which a mask narrower than them
   does not see; the model takes those OCaml semantics as given. *)

let shift_right_logical (w:nat) (s:nat) : nat = w / pow2 s
let to_int (x:nat) : nat = x % pow2 63

let extract (w:nat) (a k:nat) (m:nat{k + m <= a + 8}) : nat =
  let s = 8 * (a + 8 - k - m) in
  let v = to_int (shift_right_logical w s) in
  if a = k then v else v % pow2 (8 * m)

let word_field (b:bytes) (a k:nat) (m:nat{1 <= m /\ m <= 7})
  : Lemma (requires a <= k /\ k + m <= a + 8)
          (ensures extract (be b a 8) a k m == be b k m)
  = let k' = k - a in
    let rest = 8 - k' - m in
    (* The word is [top * 2^(8 rest) + below], [top] the bytes up to the
       field's end. *)
    be_split b a (k' + m) rest;
    be_bound b (a + k' + m) rest;
    let top = be b a (k' + m) in
    div_shift top (be b (a + k' + m) rest) (8 * rest);
    assert (shift_right_logical (be b a 8) (8 * (a + 8 - k - m)) == top);
    (* [top] is [be b a k' * 2^(8m) + be b k m]. *)
    be_split b a k' m;
    be_bound b k m;
    mod_shift (be b a k') (be b k m) (8 * m);
    if a = k then begin
      be_bound b a m;
      pow2_le_compat 63 (8 * m);
      small_mod top (pow2 63)
    end
    else
      pow2_modulo_modulo_lemma_1 top (8 * m) 63

(* The shared load stays inside the prefix whose bound was checked: the
   word starts at [a = min (8 * i) (size - 8)] (Emitter.words), so
   [0 <= a] and [a + 8 <= size] for a prefix of at least 8 bytes. *)
let word_in_prefix (size i:nat) : Lemma (requires size >= 8)
  (ensures (let a = if 8 * i < size - 8 then 8 * i else size - 8 in 0 <= a /\ a + 8 <= size)) = ()

(* {1 A 16-bit field through a 32-bit load (OxCaml)} *)

let low16 (x:nat) : nat = x % pow2 16
let high16 (x:nat) : nat = (x / pow2 16) % pow2 16

let u16_be_low (b:bytes) (k:nat) : Lemma (requires k >= 2) (ensures low16 (be b (k - 2) 4) == be b k 2) =
  be_split b (k - 2) 2 2; be_bound b k 2; mod_shift (be b (k - 2) 2) (be b k 2) 16

let u16_be_high (b:bytes) (k:nat) : Lemma (high16 (be b k 4) == be b k 2) =
  be_split b k 2 2; be_bound b (k + 2) 2; be_bound b k 2;
  div_shift (be b k 2) (be b (k + 2) 2) 16; small_mod (be b k 2) (pow2 16)

let u16_le_low (b:bytes) (k:nat) : Lemma (low16 (le b k 4) == le b k 2) =
  le_split b k 2 2; le_bound b k 2;
  lemma_mod_plus (le b k 2) (le b (k + 2) 2) (pow2 16); small_mod (le b k 2) (pow2 16)

let u16_le_high (b:bytes) (k:nat) : Lemma (requires k >= 2) (ensures high16 (le b (k - 2) 4) == le b k 2) =
  le_split b (k - 2) 2 2; le_bound b (k - 2) 2; le_bound b k 2;
  div_shift (le b k 2) (le b (k - 2) 2) 16; small_mod (le b k 2) (pow2 16)

(* {1 Two stacked bitfields as one range of their word}

   Planner.word_range: [f] is the top [width] bits of a word of [bits] bits
   ([shift + width = bits]), [g] the [width'] bits right below it
   ([shift' + width' = shift]). The bitfield views are
   [(word lsr shift) land (2^width - 1)]. When the checks are [f = c] and
   [g >= d], the planner tests [lo <= word <= hi] with
   [lo = c lsl shift lor d lsl shift'] and [hi = (c + 1) lsl shift - 1]. *)

let view (word shift width:nat) : nat = (word / pow2 shift) % pow2 width

let word_range (word:nat) (shift width shift' width' c d:nat)
  : Lemma (requires shift' + width' == shift /\ word < pow2 (shift + width) /\
                    c < pow2 width /\ d < pow2 width')
          (ensures (view word shift width == c /\ view word shift' width' >= d) <==>
                   (c * pow2 shift + d * pow2 shift' <= word /\ word <= (c + 1) * pow2 shift - 1))
  = let f = word / pow2 shift in
    let r = word % pow2 shift in
    let g = r / pow2 shift' in
    let low = r % pow2 shift' in
    let ps = pow2 shift in
    let ps' = pow2 shift' in
    let pw' = pow2 width' in
    lemma_div_mod word ps;
    lemma_div_mod r ps';
    pow2_plus width' shift';
    assert (ps == pw' * ps');
    lemma_div_lt_nat word (shift + width) shift;
    lemma_div_lt_nat r shift shift';
    assert (f < pow2 width /\ g < pw' /\ low < ps');
    small_mod f (pow2 width);
    assert (view word shift width == f);
    (* [word = (f * 2^width' + g) * 2^shift' + low], so dividing by
       [2^shift'] then keeping [width'] bits gives [g]. *)
    let hi = f * pw' + g in
    paren_mul_right f pw' ps';
    distributivity_add_left (f * pw') g ps';
    swap_mul ps f;
    swap_mul ps' g;
    assert (word == hi * ps' + low);
    div_shift hi low shift';
    mod_shift f g width';
    assert (view word shift' width' == g);
    (* [word = f * 2^shift + rest] with [rest = g * 2^shift' + low < 2^shift]. *)
    let rest = g * ps' + low in
    lemma_mult_le_right ps' (g + 1) pw';
    assert (rest < ps);
    assert (word == f * ps + rest);
    if f = c then begin
      if g < d then lemma_mult_le_right ps' (g + 1) d
      else lemma_mult_le_right ps' d g
    end
    else if f < c then
      lemma_mult_le_right ps (f + 1) c
    else
      lemma_mult_le_right ps (c + 1) f

(* [c lsl shift lor d lsl shift'] is the sum: the two do not overlap. *)
let lor_disjoint (c d shift shift':nat)
  : Lemma (requires shift' <= shift /\ d < pow2 (shift - shift'))
          (ensures d * pow2 shift' < pow2 shift)
  = pow2_plus (shift - shift') shift';
    lemma_mult_lt_right (pow2 shift') d (pow2 (shift - shift'))
