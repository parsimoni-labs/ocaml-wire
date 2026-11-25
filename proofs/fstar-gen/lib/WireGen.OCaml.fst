module WireGen.OCaml

(* The OCaml primitives the generated scanner calls, with OCaml's meaning on
   a 64-bit host (the manual: an int is 63-bit two's complement, an int64 a
   64-bit pattern, Int64.to_int keeps the low 63 bits). The F* rendering
   calls these where the OCaml calls the operator or the runtime function;
   the printer's token table maps one to the other.

   Plain [+], [-] and [lsl] wrap in OCaml; the emitter writes them plain
   only where the planner proved the result fits, so here they require it:
   that the rendering is typable is the proof that none wraps. The
   unchecked loads require the bytes to be in the buffer: that the rendering
   is typable is the proof that no read leaves it.

   A primitive whose meaning is more than linear arithmetic is opaque to
   Z3: a scanner is checked with its type alone, which states all its
   checks need, and its meaning is in WireGen.Defs, which a proof loads.
   Unfolded in the context of a long walk, the definitions of its masks,
   shifts and loads make Z3 time out on bounds it proves alone at once. *)

open WireGen.Lib
module U = FStar.UInt
module M = FStar.Math.Lemmas

let min_int : Prims.int = -4611686018427387904
let max_int : Prims.int = 4611686018427387903

let int_range () : Lemma (min_int == - pow2 62 /\ max_int == pow2 62 - 1 /\
                          144115188075855863 == pow2 57 - 9)
  = assert_norm (min_int == - pow2 62 /\ max_int == pow2 62 - 1 /\
                 144115188075855863 == pow2 57 - 9)
let fits (x:Prims.int) : bool = min_int <= x && x <= max_int

(* OCaml's int. *)
type int = x:Prims.int{fits x}

(* Sys.max_string_length on a 64-bit host. *)
let max_string_length : Prims.int = 144115188075855863

(* A Bytes.t: no longer than Sys.max_string_length. *)
type buf = s:bytes{Seq.length s <= max_string_length}

let length (b:buf) : int = Seq.length b

let add (a b:int) : Pure int (requires fits (a + b)) (ensures fun r -> r == a + b) = a + b
let sub (a b:int) : Pure int (requires fits (a - b)) (ensures fun r -> r == a - b) = a - b
let mul (a b:int) : Pure int (requires fits (a * b)) (ensures fun r -> r == a * b) = a * b

let lt (a b:int) : bool = a < b
let le (a b:int) : bool = a <= b
let gt (a b:int) : bool = a > b
let ge (a b:int) : bool = a >= b
let eq (a b:int) : bool = a = b
let ne (a b:int) : bool = a <> b

(* The 63-bit pattern of an int, and back. *)
let pattern (a:int) : r:nat{r < pow2 63} = a % pow2 63
let of_pattern (r:nat{r < pow2 63}) : int = if r >= pow2 62 then r - pow2 63 else r

(* A mask that is not negative keeps a value between 0 and itself: the
   type says so, so that a walk's bounds need no lemma. *)
[@@"opaque_to_smt"]
let land (a b:int) : r:int{b >= 0 ==> 0 <= r /\ r <= b} =
  if b >= 0 then begin
    M.pow2_lt_compat 63 62;
    M.small_mod b (pow2 63);
    U.logand_le #63 (pattern a) (pattern b)
  end;
  of_pattern (U.logand #63 (pattern a) (pattern b))
(* A shift right of a value that is not negative keeps it between 0 and
   itself. *)
[@@"opaque_to_smt"]
let lsr (a:int) (k:nat{k < 63}) : r:int{a >= 0 ==> 0 <= r /\ r <= a} =
  if a >= 0 then begin
    M.pow2_lt_compat 63 62;
    M.small_mod a (pow2 63);
    M.lemma_div_le a (pow2 62) (pow2 k);
    M.lemma_div_le a a (pow2 k);
    M.pow2_le_compat 62 0;
    M.lemma_div_lt_nat a 63 k
  end;
  of_pattern (pattern a / pow2 k)
let lsl (a:int) (k:nat{k < 63})
  : Pure int (requires fits (a * pow2 k)) (ensures fun r -> r == a * pow2 k) = a * pow2 k

(* Wire_runtime.Unchecked: no bounds check of their own. A load's type
   states the range of the value it reads. *)
[@@"opaque_to_smt"]
let u8 (b:buf) (o:int{0 <= o /\ o + 1 <= length b}) : r:int{0 <= r /\ r <= 0xff} =
  assert_norm (pow2 8 == 0x100); M.pow2_lt_compat 62 8; be b o 1
[@@"opaque_to_smt"]
let u16be (b:buf) (o:int{0 <= o /\ o + 2 <= length b}) : r:int{0 <= r /\ r <= 0xffff} =
  assert_norm (pow2 16 == 0x10000); M.pow2_lt_compat 62 16; be b o 2

(* Int32: a 32-bit pattern, which Int32.to_int sign-extends. *)
type int32 = w:nat{w < pow2 32}

[@@"opaque_to_smt"]
let i32be (b:buf) (o:int{0 <= o /\ o + 4 <= length b}) : int32 = be b o 4
[@@"opaque_to_smt"]
let int32_to_int (x:int32) : int =
  M.pow2_lt_compat 62 32;
  if x >= pow2 31 then x - pow2 32 else x

(* Int64: a 64-bit pattern. *)
type int64 = w:nat{w < pow2 64}

[@@"opaque_to_smt"]
let i64be (b:buf) (o:int{0 <= o /\ o + 8 <= length b}) : int64 = be b o 8
[@@"opaque_to_smt"]
let shift_right_logical (w:int64) (k:nat{k < 64}) : int64 = w / pow2 k
[@@"opaque_to_smt"]
let to_int (w:int64) : int = of_pattern (w % pow2 63)

(* {1 Int64 values} as the step functions hold them: the [Int64] operations
   on 64-bit patterns, with their constants written out (2^63
   9223372036854775808, 2^64 18446744073709551616), so that a walk is
   checked without unfolding [pow2]. Each is Plan.Values' operation of the
   same name (WireGen.Defs proves it). Those that are linear, a conversion,
   an addition, a subtraction and a signed comparison, Z3 reads as they are;
   [Int64.mul] and [Int64.logand] are opaque. *)

let pow2_64 () : Lemma (pow2 63 == 9223372036854775808 /\ pow2 64 == 18446744073709551616)
  = assert_norm (pow2 63 == 9223372036854775808 /\ pow2 64 == 18446744073709551616)

(* The integer an [int64] stands for. *)
let signed64 (x:int64) : Prims.int =
  pow2_64 (); if x >= 9223372036854775808 then x - 18446744073709551616 else x

(* [Int64.of_int] sign-extends, [Int64.of_int32] too. *)
let int64_of_int (x:int) : int64 =
  pow2_64 (); int_range (); if x >= 0 then x else x + 18446744073709551616
let int64_of_int32 (x:int32) : int64 =
  pow2_64 (); if x >= 2147483648 then x + 18446744069414584320 else x

(* [Int64.add] and [Int64.sub] are modulo 2^64. *)
let int64_add (a b:int64) : int64 =
  pow2_64 ();
  if a + b >= 18446744073709551616 then a + b - 18446744073709551616 else a + b
let int64_sub (a b:int64) : int64 =
  pow2_64 (); if a - b < 0 then a - b + 18446744073709551616 else a - b

[@@"opaque_to_smt"]
let int64_mul (a b:int64) : int64 = (a * b) % pow2 64

(* [Int64.shift_left] multiplies the pattern modulo 2^64. *)
[@@"opaque_to_smt"]
let shift_left (w:int64) (k:nat{k < 64}) : int64 = (w * pow2 k) % pow2 64

(* A mask that lies in the signed range keeps a pattern between 0 and
   itself. *)
[@@"opaque_to_smt"]
let int64_logand (a b:int64) : r:int64{b < 9223372036854775808 ==> r <= b} =
  U.logand_le #64 a b; U.logand #64 a b

(* The comparisons are signed. *)
let int64_lt (a b:int64) : bool = signed64 a < signed64 b
let int64_le (a b:int64) : bool = signed64 a <= signed64 b
let int64_gt (a b:int64) : bool = signed64 a > signed64 b
let int64_ge (a b:int64) : bool = signed64 a >= signed64 b
let int64_eq (a b:int64) : bool = a = b
let int64_ne (a b:int64) : bool = a <> b

(* {1 The meaning of the opaque primitives} which WireGen.Defs states as
   rewrite rules. *)

let land_def (a b:int) : Lemma (land a b == of_pattern (U.logand #63 (pattern a) (pattern b)))
  = reveal_opaque (`%land) (land a b)
let lsr_def (a:int) (k:nat{k < 63}) : Lemma (lsr a k == of_pattern (pattern a / pow2 k))
  = reveal_opaque (`%lsr) (lsr a k)
let u8_def (b:buf) (o:int{0 <= o /\ o + 1 <= length b}) : Lemma (u8 b o == be b o 1)
  = reveal_opaque (`%u8) (u8 b o)
let u16be_def (b:buf) (o:int{0 <= o /\ o + 2 <= length b}) : Lemma (u16be b o == be b o 2)
  = reveal_opaque (`%u16be) (u16be b o)
let i32be_def (b:buf) (o:int{0 <= o /\ o + 4 <= length b}) : Lemma (i32be b o == be b o 4)
  = reveal_opaque (`%i32be) (i32be b o)
let int32_to_int_def (x:int32)
  : Lemma (int32_to_int x == (if x >= pow2 31 then x - pow2 32 else x))
  = reveal_opaque (`%int32_to_int) (int32_to_int x)
let i64be_def (b:buf) (o:int{0 <= o /\ o + 8 <= length b}) : Lemma (i64be b o == be b o 8)
  = reveal_opaque (`%i64be) (i64be b o)
let shift_right_logical_def (w:int64) (k:nat{k < 64})
  : Lemma (shift_right_logical w k == w / pow2 k)
  = reveal_opaque (`%shift_right_logical) (shift_right_logical w k)
let to_int_def (w:int64) : Lemma (to_int w == of_pattern (w % pow2 63))
  = reveal_opaque (`%to_int) (to_int w)
let int64_mul_def (a b:int64) : Lemma (int64_mul a b == (a * b) % pow2 64)
  = reveal_opaque (`%int64_mul) (int64_mul a b)
let int64_logand_def (a b:int64) : Lemma (int64_logand a b == U.logand #64 a b)
  = reveal_opaque (`%int64_logand) (int64_logand a b)
let shift_left_def (w:int64) (k:nat{k < 64}) : Lemma (shift_left w k == (w * pow2 k) % pow2 64)
  = reveal_opaque (`%shift_left) (shift_left w k)

(* {1 What the walk reads these as} once proved, as rewrite rules. *)

let pow2_small (k:nat)
  : Lemma (requires k <= 62)
          (ensures 1 <= pow2 k /\ pow2 k <= pow2 62 /\ pow2 k - 1 <= max_int /\ pow2 62 == max_int + 1)
          [SMTPat (pow2 k)]
  = M.pow2_le_compat 62 k; int_range ()

let of_pattern_small (r:nat{r < pow2 63})
  : Lemma (requires r < pow2 62) (ensures of_pattern r == r) [SMTPat (of_pattern r)]
  = ()

let pattern_nonneg (a:int)
  : Lemma (requires a >= 0) (ensures pattern a == a) [SMTPat (pattern a)]
  = M.pow2_lt_compat 63 62; M.small_mod a (pow2 63)

let lsr_nonneg (a:int) (k:nat{k < 63})
  : Lemma (requires a >= 0 /\ k >= 1) (ensures lsr a k == a / pow2 k) [SMTPat (lsr a k)]
  = lsr_def a k;
    M.pow2_lt_compat 63 62; M.small_mod a (pow2 63);
    M.lemma_div_le a (pow2 62) (pow2 k);
    M.pow2_minus 62 k;
    M.pow2_le_compat 62 (62 - k)

let land_mask (a:int) (k:nat)
  : Lemma (requires a >= 0 /\ 1 <= k /\ k <= 62)
          (ensures land a (pow2 k - 1) == a % pow2 k)
          [SMTPat (land a (pow2 k - 1))]
  = land_def a (pow2 k - 1);
    M.pow2_le_compat 62 k;
    M.pow2_lt_compat 63 62; M.small_mod a (pow2 63);
    M.small_mod (pow2 k - 1) (pow2 63);
    U.logand_mask #63 a k;
    M.lemma_mod_lt a (pow2 k)

(* A field taken out of a shared word: [to_int] keeps the low 63 bits,
   which a mask of at most 62 bits does not see. *)
let land_to_int_mask (w:int64) (k:nat)
  : Lemma (requires 1 <= k /\ k <= 62)
          (ensures land (to_int w) (pow2 k - 1) == w % pow2 k)
          [SMTPat (land (to_int w) (pow2 k - 1))]
  = land_def (to_int w) (pow2 k - 1);
    to_int_def w;
    M.pow2_le_compat 62 k;
    let r = w % pow2 63 in
    assert (pattern (to_int w) == r);
    M.small_mod (pow2 k - 1) (pow2 63);
    U.logand_mask #63 r k;
    M.pow2_modulo_modulo_lemma_1 w k 63

(* The low [k] bits of any int, negative ones included. *)
let land_low (a:int) (k:nat)
  : Lemma (requires 1 <= k /\ k <= 62) (ensures land a (pow2 k - 1) == a % pow2 k)
  = land_def a (pow2 k - 1);
    M.pow2_le_compat 62 k;
    M.pow2_lt_compat 63 62;
    M.small_mod (pow2 k - 1) (pow2 63);
    U.logand_mask #63 (pattern a) k;
    M.pow2_modulo_modulo_lemma_1 a k 63;
    M.lemma_mod_lt a (pow2 k)

(* A 32-bit word sign-extended, then masked to its 32 bits, is the word. *)
let land_int32 (x:int32) (m:Prims.int)
  : Lemma (requires m == pow2 32 - 1) (ensures land (int32_to_int x) m == x)
          [SMTPat (land (int32_to_int x) m)]
  = int32_to_int_def x;
    land_low (int32_to_int x) 32;
    if x >= pow2 31 then M.lemma_mod_plus x (-1) (pow2 32);
    M.small_mod x (pow2 32)

let to_int_small (w:int64)
  : Lemma (requires w < pow2 62) (ensures to_int w == w) [SMTPat (to_int w)]
  = to_int_def w; M.pow2_lt_compat 63 62; M.small_mod w (pow2 63)

let land_lit (k:nat) (m:Prims.int)
  : Lemma (requires 1 <= k /\ k <= 62 /\ m == pow2 k - 1)
          (ensures (forall (a:int). {:pattern (land a m)} a >= 0 ==> land a m == a % pow2 k) /\
                   (forall (w:int64). {:pattern (land (to_int w) m)} land (to_int w) m == w % pow2 k))
  = let aux (a:int) : Lemma (a >= 0 ==> land a m == a % pow2 k) =
      if a >= 0 then land_mask a k in
    FStar.Classical.forall_intro aux;
    FStar.Classical.forall_intro (fun (w:int64) -> land_to_int_mask w k)

(* An [int64] masked to its low [k] bits. *)
let int64_mask (w:int64) (k:nat)
  : Lemma (requires 1 <= k /\ k <= 63) (ensures int64_logand w (pow2 k - 1) == w % pow2 k)
  = int64_logand_def w (pow2 k - 1);
    M.pow2_le_compat 64 k;
    U.logand_mask #64 w k

let int64_land_lit (k:nat) (m:Prims.int)
  : Lemma (requires 1 <= k /\ k <= 63 /\ m == pow2 k - 1)
          (ensures forall (w:int64). {:pattern (int64_logand w m)} int64_logand w m == w % pow2 k)
  = FStar.Classical.forall_intro (fun (w:int64) -> int64_mask w k)

(* A 32-bit word sign-extended to 64 bits, then masked to its 32 bits, is
   the word. *)
let int64_land_int32 (x:int32) (m:Prims.int)
  : Lemma (requires m == pow2 32 - 1) (ensures int64_logand (int64_of_int32 x) m == x)
          [SMTPat (int64_logand (int64_of_int32 x) m)]
  = pow2_64 ();
    assert_norm (pow2 32 == 4294967296);
    int64_mask (int64_of_int32 x) 32;
    if x >= 2147483648 then M.lemma_mod_plus x 4294967295 4294967296;
    M.small_mod x (pow2 32)

(* The [m] bytes at [o + k] out of the big-endian word at [o]. *)
let word_bytes (b:bytes) (o:nat) (k:nat) (m:nat)
  : Lemma (requires 1 <= m /\ m <= 7 /\ k + m <= 8)
          (ensures (be b o 8 / pow2 (8 * (8 - k - m))) % pow2 (8 * m) == be b (o + k) m /\
                   (k == 0 ==> be b o 8 / pow2 (8 * (8 - k - m)) == be b o m))
  = Plan.Word.word_field (byte b) o (o + k) m;
    let w = be b o 8 in
    let s = 8 * (8 - k - m) in
    M.pow2_modulo_modulo_lemma_1 (w / pow2 s) (8 * m) 63;
    if k = 0 then begin
      M.lemma_div_lt_nat w 64 s;
      M.pow2_le_compat 63 (64 - s);
      M.small_mod (w / pow2 s) (pow2 63);
      M.small_mod (be b o m) (pow2 (8 * m))
    end

let word_lit (k:nat) (m:nat) (sh:nat) (p:pos)
  : Lemma (requires 1 <= m /\ m <= 7 /\ k + m <= 8 /\ sh == 8 * (8 - k - m) /\ p == pow2 (8 * m))
          (ensures (forall (b:buf) (o:int). {:pattern (shift_right_logical (i64be b o) sh)}
                     (0 <= o /\ o + 8 <= length b) ==>
                     (shift_right_logical (i64be b o) sh % p == be b (o + k) m /\
                      (k == 0 ==> shift_right_logical (i64be b o) sh == be b o m))))
  = let aux (b:buf) (o:int) : Lemma ((0 <= o /\ o + 8 <= length b) ==>
                     (shift_right_logical (i64be b o) sh % p == be b (o + k) m /\
                      (k == 0 ==> shift_right_logical (i64be b o) sh == be b o m))) =
      if 0 <= o && o + 8 <= length b then begin
        i64be_def b o;
        shift_right_logical_def (i64be b o) sh;
        word_bytes b o k m
      end in
    FStar.Classical.forall_intro_2 aux

(* The [m] bytes that end the big-endian word at [o], which the emitter
   reads with no shift. *)
let word_end (k:nat) (m:nat) (p:pos)
  : Lemma (requires 1 <= m /\ m <= 7 /\ k + m == 8 /\ p == pow2 (8 * m))
          (ensures (forall (b:buf) (o:int). {:pattern (i64be b o)}
                     (0 <= o /\ o + 8 <= length b) ==> i64be b o % p == be b (o + k) m))
  = let aux (b:buf) (o:int) : Lemma ((0 <= o /\ o + 8 <= length b) ==>
                     i64be b o % p == be b (o + k) m) =
      if 0 <= o && o + 8 <= length b then begin
        i64be_def b o;
        assert_norm (pow2 0 == 1);
        word_bytes b o k m
      end in
    FStar.Classical.forall_intro_2 aux

(* {1 The 3D value as ints} for the statement: a scanner output is the
   value 3d.exe parses, or bits of it. *)

let of_u8 (x:FStar.UInt8.t) : int = M.pow2_lt_compat 62 8; FStar.UInt8.v x
let of_u16 (x:FStar.UInt16.t) : int = M.pow2_lt_compat 62 16; FStar.UInt16.v x
let of_u32 (x:FStar.UInt32.t) : int = M.pow2_lt_compat 62 32; FStar.UInt32.v x

(* The [w] bits from bit [lo] up of a value that is not negative, bit 0
   the least significant (0 for a negative one, which no 3D value is). *)
let bits (x:int) (lo:nat) (w:nat) : int =
  if x < 0 then 0 else begin
    M.lemma_div_le x max_int (pow2 lo);
    M.lemma_mod_lt (x / pow2 lo) (pow2 w);
    M.modulo_range_lemma (x / pow2 lo) (pow2 w);
    if x / pow2 lo < pow2 w then (M.small_mod (x / pow2 lo) (pow2 w); x / pow2 lo)
    else (x / pow2 lo) % pow2 w
  end

(* The [w] bits from bit [lo] up of an [int64] pattern, as the step walk
   states a bitfield of its word. *)
let bits64 (x:int64) (lo:nat) (w:nat) : nat = (x / pow2 lo) % pow2 w

(* The step walk hands out a value as its [int64]: a bitfield's is the
   pattern's bits. *)
let int64_of_int_bits (x:int) (lo:nat) (w:nat)
  : Lemma (requires x >= 0) (ensures int64_of_int (bits x lo w) == bits64 x lo w)
          [SMTPat (int64_of_int (bits x lo w))]
  = M.lemma_div_le x max_int (pow2 lo);
    if x / pow2 lo < pow2 w then M.small_mod (x / pow2 lo) (pow2 w)
