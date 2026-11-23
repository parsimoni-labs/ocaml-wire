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
   is typable is the proof that no read leaves it. *)

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

let land (a b:int) : int = of_pattern (U.logand #63 (pattern a) (pattern b))
let lsr (a:int) (k:nat{k < 63}) : int = of_pattern (pattern a / pow2 k)
let lsl (a:int) (k:nat{k < 63})
  : Pure int (requires fits (a * pow2 k)) (ensures fun r -> r == a * pow2 k) = a * pow2 k

(* Wire_runtime.Unchecked: no bounds check of their own. *)
let u8 (b:buf) (o:int{0 <= o /\ o + 1 <= length b}) : int = M.pow2_lt_compat 62 8; be b o 1
let u16be (b:buf) (o:int{0 <= o /\ o + 2 <= length b}) : int = M.pow2_lt_compat 62 16; be b o 2

(* Int32: a 32-bit pattern, which Int32.to_int sign-extends. *)
type int32 = w:nat{w < pow2 32}

let i32be (b:buf) (o:int{0 <= o /\ o + 4 <= length b}) : int32 = be b o 4
let int32_to_int (x:int32) : int =
  M.pow2_lt_compat 62 32;
  if x >= pow2 31 then x - pow2 32 else x

(* Int64: a 64-bit pattern. *)
type int64 = w:nat{w < pow2 64}

let i64be (b:buf) (o:int{0 <= o /\ o + 8 <= length b}) : int64 = be b o 8
let shift_right_logical (w:int64) (k:nat{k < 64}) : int64 = w / pow2 k
let to_int (w:int64) : int = of_pattern (w % pow2 63)

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
  = M.pow2_lt_compat 63 62; M.small_mod a (pow2 63);
    M.lemma_div_le a (pow2 62) (pow2 k);
    M.pow2_minus 62 k;
    M.pow2_le_compat 62 (62 - k)

let land_mask (a:int) (k:nat)
  : Lemma (requires a >= 0 /\ 1 <= k /\ k <= 62)
          (ensures land a (pow2 k - 1) == a % pow2 k)
          [SMTPat (land a (pow2 k - 1))]
  = M.pow2_le_compat 62 k;
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
  = M.pow2_le_compat 62 k;
    let r = w % pow2 63 in
    assert (pattern (to_int w) == r);
    M.small_mod (pow2 k - 1) (pow2 63);
    U.logand_mask #63 r k;
    M.pow2_modulo_modulo_lemma_1 w k 63

(* The low [k] bits of any int, negative ones included. *)
let land_low (a:int) (k:nat)
  : Lemma (requires 1 <= k /\ k <= 62) (ensures land a (pow2 k - 1) == a % pow2 k)
  = M.pow2_le_compat 62 k;
    M.pow2_lt_compat 63 62;
    M.small_mod (pow2 k - 1) (pow2 63);
    U.logand_mask #63 (pattern a) k;
    M.pow2_modulo_modulo_lemma_1 a k 63;
    M.lemma_mod_lt a (pow2 k)

(* A 32-bit word sign-extended, then masked to its 32 bits, is the word. *)
let land_int32 (x:int32) (m:Prims.int)
  : Lemma (requires m == pow2 32 - 1) (ensures land (int32_to_int x) m == x)
          [SMTPat (land (int32_to_int x) m)]
  = land_low (int32_to_int x) 32;
    if x >= pow2 31 then M.lemma_mod_plus x (-1) (pow2 32);
    M.small_mod x (pow2 32)

let to_int_small (w:int64)
  : Lemma (requires w < pow2 62) (ensures to_int w == w) [SMTPat (to_int w)]
  = M.pow2_lt_compat 63 62; M.small_mod w (pow2 63)

let land_lit (k:nat) (m:Prims.int)
  : Lemma (requires 1 <= k /\ k <= 62 /\ m == pow2 k - 1)
          (ensures (forall (a:int). {:pattern (land a m)} a >= 0 ==> land a m == a % pow2 k) /\
                   (forall (w:int64). {:pattern (land (to_int w) m)} land (to_int w) m == w % pow2 k))
  = let aux (a:int) : Lemma (a >= 0 ==> land a m == a % pow2 k) =
      if a >= 0 then land_mask a k in
    FStar.Classical.forall_intro aux;
    FStar.Classical.forall_intro (fun (w:int64) -> land_to_int_mask w k)

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
      if 0 <= o && o + 8 <= length b then word_bytes b o k m in
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
