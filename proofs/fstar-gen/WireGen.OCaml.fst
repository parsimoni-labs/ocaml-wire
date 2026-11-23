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

let min_int : Prims.int = - pow2 62
let max_int : Prims.int = pow2 62 - 1
let fits (x:Prims.int) : bool = min_int <= x && x <= max_int

(* OCaml's int. *)
type int = x:Prims.int{fits x}

(* Sys.max_string_length on a 64-bit host. *)
let max_string_length : Prims.int = pow2 57 - 9

(* A Bytes.t: no longer than Sys.max_string_length. *)
type buf = s:bytes{Seq.length s <= max_string_length}

let length (b:buf) : int = Seq.length b

let add (a b:int) : Pure int (requires fits (a + b)) (ensures fun r -> r == a + b) = a + b
let sub (a b:int) : Pure int (requires fits (a - b)) (ensures fun r -> r == a - b) = a - b

let lt (a b:int) : bool = a < b
let le (a b:int) : bool = a <= b
let gt (a b:int) : bool = a > b
let ge (a b:int) : bool = a >= b
let eq (a b:int) : bool = a = b

(* The 63-bit pattern of an int, and back. *)
let pattern (a:int) : r:nat{r < pow2 63} = a % pow2 63
let of_pattern (r:nat{r < pow2 63}) : int = if r >= pow2 62 then r - pow2 63 else r

let land (a b:int) : int = of_pattern (U.logand #63 (pattern a) (pattern b))
let lsr (a:int) (k:nat{k < 63}) : int = of_pattern (pattern a / pow2 k)

(* Wire_runtime.Unchecked: no bounds check of their own. *)
let u8 (b:buf) (o:int{0 <= o /\ o + 1 <= length b}) : int = M.pow2_lt_compat 62 8; be b o 1
let u16be (b:buf) (o:int{0 <= o /\ o + 2 <= length b}) : int = M.pow2_lt_compat 62 16; be b o 2

(* Int64: a 64-bit pattern. *)
type int64 = w:nat{w < pow2 64}

let i64be (b:buf) (o:int{0 <= o /\ o + 8 <= length b}) : int64 = be b o 8
let shift_right_logical (w:int64) (k:nat{k < 64}) : int64 = w / pow2 k
let to_int (w:int64) : int = of_pattern (w % pow2 63)

(* {1 What the walk reads these as} once proved, as rewrite rules. *)

let pow2_small (k:nat)
  : Lemma (requires k <= 62) (ensures 1 <= pow2 k /\ pow2 k <= pow2 62) [SMTPat (pow2 k)]
  = M.pow2_le_compat 62 k

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

let to_int_small (w:int64)
  : Lemma (requires w < pow2 62) (ensures to_int w == w) [SMTPat (to_int w)]
  = M.pow2_lt_compat 63 62; M.small_mod w (pow2 63)
