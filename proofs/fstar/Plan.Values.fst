module Plan.Values

(* How the step and OxCaml walks hold integer values (Decision.values):
   [`Int64] in the OCaml step functions, [`Int64_u] on OxCaml, where the
   other walks hold an [int]. The emitter writes the same plan with
   Int64 (or Int64_u, whose operations are the same on the 64-bit pattern)
   operations: a value of the codec's 63-bit int is held sign-extended,
   [of_int], plain arithmetic where the planner's ranges keep it inside the
   63-bit int, checked arithmetic through [to_int] and Wire_runtime.Arith
   elsewhere, a signed field as the sign extension of its unsigned bytes,
   [shift_right (shift_left v (64 - w)) (64 - w)], and a bitfield as
   [logand (shift_right_logical word shift) (2^width - 1)] of a word that may
   hold more than its bytes above them.

   What is assumed of OCaml, from its manual, as in Plan.Word: an [int64] is
   a 64-bit two's complement pattern; [Int64.add], [sub] and [mul] are
   modulo 2^64; [Int64.of_int] sign-extends; [Int64.to_int] keeps the low 63
   bits; [shift_left] multiplies the pattern by a power of two modulo 2^64;
   [shift_right] divides its signed value by one, rounding down;
   [shift_right_logical] divides the pattern; [logand] with [2^w - 1] keeps
   its low [w] bits. *)

open FStar.Math.Lemmas
open Plan.Expr

(* A 64-bit pattern and the integer it stands for. *)
type pattern = x:nat{x < pow2 64}

let signed (x:pattern) : int = if x >= pow2 63 then x - pow2 64 else x

(* [Int64.of_int]: the pattern of an integer of the 63-bit int. *)
let of_int (v:int) : pattern = v % pow2 64

(* [Int64.to_int]: the low 63 bits, as a 63-bit int. *)
let to_int (x:pattern) : int = wrap x

let add64 (a b:pattern) : pattern = (a + b) % pow2 64
let sub64 (a b:pattern) : pattern = (a - b) % pow2 64
let mul64 (a b:pattern) : pattern = (a * b) % pow2 64

let bounds () : Lemma (pow2 63 == 2 * pow2 62 /\ pow2 64 == 2 * pow2 63) =
  assert_norm (pow2 63 == 2 * pow2 62 /\ pow2 64 == 2 * pow2 63)

(* A value of the 63-bit int, held as an [int64], is itself. *)
let signed_of_int (v:int) : Lemma (requires in_int v) (ensures signed (of_int v) == v /\ to_int (of_int v) == v) =
  bounds ();
  if v >= 0 then small_mod v (pow2 64)
  else (lemma_mod_plus v 1 (pow2 64); small_mod (v + pow2 64) (pow2 64));
  (* [to_int] of the pattern keeps [v]: the pattern is [v] modulo 2^63. *)
  let x = of_int v in
  assert (x % pow2 63 == v % pow2 63) by (FStar.Tactics.V2.smt ());
  lemma_mod_plus_distr_l x (pow2 62) (pow2 63);
  lemma_mod_plus_distr_l v (pow2 62) (pow2 63);
  wrap_id v

(* Plain arithmetic on the patterns of two values, where the result lies in
   the 63-bit int as the planner's ranges prove (Plan.Expr.fits), holds the
   result: no 64-bit operation wraps there. *)
let plain_ops (a b:int)
  : Lemma (requires in_int a /\ in_int b)
          (ensures (in_int (a + b) ==> signed (add64 (of_int a) (of_int b)) == a + b) /\
                   (in_int (a - b) ==> signed (sub64 (of_int a) (of_int b)) == a - b) /\
                   (in_int (a * b) ==> signed (mul64 (of_int a) (of_int b)) == a * b))
  = bounds ();
    lemma_mod_plus_distr_l a (of_int b) (pow2 64);
    lemma_mod_plus_distr_r a b (pow2 64);
    lemma_mod_sub_distr a (of_int b) (pow2 64);
    lemma_mod_add_distr a (- b) (pow2 64);
    lemma_mod_mul_distr_l a (of_int b) (pow2 64);
    lemma_mod_mul_distr_r a b (pow2 64);
    let back (x:int) : Lemma (requires in_int x) (ensures signed (x % pow2 64) == x) =
      signed_of_int x in
    FStar.Classical.move_requires back (a + b);
    FStar.Classical.move_requires back (a - b);
    FStar.Classical.move_requires back (a * b)

(* Checked arithmetic goes through [to_int], which gives back each operand
   as the walk over [int] holds it, and [of_int] of its result: the step and
   OxCaml walks reject, or hold, what the [int] walk does. *)
let checked_operands (a b:int)
  : Lemma (requires in_int a /\ in_int b)
          (ensures to_int (of_int a) == a /\ to_int (of_int b) == b)
  = signed_of_int a; signed_of_int b

(* {1 Signed fields} *)

(* The signed value of [w] bits [u]: the codec's value of a signed field. *)
let signed_bits (w:pos) (u:nat{u < pow2 w}) : int = if u >= pow2 (w - 1) then u - pow2 w else u

(* [Int64.shift_left] and [Int64.shift_right]. *)
let shift_left64 (x:pattern) (k:nat) : pattern = (x * pow2 k) % pow2 64
let shift_right64 (x:pattern) (k:nat) : int = signed x / pow2 k

(* Emitter.value's [sign] on an [int64] or [int64_u]. *)
let sign64 (x:pattern) (w:pos{w <= 64}) : int = shift_right64 (shift_left64 x (64 - w)) (64 - w)

(* The sign extension of a pattern whose low [w] bits are the field's,
   whatever it holds above them, is the field's signed value. *)
let sign_extend (x:pattern) (w:pos{w <= 64})
  : Lemma (sign64 x w == signed_bits w (x % pow2 w))
  = let k = 64 - w in
    let u = x % pow2 w in
    pow2_plus w k;
    lemma_mod_mul_distr_l x (pow2 k) (pow2 64);
    (* [(x * 2^k) mod 2^64 = (x mod 2^w) * 2^k]. *)
    lemma_div_mod x (pow2 w);
    pow2_multiplication_modulo_lemma_2 x 64 k;
    lemma_mult_lt_right (pow2 k) u (pow2 w);
    small_mod (u * pow2 k) (pow2 64);
    let q = shift_left64 x k in
    assert (q == u * pow2 k);
    if w = 1 then () else pow2_plus (w - 1) 1;
    pow2_plus (w - 1) k;
    pow2_plus 63 1;
    assert (pow2 63 == pow2 (w - 1) * pow2 k);
    if u >= pow2 (w - 1) then begin
      lemma_mult_le_right (pow2 k) (pow2 (w - 1)) u;
      assert (signed q == (u - pow2 w) * pow2 k);
      cancel_mul_div (u - pow2 w) (pow2 k)
    end
    else begin
      lemma_mult_lt_right (pow2 k) u (pow2 (w - 1));
      cancel_mul_div u (pow2 k)
    end

(* {1 Bitfields} *)

(* [logand (shift_right_logical x s) (2^w - 1)], on the pattern. *)
let view64 (x:pattern) (s w:nat) : nat = (x / pow2 s) % pow2 w

(* A bitfield of the [b] low bits of a word, read from a pattern that holds
   those bits and anything above them (a 32-bit load sign-extended, [raw]),
   is the bitfield of the word itself. *)
let view_raw (x:pattern) (b s w:nat)
  : Lemma (requires s + w <= b) (ensures view64 x s w == ((x % pow2 b) / pow2 s) % pow2 w)
  = pow2_plus s w;
    pow2_le_compat b (s + w);
    modulo_division_lemma x (pow2 s) (pow2 w);
    modulo_division_lemma (x % pow2 b) (pow2 s) (pow2 w);
    pow2_modulo_modulo_lemma_1 x (s + w) b
