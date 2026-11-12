(* Tests for codec.ml: allocation of decode, encode, get, set and validate. *)

open Wire
open Test_helpers

(* >8-field codecs must not allocate a partial-application closure per decode.
   [apply_fwd] saturates the record constructor in one call for up to 16
   fields; a regression to a narrower unroll reintroduces one [caml_curry]
   closure per decode, visible under flambda-off. The 8-vs-16 field decode
   must grow only by the 8 extra record slots -- no closure. *)
type alloc_r8 = {
  a1 : UInt8.t;
  a2 : UInt8.t;
  a3 : UInt8.t;
  a4 : UInt8.t;
  a5 : UInt8.t;
  a6 : UInt8.t;
  a7 : UInt8.t;
  a8 : UInt8.t;
}

type alloc_r16 = {
  b1 : UInt8.t;
  b2 : UInt8.t;
  b3 : UInt8.t;
  b4 : UInt8.t;
  b5 : UInt8.t;
  b6 : UInt8.t;
  b7 : UInt8.t;
  b8 : UInt8.t;
  b9 : UInt8.t;
  b10 : UInt8.t;
  b11 : UInt8.t;
  b12 : UInt8.t;
  b13 : UInt8.t;
  b14 : UInt8.t;
  b15 : UInt8.t;
  b16 : UInt8.t;
}

type alloc_r17 = {
  c1 : UInt8.t;
  c2 : UInt8.t;
  c3 : UInt8.t;
  c4 : UInt8.t;
  c5 : UInt8.t;
  c6 : UInt8.t;
  c7 : UInt8.t;
  c8 : UInt8.t;
  c9 : UInt8.t;
  c10 : UInt8.t;
  c11 : UInt8.t;
  c12 : UInt8.t;
  c13 : UInt8.t;
  c14 : UInt8.t;
  c15 : UInt8.t;
  c16 : UInt8.t;
  c17 : UInt8.t;
}

let alloc_codec8 =
  Codec.v "Alloc8"
    (fun a1 a2 a3 a4 a5 a6 a7 a8 ->
      ({ a1; a2; a3; a4; a5; a6; a7; a8 } : alloc_r8))
    Codec.
      [
        (Field.v "a1" uint8 $ fun (r : alloc_r8) -> r.a1);
        (Field.v "a2" uint8 $ fun (r : alloc_r8) -> r.a2);
        (Field.v "a3" uint8 $ fun (r : alloc_r8) -> r.a3);
        (Field.v "a4" uint8 $ fun (r : alloc_r8) -> r.a4);
        (Field.v "a5" uint8 $ fun (r : alloc_r8) -> r.a5);
        (Field.v "a6" uint8 $ fun (r : alloc_r8) -> r.a6);
        (Field.v "a7" uint8 $ fun (r : alloc_r8) -> r.a7);
        (Field.v "a8" uint8 $ fun (r : alloc_r8) -> r.a8);
      ]

let alloc_codec16 =
  Codec.v "Alloc16"
    (fun b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 ->
      ({ b1; b2; b3; b4; b5; b6; b7; b8; b9; b10; b11; b12; b13; b14; b15; b16 }
        : alloc_r16))
    Codec.
      [
        (Field.v "b1" uint8 $ fun (r : alloc_r16) -> r.b1);
        (Field.v "b2" uint8 $ fun (r : alloc_r16) -> r.b2);
        (Field.v "b3" uint8 $ fun (r : alloc_r16) -> r.b3);
        (Field.v "b4" uint8 $ fun (r : alloc_r16) -> r.b4);
        (Field.v "b5" uint8 $ fun (r : alloc_r16) -> r.b5);
        (Field.v "b6" uint8 $ fun (r : alloc_r16) -> r.b6);
        (Field.v "b7" uint8 $ fun (r : alloc_r16) -> r.b7);
        (Field.v "b8" uint8 $ fun (r : alloc_r16) -> r.b8);
        (Field.v "b9" uint8 $ fun (r : alloc_r16) -> r.b9);
        (Field.v "b10" uint8 $ fun (r : alloc_r16) -> r.b10);
        (Field.v "b11" uint8 $ fun (r : alloc_r16) -> r.b11);
        (Field.v "b12" uint8 $ fun (r : alloc_r16) -> r.b12);
        (Field.v "b13" uint8 $ fun (r : alloc_r16) -> r.b13);
        (Field.v "b14" uint8 $ fun (r : alloc_r16) -> r.b14);
        (Field.v "b15" uint8 $ fun (r : alloc_r16) -> r.b15);
        (Field.v "b16" uint8 $ fun (r : alloc_r16) -> r.b16);
      ]

let alloc_codec17 =
  Codec.v "Alloc17"
    (fun c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 ->
      ({
         c1;
         c2;
         c3;
         c4;
         c5;
         c6;
         c7;
         c8;
         c9;
         c10;
         c11;
         c12;
         c13;
         c14;
         c15;
         c16;
         c17;
       }
        : alloc_r17))
    Codec.
      [
        (Field.v "c1" uint8 $ fun (r : alloc_r17) -> r.c1);
        (Field.v "c2" uint8 $ fun (r : alloc_r17) -> r.c2);
        (Field.v "c3" uint8 $ fun (r : alloc_r17) -> r.c3);
        (Field.v "c4" uint8 $ fun (r : alloc_r17) -> r.c4);
        (Field.v "c5" uint8 $ fun (r : alloc_r17) -> r.c5);
        (Field.v "c6" uint8 $ fun (r : alloc_r17) -> r.c6);
        (Field.v "c7" uint8 $ fun (r : alloc_r17) -> r.c7);
        (Field.v "c8" uint8 $ fun (r : alloc_r17) -> r.c8);
        (Field.v "c9" uint8 $ fun (r : alloc_r17) -> r.c9);
        (Field.v "c10" uint8 $ fun (r : alloc_r17) -> r.c10);
        (Field.v "c11" uint8 $ fun (r : alloc_r17) -> r.c11);
        (Field.v "c12" uint8 $ fun (r : alloc_r17) -> r.c12);
        (Field.v "c13" uint8 $ fun (r : alloc_r17) -> r.c13);
        (Field.v "c14" uint8 $ fun (r : alloc_r17) -> r.c14);
        (Field.v "c15" uint8 $ fun (r : alloc_r17) -> r.c15);
        (Field.v "c16" uint8 $ fun (r : alloc_r17) -> r.c16);
        (Field.v "c17" uint8 $ fun (r : alloc_r17) -> r.c17);
      ]

(* 32 fields is the saturated-table ceiling: it decodes in one saturated
   [make] call, no closure. 33 fields is the first arity past the table, so it
   makes one partial application and exercises the recursive unroll. *)
type alloc_r32 = {
  d1 : UInt8.t;
  d2 : UInt8.t;
  d3 : UInt8.t;
  d4 : UInt8.t;
  d5 : UInt8.t;
  d6 : UInt8.t;
  d7 : UInt8.t;
  d8 : UInt8.t;
  d9 : UInt8.t;
  d10 : UInt8.t;
  d11 : UInt8.t;
  d12 : UInt8.t;
  d13 : UInt8.t;
  d14 : UInt8.t;
  d15 : UInt8.t;
  d16 : UInt8.t;
  d17 : UInt8.t;
  d18 : UInt8.t;
  d19 : UInt8.t;
  d20 : UInt8.t;
  d21 : UInt8.t;
  d22 : UInt8.t;
  d23 : UInt8.t;
  d24 : UInt8.t;
  d25 : UInt8.t;
  d26 : UInt8.t;
  d27 : UInt8.t;
  d28 : UInt8.t;
  d29 : UInt8.t;
  d30 : UInt8.t;
  d31 : UInt8.t;
  d32 : UInt8.t;
}

type alloc_r33 = {
  e1 : UInt8.t;
  e2 : UInt8.t;
  e3 : UInt8.t;
  e4 : UInt8.t;
  e5 : UInt8.t;
  e6 : UInt8.t;
  e7 : UInt8.t;
  e8 : UInt8.t;
  e9 : UInt8.t;
  e10 : UInt8.t;
  e11 : UInt8.t;
  e12 : UInt8.t;
  e13 : UInt8.t;
  e14 : UInt8.t;
  e15 : UInt8.t;
  e16 : UInt8.t;
  e17 : UInt8.t;
  e18 : UInt8.t;
  e19 : UInt8.t;
  e20 : UInt8.t;
  e21 : UInt8.t;
  e22 : UInt8.t;
  e23 : UInt8.t;
  e24 : UInt8.t;
  e25 : UInt8.t;
  e26 : UInt8.t;
  e27 : UInt8.t;
  e28 : UInt8.t;
  e29 : UInt8.t;
  e30 : UInt8.t;
  e31 : UInt8.t;
  e32 : UInt8.t;
  e33 : UInt8.t;
}

let alloc_codec32 =
  Codec.v "Alloc32"
    (fun d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20
         d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 ->
      ({
         d1;
         d2;
         d3;
         d4;
         d5;
         d6;
         d7;
         d8;
         d9;
         d10;
         d11;
         d12;
         d13;
         d14;
         d15;
         d16;
         d17;
         d18;
         d19;
         d20;
         d21;
         d22;
         d23;
         d24;
         d25;
         d26;
         d27;
         d28;
         d29;
         d30;
         d31;
         d32;
       }
        : alloc_r32))
    Codec.
      [
        (Field.v "d1" uint8 $ fun (r : alloc_r32) -> r.d1);
        (Field.v "d2" uint8 $ fun (r : alloc_r32) -> r.d2);
        (Field.v "d3" uint8 $ fun (r : alloc_r32) -> r.d3);
        (Field.v "d4" uint8 $ fun (r : alloc_r32) -> r.d4);
        (Field.v "d5" uint8 $ fun (r : alloc_r32) -> r.d5);
        (Field.v "d6" uint8 $ fun (r : alloc_r32) -> r.d6);
        (Field.v "d7" uint8 $ fun (r : alloc_r32) -> r.d7);
        (Field.v "d8" uint8 $ fun (r : alloc_r32) -> r.d8);
        (Field.v "d9" uint8 $ fun (r : alloc_r32) -> r.d9);
        (Field.v "d10" uint8 $ fun (r : alloc_r32) -> r.d10);
        (Field.v "d11" uint8 $ fun (r : alloc_r32) -> r.d11);
        (Field.v "d12" uint8 $ fun (r : alloc_r32) -> r.d12);
        (Field.v "d13" uint8 $ fun (r : alloc_r32) -> r.d13);
        (Field.v "d14" uint8 $ fun (r : alloc_r32) -> r.d14);
        (Field.v "d15" uint8 $ fun (r : alloc_r32) -> r.d15);
        (Field.v "d16" uint8 $ fun (r : alloc_r32) -> r.d16);
        (Field.v "d17" uint8 $ fun (r : alloc_r32) -> r.d17);
        (Field.v "d18" uint8 $ fun (r : alloc_r32) -> r.d18);
        (Field.v "d19" uint8 $ fun (r : alloc_r32) -> r.d19);
        (Field.v "d20" uint8 $ fun (r : alloc_r32) -> r.d20);
        (Field.v "d21" uint8 $ fun (r : alloc_r32) -> r.d21);
        (Field.v "d22" uint8 $ fun (r : alloc_r32) -> r.d22);
        (Field.v "d23" uint8 $ fun (r : alloc_r32) -> r.d23);
        (Field.v "d24" uint8 $ fun (r : alloc_r32) -> r.d24);
        (Field.v "d25" uint8 $ fun (r : alloc_r32) -> r.d25);
        (Field.v "d26" uint8 $ fun (r : alloc_r32) -> r.d26);
        (Field.v "d27" uint8 $ fun (r : alloc_r32) -> r.d27);
        (Field.v "d28" uint8 $ fun (r : alloc_r32) -> r.d28);
        (Field.v "d29" uint8 $ fun (r : alloc_r32) -> r.d29);
        (Field.v "d30" uint8 $ fun (r : alloc_r32) -> r.d30);
        (Field.v "d31" uint8 $ fun (r : alloc_r32) -> r.d31);
        (Field.v "d32" uint8 $ fun (r : alloc_r32) -> r.d32);
      ]

let alloc_codec33 =
  Codec.v "Alloc33"
    (fun e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 e18 e19 e20
         e21 e22 e23 e24 e25 e26 e27 e28 e29 e30 e31 e32 e33 ->
      ({
         e1;
         e2;
         e3;
         e4;
         e5;
         e6;
         e7;
         e8;
         e9;
         e10;
         e11;
         e12;
         e13;
         e14;
         e15;
         e16;
         e17;
         e18;
         e19;
         e20;
         e21;
         e22;
         e23;
         e24;
         e25;
         e26;
         e27;
         e28;
         e29;
         e30;
         e31;
         e32;
         e33;
       }
        : alloc_r33))
    Codec.
      [
        (Field.v "e1" uint8 $ fun (r : alloc_r33) -> r.e1);
        (Field.v "e2" uint8 $ fun (r : alloc_r33) -> r.e2);
        (Field.v "e3" uint8 $ fun (r : alloc_r33) -> r.e3);
        (Field.v "e4" uint8 $ fun (r : alloc_r33) -> r.e4);
        (Field.v "e5" uint8 $ fun (r : alloc_r33) -> r.e5);
        (Field.v "e6" uint8 $ fun (r : alloc_r33) -> r.e6);
        (Field.v "e7" uint8 $ fun (r : alloc_r33) -> r.e7);
        (Field.v "e8" uint8 $ fun (r : alloc_r33) -> r.e8);
        (Field.v "e9" uint8 $ fun (r : alloc_r33) -> r.e9);
        (Field.v "e10" uint8 $ fun (r : alloc_r33) -> r.e10);
        (Field.v "e11" uint8 $ fun (r : alloc_r33) -> r.e11);
        (Field.v "e12" uint8 $ fun (r : alloc_r33) -> r.e12);
        (Field.v "e13" uint8 $ fun (r : alloc_r33) -> r.e13);
        (Field.v "e14" uint8 $ fun (r : alloc_r33) -> r.e14);
        (Field.v "e15" uint8 $ fun (r : alloc_r33) -> r.e15);
        (Field.v "e16" uint8 $ fun (r : alloc_r33) -> r.e16);
        (Field.v "e17" uint8 $ fun (r : alloc_r33) -> r.e17);
        (Field.v "e18" uint8 $ fun (r : alloc_r33) -> r.e18);
        (Field.v "e19" uint8 $ fun (r : alloc_r33) -> r.e19);
        (Field.v "e20" uint8 $ fun (r : alloc_r33) -> r.e20);
        (Field.v "e21" uint8 $ fun (r : alloc_r33) -> r.e21);
        (Field.v "e22" uint8 $ fun (r : alloc_r33) -> r.e22);
        (Field.v "e23" uint8 $ fun (r : alloc_r33) -> r.e23);
        (Field.v "e24" uint8 $ fun (r : alloc_r33) -> r.e24);
        (Field.v "e25" uint8 $ fun (r : alloc_r33) -> r.e25);
        (Field.v "e26" uint8 $ fun (r : alloc_r33) -> r.e26);
        (Field.v "e27" uint8 $ fun (r : alloc_r33) -> r.e27);
        (Field.v "e28" uint8 $ fun (r : alloc_r33) -> r.e28);
        (Field.v "e29" uint8 $ fun (r : alloc_r33) -> r.e29);
        (Field.v "e30" uint8 $ fun (r : alloc_r33) -> r.e30);
        (Field.v "e31" uint8 $ fun (r : alloc_r33) -> r.e31);
        (Field.v "e32" uint8 $ fun (r : alloc_r33) -> r.e32);
        (Field.v "e33" uint8 $ fun (r : alloc_r33) -> r.e33);
      ]

let per_decode_words codec nfields =
  let buf = Bytes.make nfields '\042' in
  ignore (Sys.opaque_identity (Codec.decode_exn codec buf 0));
  let iters = 200_000 in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (Codec.decode_exn codec buf 0))
  done;
  let after = Gc.minor_words () in
  (after -. before) /. float_of_int iters

let test_decode_no_partial_closure () =
  skip_unless_gc_counters ();
  let w8 = per_decode_words alloc_codec8 8 in
  let w16 = per_decode_words alloc_codec16 16 in
  let w32 = per_decode_words alloc_codec32 32 in
  (* Each [int] record field is one word and the header is one more, so a
     closure-free decode grows by exactly the added fields. A partial-app
     closure would add several more words on top. *)
  let growth_16_8 = int_of_float (Float.round (w16 -. w8)) in
  Alcotest.(check int)
    "16-vs-8-field decode grows by 8 record slots only (no closure)" 8
    growth_16_8;
  (* A record wider than 16 fields used to trip [apply_fwd]'s recursive arm and
     allocate one [caml_curry] closure per decode. With the saturation table
     raised to 32, the 32-field decode grows by exactly its 16 extra slots. *)
  let growth_32_16 = int_of_float (Float.round (w32 -. w16)) in
  Alcotest.(check int)
    "32-vs-16-field decode grows by 16 record slots only (no closure)" 16
    growth_32_16

let test_decode_high_arity_roundtrip () =
  (* Exercise the saturated 16-field case. *)
  let buf16 = Bytes.init 16 (fun i -> Char.chr (i + 1)) in
  let v16 = Codec.decode_exn alloc_codec16 buf16 0 in
  Alcotest.(check int) "b1" 1 (UInt8.to_int v16.b1);
  Alcotest.(check int) "b16" 16 (UInt8.to_int v16.b16);
  (* 17 fields: still saturated after the table was widened to 32. *)
  let buf17 = Bytes.init 17 (fun i -> Char.chr (i + 1)) in
  let v17 = Codec.decode_exn alloc_codec17 buf17 0 in
  Alcotest.(check int) "c1" 1 (UInt8.to_int v17.c1);
  Alcotest.(check int) "c16" 16 (UInt8.to_int v17.c16);
  Alcotest.(check int) "c17" 17 (UInt8.to_int v17.c17);
  let out = Bytes.create 17 in
  Codec.encode alloc_codec17 v17 out 0;
  Alcotest.(check bytes) "17-field roundtrip" buf17 out;
  (* 32 fields: the saturated-table ceiling. *)
  let buf32 = Bytes.init 32 (fun i -> Char.chr (i + 1)) in
  let v32 = Codec.decode_exn alloc_codec32 buf32 0 in
  Alcotest.(check int) "d1" 1 (UInt8.to_int v32.d1);
  Alcotest.(check int) "d32" 32 (UInt8.to_int v32.d32);
  let out32 = Bytes.create 32 in
  Codec.encode alloc_codec32 v32 out32 0;
  Alcotest.(check bytes) "32-field roundtrip" buf32 out32;
  (* 33 fields: the first arity past the table, exercising the recursive arm. *)
  let buf33 = Bytes.init 33 (fun i -> Char.chr (i + 1)) in
  let v33 = Codec.decode_exn alloc_codec33 buf33 0 in
  Alcotest.(check int) "e1" 1 (UInt8.to_int v33.e1);
  Alcotest.(check int) "e32" 32 (UInt8.to_int v33.e32);
  Alcotest.(check int) "e33" 33 (UInt8.to_int v33.e33);
  let out33 = Bytes.create 33 in
  Codec.encode alloc_codec33 v33 out33 0;
  Alcotest.(check bytes) "33-field roundtrip" buf33 out33

(* Encoding a var-bytes field must not allocate. [var_bytes_writer]'s length
   check and string blit are top-level functions; were they local to the
   writer, each would be a heap-allocated closure per var-bytes field on
   every encode under flambda-off. *)
type alloc_vb = { a : string; b : string }

let alloc_vb_alen = Field.v "ALen" uint16be
let alloc_vb_blen = Field.v "BLen" uint16be

let alloc_vb_codec =
  Codec.v "AllocVb"
    (fun _alen a _blen b -> { a; b })
    Codec.
      [
        (alloc_vb_alen $ fun r -> UInt16.v (String.length r.a));
        (Field.v "A" (byte_array ~size:(Field.ref alloc_vb_alen)) $ fun r -> r.a);
        (alloc_vb_blen $ fun r -> UInt16.v (String.length r.b));
        (Field.v "B" (byte_array ~size:(Field.ref alloc_vb_blen)) $ fun r -> r.b);
      ]

let alloc_zt_codec =
  Codec.v "AllocZt" Fun.id Codec.[ Field.v "Z" zeroterm $ Fun.id ]

let test_encode_var_bytes_no_closure () =
  skip_unless_gc_counters ();
  let v = { a = String.make 32 'x'; b = String.make 16 'y' } in
  let buf = Bytes.create (2 + 32 + 2 + 16) in
  let zbuf = Bytes.create 3 in
  Codec.encode alloc_vb_codec v buf 0;
  Codec.encode alloc_zt_codec "hi" zbuf 0;
  let iters = 200_000 in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    Codec.encode alloc_vb_codec v buf 0;
    Codec.encode alloc_zt_codec "hi" zbuf 0
  done;
  let after = Gc.minor_words () in
  let words =
    int_of_float (Float.round ((after -. before) /. float_of_int iters))
  in
  Alcotest.(check int) "var-bytes encode allocates nothing" 0 words

(* [Codec.get] and [Codec.set] resolve a field's type and offset when the
   accessor is staged. Pin the per-call contract separately from functional
   correctness: rebuilding a reader closure returns the right value but still
   puts pressure on a hot-path minor heap. *)
type alloc_priority = Low | High

type alloc_accessor = {
  hi : int;
  lo : int;
  u8 : UInt8.t;
  u16 : UInt16.t;
  i32 : SInt32.t;
  priority : alloc_priority;
}

let alloc_hi = Field.v "Hi" (bits ~width:4 U8)
let alloc_lo = Field.v "Lo" (bits ~width:4 U8)
let alloc_u8 = Field.v "U8" uint8
let alloc_u16 = Field.v "U16" uint16be
let alloc_i32 = Field.v "I32" int32be

let alloc_priority =
  Field.v "Priority"
    (map
       ~decode:(fun v -> if UInt8.equal v UInt8.zero then Low else High)
       ~encode:(function Low -> UInt8.zero | High -> UInt8.v 1)
       uint8)

let alloc_bf_hi = Codec.(alloc_hi $ fun r -> r.hi)
let alloc_bf_lo = Codec.(alloc_lo $ fun r -> r.lo)
let alloc_bf_u8 = Codec.(alloc_u8 $ fun r -> r.u8)
let alloc_bf_u16 = Codec.(alloc_u16 $ fun r -> r.u16)
let alloc_bf_i32 = Codec.(alloc_i32 $ fun r -> r.i32)
let alloc_bf_priority = Codec.(alloc_priority $ fun r -> r.priority)

let alloc_accessor_codec =
  Codec.v "AllocAccessor"
    (fun hi lo u8 u16 i32 priority -> { hi; lo; u8; u16; i32; priority })
    Codec.
      [
        alloc_bf_hi;
        alloc_bf_lo;
        alloc_bf_u8;
        alloc_bf_u16;
        alloc_bf_i32;
        alloc_bf_priority;
      ]

let per_call_words f =
  ignore (Sys.opaque_identity (f ()));
  let iters = 200_000 in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.minor_words () in
  int_of_float (Float.round ((after -. before) /. float_of_int iters))

let check_no_per_call_allocation name f =
  Alcotest.(check int)
    (Fmt.str "%s allocates nothing" name)
    0 (per_call_words f)

let test_get_no_allocation () =
  skip_unless_gc_counters ();
  let buf = Bytes.make (Codec.wire_size alloc_accessor_codec) '\007' in
  let get f = Staged.unstage (Codec.get alloc_accessor_codec f) in
  let get_hi = get alloc_bf_hi
  and get_u8 = get alloc_bf_u8
  and get_u16 = get alloc_bf_u16
  and get_i32 = get alloc_bf_i32
  and get_priority = get alloc_bf_priority in
  check_no_per_call_allocation "get bits" (fun () -> get_hi buf 0);
  check_no_per_call_allocation "get uint8" (fun () -> get_u8 buf 0);
  check_no_per_call_allocation "get uint16be" (fun () -> get_u16 buf 0);
  check_no_per_call_allocation "get int32be" (fun () -> get_i32 buf 0);
  check_no_per_call_allocation "get map" (fun () -> get_priority buf 0)

let test_set_no_allocation () =
  skip_unless_gc_counters ();
  let buf = Bytes.make (Codec.wire_size alloc_accessor_codec) '\007' in
  let set f = Staged.unstage (Codec.set alloc_accessor_codec f) in
  let set_hi = set alloc_bf_hi
  and set_u8 = set alloc_bf_u8
  and set_u16 = set alloc_bf_u16
  and set_i32 = set alloc_bf_i32
  and set_priority = set alloc_bf_priority in
  (* Built once: the point of the check is the setter's own cost, not the
     carrier's constructor. *)
  let i32_v = SInt32.of_int 70_000 in
  let u16_v = UInt16.v 513 in
  check_no_per_call_allocation "set bits" (fun () -> set_hi buf 0 3);
  check_no_per_call_allocation "set uint8" (fun () -> set_u8 buf 0 (UInt8.v 7));
  check_no_per_call_allocation "set uint16be" (fun () -> set_u16 buf 0 u16_v);
  check_no_per_call_allocation "set int32be" (fun () -> set_i32 buf 0 i32_v);
  check_no_per_call_allocation "set map" (fun () -> set_priority buf 0 High)

(* {2 Resource bounds}

   Allocation is the deterministic half of cost: the same work turns over the
   same number of words on any machine, where wall-clock does not. The two
   tests below are not performance targets. They assert a shape -- how much
   memory a read path churns must not be a function of a length the sender
   picked -- on the entry points [codec.mli] tells a caller to run per packet
   on untrusted input.

   [Gc.minor_words] cannot measure it: it does not count blocks over
   [Max_young_wosize] (256 words), so a version written with it silently
   measures nothing once the payload passes 2 KiB. *)
let words_per_call ~iters f =
  ignore (Sys.opaque_identity (f ()));
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.allocated_bytes () in
  int_of_float (Float.round ((after -. before) /. float_of_int iters /. 8.))

(* Length-parameterised span types, each with a payload it accepts. All five
   hand the caller one string of about [n] bytes, so they are comparable to
   each other and to the payload; [byte_array] and [all_bytes] are the
   unrefined twins of the three that check something as they go. *)
(* The two a presence gate can hold: [optional] and [optional_or] refuse an
   inner that is neither byte-sized nor self-delimiting, since 3D has no way to
   bound one inside a gate, which rules out the terminator search and both
   greedy spans. *)
let gateable_span_typs =
  [
    ( "byte_array",
      (fun n -> byte_array ~size:(int n)),
      fun n -> Bytes.make n 'a' );
    ( "byte_array_where",
      (fun n -> byte_array_where ~size:(int n) ~per_byte:printable_byte),
      fun n -> Bytes.make n 'a' );
  ]

let span_typs =
  gateable_span_typs
  @ [
      ( "zeroterm_at_most",
        (fun n -> zeroterm_at_most ~size:(int n)),
        fun n ->
          let b = Bytes.make n 'a' in
          Bytes.set b (n - 1) '\000';
          b );
      ("all_bytes", (fun _ -> all_bytes), fun n -> Bytes.make n 'a');
      ("all_zeros", (fun _ -> all_zeros), fun n -> Bytes.make n '\000');
    ]

let span_family name typ =
  Codec.v name Fun.id Codec.[ Field.v "d" typ $ Fun.id ]

let span_families =
  List.map
    (fun (name, typ, payload) ->
      (name, (fun n -> span_family ("Span" ^ name) (typ n)), payload))
    span_typs

(* The same spans behind a presence gate. [optional] and [optional_or] lay out
   their inner themselves instead of going through the plain field path, and
   each has a branch for a gate the schema fixes and one for a gate the frame
   carries, so a scan wired into one of the four says nothing about the other
   three. A leading gate byte puts the span at offset 1. *)
let f_span_gate = Field.v "gate" uint8
let gate_is_set = Expr.(Field.ref f_span_gate = int 1)

let gated_payload payload n =
  let src = payload n in
  let b = Bytes.make (Bytes.length src + 1) '\001' in
  Bytes.blit src 0 b 1 (Bytes.length src);
  b

let optional_family name ~present typ =
  Codec.v name
    (fun _ d -> d)
    Codec.
      [
        (f_span_gate $ fun _ -> UInt8.v 1);
        Field.optional "d" ~present typ $ Fun.id;
      ]

let optional_or_family name ~present typ =
  Codec.v name
    (fun _ d -> d)
    Codec.
      [
        (f_span_gate $ fun _ -> UInt8.v 1);
        Field.optional_or "d" ~present ~default:"" typ $ Fun.id;
      ]

(* A [name, validate this size] pair, so gates whose codecs have different
   record types measure through one fold. *)
let validate_case name mk payload n =
  let c = mk n and buf = gated_payload payload n in
  (name, fun () -> Codec.validate c buf 0)

let gated_span_cases n =
  List.concat_map
    (fun (name, typ, payload) ->
      [
        validate_case ("optional(true)/" ^ name)
          (fun n -> optional_family "OptT" ~present:Expr.true_ (typ n))
          payload n;
        validate_case ("optional(gate)/" ^ name)
          (fun n -> optional_family "OptG" ~present:gate_is_set (typ n))
          payload n;
        validate_case
          ("optional_or(true)/" ^ name)
          (fun n -> optional_or_family "OptOrT" ~present:Expr.true_ (typ n))
          payload n;
        validate_case
          ("optional_or(gate)/" ^ name)
          (fun n -> optional_or_family "OptOrG" ~present:gate_is_set (typ n))
          payload n;
      ])
    gateable_span_typs

let report_span_failures what failures =
  if failures <> [] then
    Alcotest.failf "%s: %s" what (String.concat "; " (List.rev failures))

(* [Codec.validate] returns unit, so everything it allocates it drops on the
   floor. What it turns over must therefore not depend on how long the sender
   made the frame. The two runs compared differ in nothing but a number the
   sender chose, which is what makes the growth between them attributable. A
   validator that copies the span it is scanning gives an attacker a heap
   multiplier on a path a server runs once per packet. *)
let test_validate_allocation_is_bounded () =
  skip_unless_gc_counters ();
  let small = 512 and large = 4096 in
  let cases n =
    List.map
      (fun (name, mk, mkbuf) ->
        let c = mk n and buf = mkbuf n in
        (name, fun () -> Codec.validate c buf 0))
      span_families
    @ gated_span_cases n
  in
  let failures =
    List.fold_left2
      (fun acc (name, at_small) (_, at_large) ->
        let at_small = words_per_call ~iters:2000 at_small in
        let at_large = words_per_call ~iters:2000 at_large in
        if at_large - at_small > 32 then
          Fmt.str
            "%s grows %d words between a %d-byte and a %d-byte frame (%d then \
             %d)"
            name (at_large - at_small) small large at_small at_large
          :: acc
        else acc)
      [] (cases small) (cases large)
  in
  report_span_failures "validate allocation scales with the frame length"
    failures

(* A [repeat]'s element count comes from its byte budget, which comes from a
   length field: the sender picks how many times the element loop runs.
   [Codec.validate] wants those elements checked and does not want the
   sequence, so building one and dropping it handed an attacker a heap
   multiplier per packet. The byte spans and the 64-bit and floating-point
   scalars are here for the same reason one step in: their reader's only product
   is the value, boxed for the wide scalars and copied for the spans, so on a
   walk that keeps nothing they have nothing to do. A sub-codec element is here
   because it carries its own validator: the C loop EverParse generates calls
   that validator and keeps one position, so validating a repeat of them must
   keep no more, where building a record per element would hand the sender the
   multiplier again. What an element's reader has to do to check it (a
   casetype's tag dispatch, a [map]'s conversion) is a separate question, so
   those stay out. *)
let f_repeat_len = Field.v "len" uint16be

let repeat_budget_codec elem =
  Codec.v "RepeatBudget"
    (fun _ xs -> xs)
    Codec.
      [
        (f_repeat_len $ fun xs -> UInt16.v (List.length xs));
        Field.repeat "xs" ~size:(Field.ref f_repeat_len) elem $ Fun.id;
      ]

let repeat_budget_buf n =
  let b = Bytes.make (2 + n) '\001' in
  Bytes.set_uint16_be b 0 n;
  b

(* A NUL every fourth byte, so a repeat of NUL-terminated elements splits any
   multiple-of-four budget into four-byte elements. *)
let repeat_zeroterm_buf n =
  let b = Bytes.make (2 + n) 'a' in
  Bytes.set_uint16_be b 0 n;
  for i = 0 to n - 1 do
    if i mod 4 = 3 then Bytes.set b (2 + i) '\000'
  done;
  b

(* A [name, validate this budget] pair, so elements of different value types
   measure through one fold. *)
let repeat_budget_case ?(buf = repeat_budget_buf) name elem =
  let c = repeat_budget_codec elem in
  ( name,
    fun n ->
      let buf = buf n in
      fun () -> Codec.validate c buf 0 )

(* Four bytes wide, so a 512- and a 4096-byte budget both divide into whole
   elements, and constrained, so the element has something to check. *)
let repeat_elem_codec =
  Codec.v "RepeatElem"
    (fun a b c d -> (a, b, c, d))
    Codec.
      [
        ( Field.v "a" uint8 ~self_constraint:(fun r -> Expr.(r <> int 0))
        $ fun (a, _, _, _) -> a );
        (Field.v "b" uint8 $ fun (_, b, _, _) -> b);
        (Field.v "c" uint8 $ fun (_, _, c, _) -> c);
        (Field.v "d" uint8 $ fun (_, _, _, d) -> d);
      ]

let test_repeat_validate_allocation_is_bounded () =
  skip_unless_gc_counters ();
  let small = 512 and large = 4096 in
  let failures =
    List.fold_left
      (fun acc (name, at) ->
        let words n = words_per_call ~iters:500 (at n) in
        let at_small = words small and at_large = words large in
        if at_large <> at_small then
          Fmt.str
            "%s grows %d words between a %d-byte and a %d-byte budget (%d then \
             %d)"
            name (at_large - at_small) small large at_small at_large
          :: acc
        else acc)
      []
      [
        repeat_budget_case "uint8" uint8;
        repeat_budget_case "uint16be" uint16be;
        repeat_budget_case "uint32be" uint32be;
        repeat_budget_case "int8" int8;
        repeat_budget_case "int16be" int16be;
        repeat_budget_case "uint64be" uint64be;
        repeat_budget_case "int64be" int64be;
        repeat_budget_case "float32be" float32be;
        repeat_budget_case "float64be" float64be;
        repeat_budget_case "byte_array" (byte_array ~size:(int 4));
        repeat_budget_case "byte_slice" (byte_slice ~size:(int 4));
        repeat_budget_case ~buf:repeat_zeroterm_buf "zeroterm" zeroterm;
        repeat_budget_case "codec" (codec repeat_elem_codec);
      ]
  in
  report_span_failures "validate allocation scales with a repeat's budget"
    failures

(* A sub-codec as a plain field runs the sub-codec's own validator, so the
   record never reaches the heap. Reaching the same sub-codec through a
   container -- an [array] of them, a [nested] region holding one, a casetype
   case body -- has to cost the same: a container has nothing of its own to
   build, so whatever it turns over is the record [Codec.validate] exists not
   to build. Measured against the same sub-codec as a plain field rather than
   against a fixed number, so what is pinned is the shape and not this codec's
   own cost. *)
let container_elem_codec =
  Codec.v "ContainerElem"
    (fun a b -> (a, b))
    Codec.
      [
        ( Field.v "a" uint8 ~self_constraint:(fun r -> Expr.(r <> int 0))
        $ fun (a, _) -> a );
        (Field.v "b" uint8 $ fun (_, b) -> b);
      ]

(* Tag 1 selects the sub-codec body; the second case is there so the tag is
   dispatched rather than assumed. Neither [inject] allocates, so what the
   measurement sees is the container and not the case's own injection. *)
let container_case_typ =
  casetype "ContainerCase" uint8
    [
      case ~index:(UInt8.v 1) (codec container_elem_codec) ~inject:Fun.id
        ~project:(fun v -> Some v);
      case ~index:(UInt8.v 2) uint16be
        ~inject:(fun v ->
          let v = UInt16.to_int v in
          (UInt8.v (v lsr 8), UInt8.v (v land 0xff)))
        ~project:(fun (a, b) ->
          Some (UInt16.v ((UInt8.to_int a lsl 8) lor UInt8.to_int b)));
    ]

let container_words name typ buf =
  let c = Codec.v name Fun.id Codec.[ Field.v "x" typ $ Fun.id ] in
  words_per_call ~iters:5000 (fun () -> Codec.validate c buf 0)

let test_container_subcodec_validate_no_alloc () =
  skip_unless_gc_counters ();
  let ones n = Bytes.make n '\001' in
  let direct = container_words "Direct" (codec container_elem_codec) (ones 2) in
  let failures =
    List.fold_left
      (fun acc (name, words) ->
        if words <> direct then
          Fmt.str
            "a sub-codec through %s costs %d words a validate where the same \
             sub-codec as a field costs %d"
            name words direct
          :: acc
        else acc)
      []
      [
        ( "an array of 4",
          container_words "ArrayOf4"
            (array ~len:(int 4) (codec container_elem_codec))
            (ones 8) );
        ( "an array of 32",
          container_words "ArrayOf32"
            (array ~len:(int 32) (codec container_elem_codec))
            (ones 64) );
        ( "a nested region",
          container_words "NestedOne"
            (nested ~size:(int 2) (codec container_elem_codec))
            (ones 2) );
        ( "a casetype case body",
          container_words "CaseBody" container_case_typ (ones 3) );
      ]
  in
  report_span_failures "a container decodes a sub-codec validate only checks"
    failures

(* A closed [enum] checks membership once per decoded element, and both sides of
   that cost are the sender's: the element count comes from the byte budget, and
   a protocol enum names as many codes as it likes. Validating a repeat of them
   must turn over the same words whatever the budget and whatever the case
   count, or naming 128 codes multiplies the heap a packet churns by the number
   of elements it carries. *)
let enum_of_case_count n =
  enum "Codes" (List.init n (fun i -> (Fmt.str "C%d" i, i))) uint8

let enum_budget_words ~cases ~budget =
  let c = repeat_budget_codec (enum_of_case_count cases) in
  let buf = repeat_budget_buf budget in
  words_per_call ~iters:500 (fun () -> Codec.validate c buf 0)

let test_enum_element_validate_allocation_is_bounded () =
  skip_unless_gc_counters ();
  let small = 512 and large = 4096 in
  let failures =
    List.fold_left
      (fun acc cases ->
        let at_small = enum_budget_words ~cases ~budget:small in
        let at_large = enum_budget_words ~cases ~budget:large in
        if at_large <> at_small then
          Fmt.str
            "a %d-case enum grows %d words between a %d-byte and a %d-byte \
             budget (%d then %d)"
            cases (at_large - at_small) small large at_small at_large
          :: acc
        else acc)
      [] [ 2; 128 ]
  in
  let few = enum_budget_words ~cases:2 ~budget:large in
  let many = enum_budget_words ~cases:128 ~budget:large in
  let failures =
    if many <> few then
      Fmt.str "a 128-case enum costs %d words where a 2-case one costs %d" many
        few
      :: failures
    else failures
  in
  report_span_failures "validate allocation scales with a closed enum" failures

(* A check-free codec skips the validator pass, so it never builds an
   evaluation context. One with a constraint or a where clause does run the
   pass, and a parameter-free codec has nothing to put in the context it used to
   build on the way in. [Codec.validate] is the entry point a server runs per
   packet, so what it costs must not depend on whether the codec checks
   anything. *)
let checked_no_param_codecs =
  let f_a = Field.v "a" uint8 in
  let f_bounded =
    Field.v "b" ~self_constraint:(fun self -> Expr.(self < int 200)) uint8
  in
  [
    ( "field constraint",
      Codec.v "Constrained"
        (fun a b -> (a, b))
        Codec.[ (f_a $ fun (a, _) -> a); (f_bounded $ fun (_, b) -> b) ] );
    ( "where clause",
      Codec.v "Whered"
        ~where:Expr.(Field.ref f_a < int 200)
        (fun a b -> (a, b))
        Codec.[ (f_a $ fun (a, _) -> a); (Field.v "b" uint8 $ fun (_, b) -> b) ]
    );
  ]

let test_validate_checked_no_param_no_alloc () =
  skip_unless_gc_counters ();
  let buf = Bytes.make 8 '\001' in
  let failures =
    List.fold_left
      (fun acc (name, c) ->
        let words =
          words_per_call ~iters:5000 (fun () -> Codec.validate c buf 0)
        in
        if words <> 0 then
          Fmt.str "a codec with a %s costs %d words a validate" name words
          :: acc
        else acc)
      [] checked_no_param_codecs
  in
  report_span_failures "a parameter-free validate allocates" failures

(* [Codec.decode] hands the span back, so one copy of it is the price of the
   answer. A second copy is work the caller never sees and the sender sizes.
   The bound is against the payload the returned value carries, not against
   another run of the same decoder: a decoder measured against itself would
   agree with any amount of copying. *)
let test_decode_allocates_one_copy () =
  skip_unless_gc_counters ();
  let n = 4096 in
  let payload = n / 8 in
  let failures =
    List.fold_left
      (fun acc (name, mk, mkbuf) ->
        let c = mk n and buf = mkbuf n in
        let words =
          words_per_call ~iters:2000 (fun () -> Codec.decode c buf 0)
        in
        if words > payload + 32 then
          Fmt.str "%s allocates %d words for a %d-word payload" name words
            payload
          :: acc
        else acc)
      [] span_families
  in
  report_span_failures "decode allocates more than the value it returns"
    failures

let suite =
  ( "codec_alloc",
    [
      Alcotest.test_case "decode: >8 fields allocate no partial closure" `Quick
        test_decode_no_partial_closure;
      Alcotest.test_case "decode: high-arity roundtrip" `Quick
        test_decode_high_arity_roundtrip;
      Alcotest.test_case "encode: var-bytes fields allocate nothing" `Quick
        test_encode_var_bytes_no_closure;
      Alcotest.test_case "get: immediate fields allocate nothing" `Quick
        test_get_no_allocation;
      Alcotest.test_case "set: immediate fields allocate nothing" `Quick
        test_set_no_allocation;
      Alcotest.test_case "validate: allocation does not scale with the frame"
        `Quick test_validate_allocation_is_bounded;
      Alcotest.test_case
        "validate: allocation does not scale with a repeat's budget" `Quick
        test_repeat_validate_allocation_is_bounded;
      Alcotest.test_case
        "validate: allocation does not scale with an enum's cases" `Quick
        test_enum_element_validate_allocation_is_bounded;
      Alcotest.test_case
        "validate: a sub-codec in a container costs what it costs as a field"
        `Quick test_container_subcodec_validate_no_alloc;
      Alcotest.test_case
        "validate: a checked param-free codec allocates nothing" `Quick
        test_validate_checked_no_param_no_alloc;
      Alcotest.test_case "decode: allocates at most one copy of the span" `Quick
        test_decode_allocates_one_copy;
    ] )
