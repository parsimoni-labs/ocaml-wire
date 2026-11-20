module Spec.Ipv4

(* M3, the route to "the scanner accepts exactly what the 3D spec accepts",
   on the first four bytes of the IPv4 header of examples/net: the byte
   holding version and IHL, the byte holding DSCP and ECN, and
   total_length, with version = 4, ihl >= 5 and total_length >= ihl * 4.
   The version and IHL checks are one range of their byte (Planner.word_range,
   proved in Plan.Word), so the plan reads the byte whole and IHL is
   [byte - 0x40]. The parser is the LowParse combinator EverParse's 3D
   produces for
     UINT8 vihl { 0x45 <= vihl && vihl <= 0x4f }; UINT8 tos;
     UINT16BE total_length { total_length >= (vihl - 0x40) * 4 } *)

open FStar.Mul
open LowParse.Spec.Combinators
open LowParse.Spec.Int
open Plan.Expr
open Plan.Scan
open Plan.Proof

module U8 = FStar.UInt8
module U16 = FStar.UInt16
module E = FStar.Endianness
module Seq = FStar.Seq

let ok (x:(U8.t & U8.t) & U16.t) : bool =
  let ((b, _), tl) = x in
  0x45 <= U8.v b && U8.v b <= 0x4f && U16.v tl >= (U8.v b - 64) * 4

let p3d = ((parse_u8 `nondep_then` parse_u8) `nondep_then` parse_u16) `parse_filter` ok

(* The plan Planner makes of it, the window's length in variable 0. *)
let lim : var = 0
let vihl : var = 1
let tos : var = 2
let total_length : var = 3

let plan : list node = [
  Prefix [ { id = vihl; off = 0; width = 1 }; { id = tos; off = 1; width = 1 };
           { id = total_length; off = 2; width = 2 } ] 4;
  Test (Ge (V vihl) (Lit 0x45));
  Test (Ge (Lit 0x4f) (V vihl));
  Test (Ge (V total_length) (Shl (Sub (V vihl) (Lit 64)) 2));
]

let plan_wf () : Lemma (wf (start lim).sc plan) = assert_norm (wf (start lim).sc plan)

(* The window's bytes, big-endian. *)
let read_be (s:LowParse.Bytes.bytes) (o:int) (w:nat) : v:nat{v < pow2 (8 * w)} =
  if 0 <= o && o + w <= Seq.length s then begin
    E.lemma_be_to_n_is_bounded (Seq.slice s o (o + w));
    E.be_to_n (Seq.slice s o (o + w))
  end
  else 0

let reader_of (s:LowParse.Bytes.bytes) : reader = read_be s

let accepted (o:outcome) : bool = Accept? o

(* What the integer parsers consume and read. *)
let u8_is (s:LowParse.Bytes.bytes)
  : Lemma (match parse parse_u8 s with
           | Some (v, c) -> c == 1 /\ Seq.length s >= 1 /\ U8.v v == E.be_to_n (Seq.slice s 0 1)
           | None -> Seq.length s < 1)
  = parser_kind_prop_equiv parse_u8_kind parse_u8;
    if Seq.length s >= 1 then parse_u8_spec s

let u16_is (s:LowParse.Bytes.bytes)
  : Lemma (match parse parse_u16 s with
           | Some (v, c) -> c == 2 /\ Seq.length s >= 2 /\ U16.v v == E.be_to_n (Seq.slice s 0 2)
           | None -> Seq.length s < 2)
  = parser_kind_prop_equiv parse_u16_kind parse_u16;
    if Seq.length s >= 2 then parse_u16_spec s

(* The 3D parser accepts the bytes exactly when there are four and the
   checks hold on the big-endian values. *)
let p3d_is (s:LowParse.Bytes.bytes)
  : Lemma (Some? (parse p3d s) <==>
           (Seq.length s >= 4 /\
            (let b = E.be_to_n (Seq.slice s 0 1) in
             let tl = E.be_to_n (Seq.slice s 2 4) in
             0x45 <= b /\ b <= 0x4f /\ tl >= (b - 64) * 4)))
  = let len = Seq.length s in
    parse_filter_eq ((parse_u8 `nondep_then` parse_u8) `nondep_then` parse_u16) ok s;
    nondep_then_eq (parse_u8 `nondep_then` parse_u8) parse_u16 s;
    nondep_then_eq parse_u8 parse_u8 s;
    u8_is s;
    if len >= 1 then begin
      u8_is (Seq.slice s 1 len);
      if len >= 2 then begin
        u16_is (Seq.slice s 2 len);
        if len >= 4 then Seq.slice_slice s 2 len 0 2
      end
    end

#push-options "--fuel 8 --ifuel 2 --z3rlimit 100"
(* The codec's walk of the plan accepts on the same condition. *)
let spec_is (s:LowParse.Bytes.bytes) (en:env)
  : Lemma (requires Seq.length s <= max_int)
          (ensures accepted (spec (Seq.length s) (reader_of s) en 0 plan) <==>
                   (Seq.length s >= 4 /\
                    (let b = E.be_to_n (Seq.slice s 0 1) in
                     let tl = E.be_to_n (Seq.slice s 2 4) in
                     0x45 <= b /\ b <= 0x4f /\ tl >= (b - 64) * 4)))
  = let len = Seq.length s in
    let rd = reader_of s in
    let fs = [ { id = vihl; off = 0; width = 1 }; { id = tos; off = 1; width = 1 };
               { id = total_length; off = 2; width = 2 } ] in
    let c1 = Ge (V vihl) (Lit 0x45) in
    let c2 = Ge (Lit 0x4f) (V vihl) in
    let c3 = Ge (V total_length) (Shl (Sub (V vihl) (Lit 64)) 2) in
    assert_norm (plan == [ Prefix fs 4; Test c1; Test c2; Test c3 ]);
    if len >= 4 then begin
      E.lemma_be_to_n_is_bounded (Seq.slice s 0 1);
      E.lemma_be_to_n_is_bounded (Seq.slice s 2 4);
      let b = E.be_to_n (Seq.slice s 0 1) in
      let tl = E.be_to_n (Seq.slice s 2 4) in
      let en' = read_fields rd en 0 fs in
      let en3 = upd (upd (upd en vihl (rd 0 1)) tos (rd 1 1)) total_length (rd 2 2) in
      assert (en' == en3);
      assert (rd 0 1 == b);
      assert (2 + 2 <= len);
      assert (read_be s 2 2 == E.be_to_n (Seq.slice s 2 (2 + 2)));
      assert (rd 2 2 == read_be s 2 2);
      assert (Seq.slice s 2 (2 + 2) == Seq.slice s 2 4);
      assert (E.be_to_n (Seq.slice s 2 (2 + 2)) == E.be_to_n (Seq.slice s 2 4));
      assert (E.be_to_n (Seq.slice s 2 4) == tl);
      assert (rd 2 2 == tl);
      assert (en' vihl == b /\ en' total_length == tl);
      assert_norm (spec len rd en 0 plan ==
                   (if 0 + 4 > len then Reject
                    else match holds (read_fields rd en 0 fs) c1 with
                      | Some true ->
                        (match holds (read_fields rd en 0 fs) c2 with
                         | Some true ->
                           (match holds (read_fields rd en 0 fs) c3 with
                            | Some true -> Accept (read_fields rd en 0 fs)
                            | _ -> Reject)
                         | _ -> Reject)
                      | _ -> Reject));
      assert_norm (pow2 2 == 4);
      assert_norm (pow2 62 == 4611686018427387904);
      assert (holds en' c1 == Some (b >= 0x45));
      assert (holds en' c2 == Some (0x4f >= b));
      assert (holds en' c3 == Some (tl >= (b - 64) * 4))
    end
#pop-options

let spec_iff (s:LowParse.Bytes.bytes) (en:env)
  : Lemma (requires Seq.length s <= max_int)
          (ensures accepted (spec (Seq.length s) (reader_of s) en 0 plan) <==> Some? (parse p3d s))
  = p3d_is s; spec_is s en

(* The code the emitter writes for this plan, with or without O1, accepts
   exactly the bytes the 3D parser accepts. *)
let scanner_is_3d (o:opts) (s:LowParse.Bytes.bytes) (en:env)
  : Lemma (requires Seq.length s <= max_string_length /\ en lim == Seq.length s)
          (ensures accepted (exec o (Seq.length s) lim (reader_of s) (start lim) en plan) <==>
                   Some? (parse p3d s))
  = plan_wf ();
    scanner_correct o (Seq.length s) lim (reader_of s) en plan;
    max_string_length_small ();
    spec_iff s en
