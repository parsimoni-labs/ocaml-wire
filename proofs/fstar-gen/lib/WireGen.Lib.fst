module WireGen.Lib

(* Written once for every codec: the byte reads a generated scanner is
   written with, and the LowParse equations of the combinators 3d.exe builds
   its parsers from, as rewrite rules Z3 applies (SMTPat). A codec's proof
   then only unfolds its 3D parser to those combinators. *)

module LP = LowParse.Spec.Base
module LPC = LowParse.Spec.Combinators
module LPI = LowParse.Spec.Int
module E = FStar.Endianness
module Seq = FStar.Seq
module U8 = FStar.UInt8
module U16 = FStar.UInt16
module U32 = FStar.UInt32

type bytes = LP.bytes

(* Byte [i] of [s], 0 past its end, as Plan.Word reads a buffer. *)
let byte (s:bytes) : Plan.Word.bytes =
  fun i -> if i < Seq.length s then U8.v (Seq.index s i) else 0

(* The big-endian value of the [w] bytes at [o]: Plan.Word.be, so the word
   lemmas of Plan.Word apply to it. *)
let be (s:bytes) (o:nat) (w:nat) : v:nat{v < pow2 (8 * w)} =
  Plan.Word.be_bound (byte s) o w;
  Plan.Word.be (byte s) o w

let rec be_to_n_is_be (s:bytes) (o:nat) (w:nat)
  : Lemma (requires o + w <= Seq.length s)
          (ensures E.be_to_n (Seq.slice s o (o + w)) == be s o w)
          (decreases w)
  = E.reveal_be_to_n (Seq.slice s o (o + w));
    if w = 0 then ()
    else begin
      be_to_n_is_be s o (w - 1);
      Seq.slice_slice s o (o + w) 0 (w - 1);
      assert_norm (pow2 8 == 256)
    end

let rec be_slice_gen (s:bytes) (a:nat) (o:nat) (w:nat)
  : Lemma (requires a <= Seq.length s)
          (ensures be (Seq.slice s a (Seq.length s)) o w == be s (a + o) w)
          (decreases w)
  = if w = 0 then () else be_slice_gen s a o (w - 1)

let be_slice (s:bytes) (a:nat) (o:nat) (w:nat)
  : Lemma (requires a <= Seq.length s)
          (ensures be (Seq.slice s a (Seq.length s)) o w == be s (a + o) w)
          [SMTPat (be (Seq.slice s a (Seq.length s)) o w)]
  = be_slice_gen s a o w

(* A window [off, off + len) of a buffer reads the buffer's bytes. *)
let rec be_window (s:bytes) (off:nat) (len:nat) (o:nat) (w:nat)
  : Lemma (requires off + len <= Seq.length s /\ o + w <= len)
          (ensures be (Seq.slice s off (off + len)) o w == be s (off + o) w)
          (decreases w)
  = if w = 0 then () else be_window s off len o (w - 1)

let be_window_pat (s:bytes) (off:nat) (len:nat) (o:nat) (w:nat)
  : Lemma (requires off + len <= Seq.length s /\ o + w <= len)
          (ensures be (Seq.slice s off (off + len)) o w == be s (off + o) w)
          [SMTPat (be (Seq.slice s off (off + len)) o w)]
  = be_window s off len o w

let slice_len (s:bytes) (a:nat)
  : Lemma (requires a <= Seq.length s)
          (ensures Seq.length (Seq.slice s a (Seq.length s)) == Seq.length s - a)
          [SMTPat (Seq.slice s a (Seq.length s))]
  = ()

(* The equations are stated on the bare parser applied to the bytes, with
   LP.parse unfolded: LP.parse carries the parser's value type, which in a
   3d.exe denotation is [as_type] of its typ, so a pattern on LP.parse would
   not match it; the application has no type to match. *)

let u8_parse (s:bytes)
  : Lemma (LP.parse LPI.parse_u8 s ==
           (if Seq.length s >= 1 then Some (U8.uint_to_t (be s 0 1), 1) else None))
  = LP.parser_kind_prop_equiv LPI.parse_u8_kind LPI.parse_u8;
    if Seq.length s >= 1 then begin LPI.parse_u8_spec s; be_to_n_is_be s 0 1 end

let u8_eq (s:bytes)
  : Lemma ((LPI.parse_u8 <: LP.bare_parser U8.t) s ==
           (if Seq.length s >= 1 then Some (U8.uint_to_t (be s 0 1), 1) else None))
          [SMTPat ((LPI.parse_u8 <: LP.bare_parser U8.t) s)]
  = u8_parse s;
    assert (LP.parse LPI.parse_u8 s == (LPI.parse_u8 <: LP.bare_parser U8.t) s)

let u16_parse (s:bytes)
  : Lemma (LP.parse LPI.parse_u16 s ==
           (if Seq.length s >= 2 then Some (U16.uint_to_t (be s 0 2), 2) else None))
  = LP.parser_kind_prop_equiv LPI.parse_u16_kind LPI.parse_u16;
    if Seq.length s >= 2 then begin LPI.parse_u16_spec s; be_to_n_is_be s 0 2 end

let u16_eq (s:bytes)
  : Lemma ((LPI.parse_u16 <: LP.bare_parser U16.t) s ==
           (if Seq.length s >= 2 then Some (U16.uint_to_t (be s 0 2), 2) else None))
          [SMTPat ((LPI.parse_u16 <: LP.bare_parser U16.t) s)]
  = u16_parse s;
    assert (LP.parse LPI.parse_u16 s == (LPI.parse_u16 <: LP.bare_parser U16.t) s)

let u32_parse (s:bytes)
  : Lemma (LP.parse LPI.parse_u32 s ==
           (if Seq.length s >= 4 then Some (U32.uint_to_t (be s 0 4), 4) else None))
  = LP.parser_kind_prop_equiv LPI.parse_u32_kind LPI.parse_u32;
    if Seq.length s >= 4 then begin LPI.parse_u32_spec s; be_to_n_is_be s 0 4 end

let u32_eq (s:bytes)
  : Lemma ((LPI.parse_u32 <: LP.bare_parser U32.t) s ==
           (if Seq.length s >= 4 then Some (U32.uint_to_t (be s 0 4), 4) else None))
          [SMTPat ((LPI.parse_u32 <: LP.bare_parser U32.t) s)]
  = u32_parse s;
    assert (LP.parse LPI.parse_u32 s == (LPI.parse_u32 <: LP.bare_parser U32.t) s)

let nondep_then_eq #k1 #t1 (p1:LP.parser k1 t1) #k2 #t2 (p2:LP.parser k2 t2) (s:bytes)
  : Lemma ((LPC.nondep_then p1 p2 <: LP.bare_parser (t1 & t2)) s ==
           (match (p1 <: LP.bare_parser t1) s with
            | Some (x1, c1) ->
              (match (p2 <: LP.bare_parser t2) (Seq.slice s c1 (Seq.length s)) with
               | Some (x2, c2) -> Some ((x1, x2), c1 + c2)
               | None -> None)
            | None -> None))
          [SMTPat ((LPC.nondep_then p1 p2 <: LP.bare_parser (t1 & t2)) s)]
  = LPC.nondep_then_eq p1 p2 s

let dtuple2_eq #k1 #t1 (p1:LP.parser k1 t1) #k2 (#t2:t1 -> Type) (p2:(x:t1 -> LP.parser k2 (t2 x))) (s:bytes)
  : Lemma ((LPC.parse_dtuple2 p1 p2 <: LP.bare_parser (dtuple2 t1 t2)) s ==
           (match (p1 <: LP.bare_parser t1) s with
            | Some (x1, c1) ->
              (match (p2 x1 <: LP.bare_parser (t2 x1)) (Seq.slice s c1 (Seq.length s)) with
               | Some (x2, c2) -> Some ((| x1, x2 |), c1 + c2)
               | None -> None)
            | None -> None))
          [SMTPat ((LPC.parse_dtuple2 p1 p2 <: LP.bare_parser (dtuple2 t1 t2)) s)]
  = LPC.parse_dtuple2_eq p1 p2 s

let filter_eq #k #t (p:LP.parser k t) (f:t -> GTot bool) (s:bytes)
  : Lemma ((LPC.parse_filter p f <: LP.bare_parser (LPC.parse_filter_refine f)) s ==
           (match (p <: LP.bare_parser t) s with
            | None -> None
            | Some (x, c) -> if f x then Some (x, c) else None))
          [SMTPat ((LPC.parse_filter p f <: LP.bare_parser (LPC.parse_filter_refine f)) s)]
  = LPC.parse_filter_eq p f s

(* 3d.exe types a refined field as [EverParse3d.Prelude.refine t f], which
   is [x:t{f x}], where LowParse types its filter as
   [parse_filter_refine f], [x:t{f x == true}]. F* identifies the two; Z3
   does not, and needs the filter at the 3D type to instantiate the
   equation of the combinator around it. *)
let filter_typing #k #t (p:LP.parser k t) (f:t -> bool)
  : Lemma (has_type (LPC.parse_filter p f) (LP.parser (LPC.parse_filter_kind k) (EverParse3d.Prelude.refine t f)))
          [SMTPat (LPC.parse_filter p f)]
  = let q : LP.parser (LPC.parse_filter_kind k) (EverParse3d.Prelude.refine t f) = LPC.parse_filter p f in
    assert (has_type q (LP.parser (LPC.parse_filter_kind k) (EverParse3d.Prelude.refine t f)))

(* 3d.exe's bitfields, least significant bit first. *)
let bitfield8_eq (x:U8.t) (lo:U32.t{U32.v lo < 8}) (hi:U32.t{U32.v lo < U32.v hi /\ U32.v hi <= 8})
  : Lemma (U8.v (EverParse3d.Prelude.StaticHeader.get_bitfield8 x lo hi) ==
           (U8.v x / pow2 (U32.v lo)) % pow2 (U32.v hi - U32.v lo))
          [SMTPat (EverParse3d.Prelude.StaticHeader.get_bitfield8 x lo hi)]
  = LowParse.BitFields.get_bitfield_eq_2 #8 (U8.v x) (U32.v lo) (U32.v hi);
    LowParse.BitFields.get_bitfield_eq #8 (U8.v x) (U32.v lo) (U32.v hi)

let bitfield16_eq (x:U16.t) (lo:U32.t{U32.v lo < 16}) (hi:U32.t{U32.v lo < U32.v hi /\ U32.v hi <= 16})
  : Lemma (U16.v (EverParse3d.Prelude.StaticHeader.get_bitfield16 x lo hi) ==
           (U16.v x / pow2 (U32.v lo)) % pow2 (U32.v hi - U32.v lo))
          [SMTPat (EverParse3d.Prelude.StaticHeader.get_bitfield16 x lo hi)]
  = LowParse.BitFields.get_bitfield_eq #16 (U16.v x) (U32.v lo) (U32.v hi)

(* A byte array of [n] bytes, as 3d.exe denotes it (parse_nlist n of
   UINT8, a list parsed in exactly [n] bytes): it holds exactly when there
   are [n] bytes, and takes them. *)
let fldata_u8_list (n:nat) (s:bytes)
  : Lemma (let r = LowParse.Spec.FLData.parse_fldata (LowParse.Spec.List.parse_list LPI.parse_u8) n s in
           (Some? r <==> Seq.length s >= n) /\
           (match r with Some (_, c) -> c == n | None -> True))
          [SMTPat (LowParse.Spec.FLData.parse_fldata (LowParse.Spec.List.parse_list LPI.parse_u8) n s)]
  = let p = LowParse.Spec.List.parse_list LPI.parse_u8 in
    if Seq.length s >= n then begin
      let x = Seq.slice s 0 n in
      LP.parser_kind_prop_equiv LPI.parse_u8_kind LPI.parse_u8;
      LowParse.Spec.List.parse_list_total_constant_size LPI.parse_u8 n x;
      LP.parser_kind_prop_equiv (LowParse.Spec.List.parse_list_kind false) p;
      LP.parser_kind_prop_equiv (LowParse.Spec.List.parse_list_kind true) p
    end
