module WireGen.Lib3d

(* Written once for every codec with optional fields or regions: how a
   casetype's value hands out its case, and the equations of the 3D
   combinators and operations WireGen.Lib does not state, as rewrite rules
   Z3 applies (SMTPat). Those that name EverParse3d.Prelude's parsers, which
   it keeps abstract, are in each reading. *)

open WireGen.Lib
module LP = LowParse.Spec.Base
module LPC = LowParse.Spec.Combinators
module EP = EverParse3d.Prelude
module U8 = FStar.UInt8
module U16 = FStar.UInt16
module U32 = FStar.UInt32
module UInt = FStar.UInt
module BF = LowParse.BitFields

(* The value of a casetype on [e]: [no] in its first case, [yes] of its
   value in the second. *)
let get_b (#e:bool) (#a:squash e -> Type) (#b:squash (not e) -> Type)
  (v:EP.t_ite e a b) (_:squash (not e)) : b () = v

let ite_case (#e:bool) (#a:squash e -> Type) (#b:squash (not e) -> Type) (#r:Type)
  (v:EP.t_ite e a b) (no:squash e -> r) (yes:(h:squash (not e)) -> b h -> r) : r =
  if e then no () else yes () (get_b v ())

(* 3D's bitfields of a big-endian word, most significant bit first. *)
let bitfield8_msb_eq (x:U8.t) (lo:U32.t{U32.v lo < 8}) (hi:U32.t{U32.v lo < U32.v hi /\ U32.v hi <= 8})
  : Lemma (U8.v (EverParse3d.Prelude.StaticHeader.get_bitfield8_msb_first x lo hi) ==
           (U8.v x / pow2 (8 - U32.v hi)) % pow2 (U32.v hi - U32.v lo))
          [SMTPat (EverParse3d.Prelude.StaticHeader.get_bitfield8_msb_first x lo hi)]
  = BF.get_bitfield_eq_2 #8 (U8.v x) (8 - U32.v hi) (8 - U32.v lo);
    BF.get_bitfield_eq #8 (U8.v x) (8 - U32.v hi) (8 - U32.v lo)

let bitfield16_msb_eq (x:U16.t) (lo:U32.t{U32.v lo < 16}) (hi:U32.t{U32.v lo < U32.v hi /\ U32.v hi <= 16})
  : Lemma (U16.v (EverParse3d.Prelude.StaticHeader.get_bitfield16_msb_first x lo hi) ==
           (U16.v x / pow2 (16 - U32.v hi)) % pow2 (U32.v hi - U32.v lo))
          [SMTPat (EverParse3d.Prelude.StaticHeader.get_bitfield16_msb_first x lo hi)]
  = BF.get_bitfield_eq #16 (U16.v x) (16 - U32.v hi) (16 - U32.v lo)

let bitfield32_msb_eq (x:U32.t) (lo:U32.t{U32.v lo < 32}) (hi:U32.t{U32.v lo < U32.v hi /\ U32.v hi <= 32})
  : Lemma (U32.v (EverParse3d.Prelude.StaticHeader.get_bitfield32_msb_first x lo hi) ==
           (U32.v x / pow2 (32 - U32.v hi)) % pow2 (U32.v hi - U32.v lo))
          [SMTPat (EverParse3d.Prelude.StaticHeader.get_bitfield32_msb_first x lo hi)]
  = BF.get_bitfield_eq #32 (U32.v x) (32 - U32.v hi) (32 - U32.v lo)

(* 3D's shifts and masks on unsigned words, as arithmetic. *)
let shift_left_eq (#n:pos) (a:UInt.uint_t n) (s:nat)
  : Lemma (requires s < n)
          (ensures UInt.shift_left #n a s == (a * pow2 s) % pow2 n)
          [SMTPat (UInt.shift_left #n a s)]
  = UInt.shift_left_value_lemma #n a s

let logand_one (#n:pos{n > 1}) (a:UInt.uint_t n)
  : Lemma (UInt.logand #n a 1 == a % 2) [SMTPat (UInt.logand #n a 1)]
  = UInt.logand_mask #n a 1
