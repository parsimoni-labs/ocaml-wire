module WireGen.Lib3d

(* Written once: the 3D combinators that wrap a LowParse parser at a kind
   of their own, stated at that kind. *)

module LP = LowParse.Spec.Base
module LPI = LowParse.Spec.Int
module P = EverParse3d.Prelude
module U32 = FStar.UInt32

(* A byte array of [n] bytes (UINT8 x[:byte-size n]): it holds exactly when
   there are [n] bytes, and takes them. *)
val nlist_u8 (n:U32.t) (c:option nat{P.memoizes_n_as_const c n}) (s:LP.bytes)
  : Lemma (let r = LP.parse (P.parse_nlist n c #_ #P.kind____UINT8 P.parse____UINT8) s in
           (Some? r <==> Seq.length s >= U32.v n) /\
           (match r with Some (_, k) -> k == U32.v n | None -> True))
          [SMTPat (LP.parse (P.parse_nlist n c #_ #P.kind____UINT8 P.parse____UINT8) s)]
