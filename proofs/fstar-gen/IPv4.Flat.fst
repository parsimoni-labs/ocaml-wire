module IPv4.Flat

friend EverParse3d.Prelude
friend IPv4

open WireGen.Lib
open WireGen.Lib3d
module P = WireGen.OCaml
module LP = LowParse.Spec.Base
module U8 = FStar.UInt8
module U16 = FStar.UInt16
module U32 = FStar.UInt32

let spec (s:bytes) : GTot (option value) =
  match LP.parse (IPv4.parser____WireIPv4 ()) s with
  | None -> None
  | Some (v, _) -> Some v

(* Bitfields of a UINT8 are numbered from the least significant bit, those
   of a UINT16BE from the most (3D's rule, which wire's projection
   follows). *)
let answer #r invalid valid o =
  match o with
  | None -> invalid ()
  | Some v ->
    let (| bf0, (bf1, (| total_length, (identification, (bf2, (ttl, (protocol, (checksum, (src, dst)))))) |)) |) = v in
    valid (P.bits (P.of_u8 bf0) 4 4) (P.bits (P.of_u8 bf0) 0 4)
      (P.bits (P.of_u8 bf1) 2 6) (P.bits (P.of_u8 bf1) 0 2)
      (P.of_u16 total_length) (P.of_u16 identification)
      (P.bits (P.of_u16 bf2) 13 3) (P.bits (P.of_u16 bf2) 0 13)
      (P.of_u8 ttl) (P.of_u8 protocol) (P.of_u16 checksum) (P.of_u32 src) (P.of_u32 dst)

#push-options "--z3rlimit 400 --fuel 1 --ifuel 1"
let flat_is_3d #r invalid valid s =
  assert (answer invalid valid (spec s) == flat invalid valid s)
    by (WireGen.Tac.unfold_flat [`%spec; `%answer; `%flat;
          `%IPv4.parser____WireIPv4; `%IPv4.def'____WireIPv4; `%IPv4.def____WireIPv4])
#pop-options
