(** Fuzz tests for the DER codec of {!Wire.Asn1}, on the SubjectPublicKeyInfo
    and Certificate types of RFC 5280 and on the primitives with a contents rule
    of their own. *)

open Wire
open Alcobar

(* Arbitrary bytes never make decode, validate or a view check raise, and the
   three agree on bytes decode reads in full; bytes decode accepts are the
   bytes its value writes, DER having one encoding per value. *)
let check_bytes (type a) name (c : a Codec.t) s =
  let b = Bytes.of_string s in
  let validated = Result.is_ok (Codec.validate c b 0) in
  let module V = (val Codec.View.codec c) in
  let (Buf.Any vb) = Buf.v b in
  let checked = Codec.View.ok (Codec.View.check V.codec vb (Buf.length vb) 0) in
  if validated <> checked then
    failf "%s: validate %b, View.check %b" name validated checked;
  match Codec.decode ~consume:`All c b 0 with
  | Error _ -> ()
  | Ok v ->
      if not validated then
        failf "%s: decode accepts what validate refuses" name;
      let s' = Codec.to_string c v in
      if not (String.equal s s') then failf "%s: %S writes back as %S" name s s'

let test_spki s = check_bytes "spki" Rfc5280.spki_codec s
let test_certificate s = check_bytes "certificate" Rfc5280.certificate_codec s

(* The primitives whose contents rule is more than an alphabet, under their
   own identifier so that arbitrary contents reach the rule. *)
let primitives =
  let c name t = check_bytes name (Asn1.codec t) in
  [|
    ("\x0d", c "relative_oid" Asn1.relative_oid);
    ("\x1f\x1f", c "date" Asn1.date);
    ("\x1f\x20", c "time_of_day" Asn1.time_of_day);
    ("\x1f\x21", c "date_time" Asn1.date_time);
    ("\x1f\x23", c "oid_iri" Asn1.oid_iri);
    ("\x1f\x24", c "relative_oid_iri" Asn1.relative_oid_iri);
  |]

let test_primitive i s =
  let s = if String.length s > 127 then String.sub s 0 127 else s in
  let id, check = primitives.(i) in
  check s;
  check (id ^ String.make 1 (Char.chr (String.length s)) ^ s)

(* Certificates built from generated parts, so that mutations start from
   bytes the codec accepts. *)
let common_name = Asn1.Oid.(base 2 5 <|| [ 4; 3 ])
let sha256_rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 11 ])
let p256 = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 3; 1; 7 ])
let key_usage = Asn1.Oid.(base 2 5 <|| [ 29; 15 ])

let letters s =
  let s = if String.length s > 100 then String.sub s 0 100 else s in
  String.map (fun c -> Char.chr (97 + (Char.code c mod 26))) s

let utf8 s = "\x0c" ^ String.make 1 (Char.chr (String.length s)) ^ s

let certificate serial cn key ext critical kind : Rfc5280.certificate =
  let name =
    [ [ { Rfc5280.attr = common_name; value = utf8 (letters cn) } ] ]
  in
  let alg = { Rfc5280.algorithm = sha256_rsa; parameters = Some "\x05\x00" } in
  let spki_alg : Rfc5280.alg =
    match kind mod 3 with
    | 0 -> { oid = Rfc5280.rsa; params = Null }
    | 1 -> { oid = Rfc5280.ec; params = Curve p256 }
    | _ -> { oid = Rfc5280.ed25519; params = Absent }
  in
  let tbs : Rfc5280.tbs =
    {
      version = "\x02";
      serial = "\x01" ^ serial;
      signature = alg;
      issuer = name;
      validity =
        { from = Utc "250101000000Z"; until = Generalized "20500101000000Z" };
      subject = name;
      subject_public_key_info = ({ alg = spki_alg; key = (0, key) }, "");
      issuer_unique_id = None;
      subject_unique_id = None;
      extensions = Some [ { extn_id = key_usage; critical; extn_value = ext } ];
    }
  in
  {
    tbs_certificate = (tbs, "");
    signature_algorithm = alg;
    signature_value = (0, key);
  }

(* A built certificate writes, reads back, and writes the same bytes. *)
let test_built serial cn key ext critical kind =
  let c = Rfc5280.certificate_codec in
  let s = Codec.to_string c (certificate serial cn key ext critical kind) in
  match Codec.decode ~consume:`All c (Bytes.of_string s) 0 with
  | Error e -> failf "a written certificate is refused: %a" Error.pp e
  | Ok v ->
      if not (String.equal s (Codec.to_string c v)) then
        fail "a certificate does not write back as its bytes"

(* One byte of a written certificate replaced, or the bytes cut short. *)
let test_mutated serial cn key ext critical kind at byte =
  let s =
    Codec.to_string Rfc5280.certificate_codec
      (certificate serial cn key ext critical kind)
  in
  let b = Bytes.of_string s in
  let at = at mod Bytes.length b in
  Bytes.set b at (Char.chr byte);
  check_bytes "mutated certificate" Rfc5280.certificate_codec
    (Bytes.to_string b);
  check_bytes "cut certificate" Rfc5280.certificate_codec (String.sub s 0 at)

let suite =
  ( "asn1",
    [
      test_case "spki: arbitrary bytes" [ bytes ] test_spki;
      test_case "primitives: arbitrary contents"
        [ range (Array.length primitives); bytes ]
        test_primitive;
      test_case "certificate: arbitrary bytes" [ bytes ] test_certificate;
      test_case "certificate: built"
        [ bytes; bytes; bytes; bytes; bool; uint8 ]
        test_built;
      test_case "certificate: mutated"
        [ bytes; bytes; bytes; bytes; bool; uint8; range 65536; uint8 ]
        test_mutated;
    ] )
