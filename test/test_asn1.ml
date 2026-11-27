(* Tests for asn1.ml: the DER codec, each X.690 rule docs/asn1.md lists with
   an accepted and a refused vector, views, nesting with binary codecs, and
   EverParse's vectors. *)

open Wire

(* [f ()] raises [Invalid_argument] with a message that mentions [msg]. *)
let rejects ~msg f =
  match f () with
  | _ -> Alcotest.failf "expected Invalid_argument mentioning %S" msg
  | exception Invalid_argument m ->
      if not (Re.execp (Re.compile (Re.str msg)) m) then
        Alcotest.failf "Invalid_argument %S does not mention %S" m msg

let builds f = ignore (f ())
let reloid = Alcotest.testable Asn1.Relative_oid.pp Asn1.Relative_oid.equal
let pair a b = Codec.[ (a $ fun (a, _) -> a); (b $ fun (_, b) -> b) ]
let mk a b = (a, b)

(* -- The running example of docs/asn1.md: RFC 5280 SubjectPublicKeyInfo -- *)

type params = Null | Curve of Asn1.Oid.t | Absent
type alg = { oid : Asn1.Oid.t; params : params }
type spki = { alg : alg; key : int * string }

let rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 1 ])
let ec = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 2; 1 ])
let ed25519 = Asn1.Oid.(base 1 3 <|| [ 101; 112 ])
let f_oid = Asn1.required "algorithm" Asn1.oid

let params_cases =
  Asn1.
    [
      case rsa null
        ~inject:(fun () -> Null)
        ~project:(function Null -> Some () | _ -> None);
      case ec oid
        ~inject:(fun c -> Curve c)
        ~project:(function Curve c -> Some c | _ -> None);
      absent ed25519 ~inject:Absent ~project:(function
        | Absent -> true
        | _ -> false);
    ]

let f_params = Asn1.defined_by "parameters" ~on:f_oid params_cases

let alg_id () =
  Asn1.sequence "AlgorithmIdentifier"
    (fun oid params -> { oid; params })
    Codec.[ (f_oid $ fun a -> a.oid); (f_params $ fun a -> a.params) ]

let spki () =
  let f_alg = Asn1.required "algorithm" (alg_id ()) in
  let f_key = Asn1.required "subjectPublicKey" Asn1.bit_string in
  Asn1.sequence "SubjectPublicKeyInfo"
    (fun alg key -> { alg; key })
    Codec.[ (f_alg $ fun s -> s.alg); (f_key $ fun s -> s.key) ]

(* -- The DER codec -- *)

let tuple3 a b c =
  Codec.
    [
      (a $ fun (a, _, _) -> a);
      (b $ fun (_, b, _) -> b);
      (c $ fun (_, _, c) -> c);
    ]

let tuple4 a b c d =
  Codec.
    [
      (a $ fun (a, _, _, _) -> a);
      (b $ fun (_, b, _, _) -> b);
      (c $ fun (_, _, c, _) -> c);
      (d $ fun (_, _, _, d) -> d);
    ]

let mk3 a b c = (a, b, c)
let mk4 a b c d = (a, b, c, d)

let of_hex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let to_hex s =
  String.concat ""
    (List.map
       (fun c -> Fmt.str "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)))

(* The verdict of [c] on the whole of [s]: [decode] of exactly [s] answers
   it, and [validate] and [View.check] answer the same on bytes [decode]
   consumes in full. Nothing raises. *)
let verdict (type a) (c : a Codec.t) s =
  let b = Bytes.of_string s in
  let decoded = Codec.decode ~consume:`All c b 0 in
  let module V = (val Codec.View.codec c) in
  let (Buf.Any vb) = Buf.v b in
  let checked = Codec.View.ok (Codec.View.check V.codec vb (Buf.length vb) 0) in
  let validated = Result.is_ok (Codec.validate c b 0) in
  let consumed =
    match Codec.size_at c b 0 with
    | Ok n -> n = String.length s
    | Error _ -> false
  in
  if consumed && (Result.is_ok decoded <> validated || validated <> checked)
  then Alcotest.failf "%s: decode, validate and View.check disagree" (to_hex s);
  decoded

(* [c] accepts [s], and writes the value it reads back as [s]. *)
let accepts ?(msg = "") c s =
  match verdict c s with
  | Error e -> Alcotest.failf "%s %s: refused (%a)" msg (to_hex s) Error.pp e
  | Ok v ->
      Alcotest.(check string)
        (msg ^ " re-encodes") (to_hex s)
        (to_hex (Codec.to_string c v))

let refuses ?(msg = "") c s =
  match verdict c s with
  | Ok _ -> Alcotest.failf "%s %s: accepted" msg (to_hex s)
  | Error _ -> ()

let writes ?(msg = "") c v hex =
  Alcotest.(check string) (msg ^ " writes") hex (to_hex (Codec.to_string c v))

let rejects_write ~msg c v = rejects ~msg (fun () -> Codec.to_string c v)

(* One row per DER rule of docs/asn1.md, "DER rules and who enforces them":
   an accepted and a refused vector, each with its X.690 clause. *)

let octets = Asn1.codec Asn1.octet_string

(* X.690 10.1: definite length, in the fewest octets. *)
let test_rule_length () =
  accepts octets ("\x04\x81\x80" ^ String.make 128 'a');
  accepts octets "\x04\x02\xab\xcd";
  refuses octets "\x04\x81\x02\xab\xcd";
  refuses octets "\x04\x82\x00\x02\xab\xcd";
  refuses octets "\x24\x80\x04\x01\xaa\x00\x00"

(* X.690 8.1.2.4.2 c: a tag number in the fewest octets, the long form only
   from 31. *)
let test_rule_tag () =
  let high = Asn1.codec (Asn1.implicit 100 Asn1.integer) in
  accepts high (of_hex "9f640105");
  refuses high (of_hex "9f80640105");
  let low = Asn1.codec (Asn1.implicit 5 Asn1.null) in
  accepts low (of_hex "8500");
  refuses low (of_hex "9f0500")

(* X.690 10.2: a string is primitive. *)
let test_rule_primitive_strings () =
  accepts octets (of_hex "0402abcd");
  refuses octets (of_hex "24060401ab0401cd");
  let utf8 = Asn1.codec Asn1.utf8_string in
  accepts utf8 (of_hex "0c026869");
  refuses utf8 (of_hex "2c060c01680c0169")

(* X.690 8.3.2: an INTEGER in the fewest octets. *)
let test_rule_integer () =
  let int = Asn1.codec Asn1.integer in
  accepts int (of_hex "02020080");
  accepts int (of_hex "0202ff7f");
  refuses int (of_hex "02020001");
  refuses int (of_hex "0202ff80");
  refuses int (of_hex "0200");
  rejects_write ~msg:"not minimal" int "\x00\x01"

(* X.690 11.1: TRUE is 0xFF. *)
let test_rule_boolean () =
  let bool = Asn1.codec Asn1.bool in
  accepts bool (of_hex "0101ff");
  accepts bool (of_hex "010100");
  refuses bool (of_hex "010101");
  refuses bool (of_hex "0102ffff")

(* X.690 11.2.1: at most 7 unused bits, each zero. *)
let test_rule_bit_string () =
  let bits = Asn1.codec Asn1.bit_string in
  accepts bits (of_hex "03020780");
  accepts bits (of_hex "030100");
  refuses bits (of_hex "03020781");
  refuses bits (of_hex "03020880");
  refuses bits (of_hex "030107");
  rejects_write ~msg:"are not zero" bits (1, "\x01")

(* X.690 11.5: a DEFAULT component equal to its default is absent. *)
let seq_ab =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.default "b" false Asn1.bool in
  Asn1.codec (Asn1.sequence "Seq" mk (pair a b))

let test_rule_default () =
  accepts seq_ab (of_hex "30060201010101ff");
  accepts seq_ab (of_hex "3003020101");
  refuses seq_ab (of_hex "3006020101010100");
  writes seq_ab ("\x01", false) "3003020101";
  writes seq_ab ("\x01", true) "30060201010101ff"

(* X.690 10.3: the components of a SET in the canonical order of their tags. *)
let set_ab =
  let a = Asn1.required "a" (Asn1.implicit 0 Asn1.integer) in
  let c = Asn1.required "c" Asn1.octet_string in
  Asn1.codec (Asn1.set "Set" mk (pair a c))

let test_rule_set () =
  accepts set_ab (of_hex "31070402abcd800105");
  refuses set_ab (of_hex "31078001050402abcd");
  refuses set_ab (of_hex "31040402abcd");
  writes set_ab ("\x05", "\xab\xcd") "31070402abcd800105"

(* X.690 11.6: the elements of a SET OF sorted by encoding. *)
let set_of_octets = Asn1.codec (Asn1.set_of Asn1.octet_string)

let test_rule_set_of () =
  accepts set_of_octets (of_hex "31060401aa0401bb");
  accepts set_of_octets (of_hex "310504000401aa");
  refuses set_of_octets (of_hex "31060401bb0401aa");
  refuses set_of_octets (of_hex "31050401aa0400");
  writes set_of_octets [ "\xbb"; "\xaa" ] "31060401aa0401bb"

(* X.690 8.19.2: each subidentifier in the fewest octets; everparse-asn1 reads
   each below 2^32 (spec/ASN1.Spec.Content.OIDU32.fst). *)
let test_rule_oid () =
  let oid_c = Asn1.codec Asn1.oid in
  accepts oid_c (of_hex "06032a8648");
  refuses oid_c (of_hex "06042a808648");
  refuses oid_c (of_hex "0600");
  refuses oid_c (of_hex "06022a86");
  (* 1.2.4294967295, 2^32 - 1, read as a UInt32 on every target (Q7). *)
  let max = of_hex "06062a8fffffff7f" in
  accepts oid_c max;
  (match Codec.decode oid_c (Bytes.of_string max) 0 with
  | Ok o ->
      Alcotest.(check string) "2^32 - 1" "1.2.4294967295" (Asn1.Oid.to_string o)
  | Error e -> Alcotest.failf "%a" Error.pp e);
  refuses oid_c (of_hex "06072a9080808000")

(* [c] writes [v] as bytes that read back as [v]. *)
let round_trip (type a) (testable : a Alcotest.testable) (c : a Codec.t) v =
  let s = Codec.to_string c v in
  match Codec.decode ~consume:`All c (Bytes.of_string s) 0 with
  | Ok v' -> Alcotest.check testable (to_hex s) v v'
  | Error e -> Alcotest.failf "%s: refused (%a)" (to_hex s) Error.pp e

(* X.690 8.20.2: one or more subidentifiers, each in the fewest octets and
   below 2^32 (spec/ASN1.Spec.Content.OIDU32.fst:59-61), under UNIVERSAL 13
   (spec/ASN1.Base.fst:418). *)
let test_rule_relative_oid () =
  let c = Asn1.codec Asn1.relative_oid in
  accepts c (of_hex "0d0100");
  accepts c (of_hex "0d03010203");
  accepts c (of_hex "0d058fffffff7f");
  refuses c (of_hex "0d00");
  refuses c (of_hex "0d028001");
  refuses c (of_hex "0d0181");
  refuses c (of_hex "0d069080808000");
  refuses c (of_hex "2d03010203");
  refuses c (of_hex "0603010203");
  (match Codec.decode c (Bytes.of_string (of_hex "0d058fffffff7f")) 0 with
  | Ok r ->
      Alcotest.(check string)
        "2^32 - 1" "4294967295"
        (Asn1.Relative_oid.to_string r)
  | Error e -> Alcotest.failf "%a" Error.pp e);
  writes c (Asn1.Relative_oid.v [ 8571; 3; 2 ]) "0d04c27b0302";
  List.iter (round_trip reloid c)
    (List.map
       (fun s -> Option.get (Asn1.Relative_oid.of_string s))
       [ "0"; "1.2.3"; "128"; "2147483648"; "4294967295" ])

let date_c = Asn1.codec Asn1.date
let tod_c = Asn1.codec Asn1.time_of_day
let date_time_c = Asn1.codec Asn1.date_time

(* X.690 8.26.2, X.680 38.4.1: DATE is YYYYMMDD, the year Basic, 1582 to 9999
   (spec/ASN1.Spec.Time.fst:191-200), under UNIVERSAL 31
   (spec/ASN1.Base.fst:431). *)
let test_rule_date () =
  accepts date_c "\x1f\x1f\x0815821015";
  accepts date_c "\x1f\x1f\x0820000229";
  accepts date_c "\x1f\x1f\x0899991231";
  refuses date_c "\x1f\x1f\x0815811231";
  refuses date_c "\x1f\x1f\x0819000229";
  refuses date_c "\x1f\x1f\x0820250431";
  refuses date_c "\x1f\x1f\x0820251301";
  refuses date_c "\x1f\x1f\x0820250100";
  refuses date_c "\x1f\x1f\x082025010a";
  refuses date_c "\x1f\x1f\x092025010100";
  refuses date_c "\x1f\x1f\x0a2025-01-01";
  refuses date_c "\x3f\x1f\x0820250101";
  refuses date_c "\x18\x0820250101";
  rejects_write ~msg:"is not a DER DATE" date_c "2025-01-01";
  List.iter
    (round_trip Alcotest.string date_c)
    [ "15821015"; "20240229"; "99991231" ]

(* X.690 8.26.2, X.680 38.4.2: TIME-OF-DAY is hhmmss, a local time whose leap
   second ends any minute (spec/ASN1.Spec.Time.fst:81-89,202-205), under
   UNIVERSAL 32 (spec/ASN1.Base.fst:432). *)
let test_rule_time_of_day () =
  accepts tod_c "\x1f\x20\x06000000";
  accepts tod_c "\x1f\x20\x06235959";
  accepts tod_c "\x1f\x20\x06120060";
  refuses tod_c "\x1f\x20\x06240000";
  refuses tod_c "\x1f\x20\x06126000";
  refuses tod_c "\x1f\x20\x06120061";
  refuses tod_c "\x1f\x20\x0612000Z";
  refuses tod_c "\x1f\x20\x0712000000";
  refuses tod_c "\x1f\x20\x00";
  rejects_write ~msg:"is not a DER TIME-OF-DAY" tod_c "12:00:00";
  List.iter (round_trip Alcotest.string tod_c) [ "000000"; "120060"; "235959" ]

(* X.690 8.26.2, X.680 38.4.3: DATE-TIME is YYYYMMDDhhmmss, the year Basic and
   the time local (spec/ASN1.Spec.Time.fst:207-210), under UNIVERSAL 33
   (spec/ASN1.Base.fst:433). *)
let test_rule_date_time () =
  accepts date_time_c "\x1f\x21\x0e20250101120000";
  accepts date_time_c "\x1f\x21\x0e15821015000000";
  accepts date_time_c "\x1f\x21\x0e20240229101060";
  refuses date_time_c "\x1f\x21\x0e15811231235959";
  refuses date_time_c "\x1f\x21\x0e20250229120000";
  refuses date_time_c "\x1f\x21\x0e20250101240000";
  refuses date_time_c "\x1f\x21\x0e20250101120061";
  refuses date_time_c "\x1f\x21\x0f20250101120000Z";
  refuses date_time_c "\x1f\x21\x0d2025010112000";
  rejects_write ~msg:"is not a DER DATE-TIME" date_time_c "20250101T120000";
  List.iter
    (round_trip Alcotest.string date_time_c)
    [ "20250101120000"; "15821015000000"; "20161231235960" ]

let oid_iri_c = Asn1.codec Asn1.oid_iri
let rel_iri_c = Asn1.codec Asn1.relative_oid_iri

(* The universal [number] of the high-tag form, primitive, with contents [s]
   of under 128 octets. *)
let high_tlv number s =
  Fmt.str "\x1f%c%c%s" (Char.chr number) (Char.chr (String.length s)) s

(* X.690 8.21, X.680 34.3, X.660 7.5: OID-IRI is UTF-8, "/" then non-empty
   labels separated by "/", of RFC 3987 iunreserved or non-ASCII characters,
   an integer label without a leading zero (spec/ASN1.Spec.IRI.fst:19-58),
   under UNIVERSAL 35 (spec/ASN1.Base.fst:434). *)
let test_rule_oid_iri () =
  List.iter
    (fun s -> accepts ~msg:s oid_iri_c (high_tlv 0x23 s))
    [ "/ISO"; "/0"; "/1/2"; "/ISO/Registration-Authority/a.b_c~d"; "/\xc3\xa9" ];
  List.iter
    (fun s -> refuses ~msg:s oid_iri_c (high_tlv 0x23 s))
    [
      "";
      "/";
      "ISO";
      "/01";
      "/a//b";
      "/a/";
      "/a b";
      "/a?b";
      "/\xc3";
      "/\xc0\xaf";
    ];
  refuses oid_iri_c (high_tlv 0x24 "/ISO");
  rejects_write ~msg:"is not a DER OID-IRI" oid_iri_c "ISO";
  List.iter
    (round_trip Alcotest.string oid_iri_c)
    [ "/ISO/a"; "/2/999"; "/\xc3\xa9t\xc3\xa9" ]

(* X.690 8.22, X.680 35.3, X.660 7.5: RELATIVE-OID-IRI is the labels of an
   OID-IRI without the leading "/" (spec/ASN1.Spec.IRI.fst:50-52), under
   UNIVERSAL 36 (spec/ASN1.Base.fst:435). *)
let test_rule_relative_oid_iri () =
  List.iter
    (fun s -> accepts ~msg:s rel_iri_c (high_tlv 0x24 s))
    [ "a"; "0"; "a/b"; "10/ISO"; "\xc3\x9c" ];
  List.iter
    (fun s -> refuses ~msg:s rel_iri_c (high_tlv 0x24 s))
    [ ""; "/a"; "a/"; "a//b"; "007"; "a/b c"; "\xff" ];
  refuses rel_iri_c (high_tlv 0x23 "a");
  rejects_write ~msg:"is not a DER RELATIVE-OID-IRI" rel_iri_c "/a";
  List.iter (round_trip Alcotest.string rel_iri_c) [ "a/b"; "0"; "\xc3\x9c/1" ]

(* -- Other constructs -- *)

let test_strings () =
  let c t = Asn1.codec t in
  accepts (c Asn1.printable_string) "\x13\x03a'?";
  refuses (c Asn1.printable_string) "\x13\x01*";
  accepts (c Asn1.ia5_string) "\x16\x01\x7f";
  refuses (c Asn1.ia5_string) "\x16\x01\x80";
  accepts (c Asn1.numeric_string) "\x12\x02 1";
  refuses (c Asn1.numeric_string) "\x12\x01a";
  accepts (c Asn1.visible_string) "\x1a\x01~";
  refuses (c Asn1.visible_string) "\x1a\x01\x7f";
  accepts (c Asn1.utf8_string) "\x0c\x03\xe2\x82\xac";
  refuses (c Asn1.utf8_string) "\x0c\x02\xc0\xaf";
  accepts (c Asn1.bmp_string) "\x1e\x02\x00a";
  refuses (c Asn1.bmp_string) "\x1e\x02\xd8\x00";
  accepts (c Asn1.universal_string) "\x1c\x04\x00\x10\xff\xff";
  refuses (c Asn1.universal_string) "\x1c\x04\x00\x11\x00\x00";
  accepts (c Asn1.teletex_string) "\x14\x01\xff";
  accepts (c Asn1.utc_time) "\x17\x0d991231235960Z";
  refuses (c Asn1.utc_time) "\x17\x0d990230000000Z";
  accepts (c Asn1.generalized_time) "\x18\x1120000229000000.5Z";
  refuses (c Asn1.generalized_time) "\x18\x1120000229000000.0Z";
  rejects_write ~msg:"outside the alphabet" (c Asn1.printable_string) "*"

let test_size () =
  let c = Asn1.codec (Asn1.size ~min:2 ~max:3 Asn1.utf8_string) in
  accepts c "\x0c\x04\xe2\x82\xaca";
  refuses c "\x0c\x03\xe2\x82\xac";
  rejects_write ~msg:"outside SIZE" c "a"

let test_bounded_and_enumerated () =
  let byte = Asn1.codec (Asn1.bounded_integer ~min:0L ~max:255L) in
  accepts byte (of_hex "020200ff");
  refuses byte (of_hex "02020100");
  refuses byte (of_hex "0209010000000000000000");
  rejects_write ~msg:"outside INTEGER" byte 256L;
  let enum = Asn1.codec (Asn1.enumerated [ ("a", 0); ("c", 5) ]) in
  accepts enum (of_hex "0a0105");
  refuses enum (of_hex "0a0101");
  rejects_write ~msg:"not a value" enum 1

type ch = I of string | B of bool | O of string

let choice_c =
  Asn1.choice
    [
      Asn1.alternative "i" Asn1.integer
        ~inject:(fun s -> I s)
        ~project:(function I s -> Some s | _ -> None);
      Asn1.alternative "b" Asn1.bool
        ~inject:(fun b -> B b)
        ~project:(function B b -> Some b | _ -> None);
      Asn1.alternative "o"
        (Asn1.implicit 0 Asn1.octet_string)
        ~inject:(fun s -> O s)
        ~project:(function O s -> Some s | _ -> None);
    ]

let test_choice_and_optional () =
  let c = Asn1.codec choice_c in
  accepts c (of_hex "020105");
  accepts c (of_hex "8002abcd");
  refuses c (of_hex "0400");
  refuses c (of_hex "a0020400");
  let opt =
    let a = Asn1.optional "a" Asn1.bool in
    let b = Asn1.optional "b" (Asn1.explicit 1 Asn1.integer) in
    Asn1.codec (Asn1.sequence "Opt" mk (pair a b))
  in
  accepts opt (of_hex "3000");
  accepts opt (of_hex "3005a103020105");
  accepts opt (of_hex "30080101ffa103020105");
  refuses opt (of_hex "30050201050101");
  refuses opt (of_hex "3008a1030201050101ff")

let test_keep_bytes () =
  let a = Asn1.required "a" (Asn1.keep_bytes Asn1.integer) in
  let b = Asn1.required "b" Asn1.bool in
  let c = Asn1.codec (Asn1.sequence "K" mk (pair a b)) in
  match Codec.decode c (Bytes.of_string (of_hex "30060201050101ff")) 0 with
  | Ok ((v, bytes), true) ->
      Alcotest.(check string) "value" "\x05" v;
      Alcotest.(check string) "bytes" "020105" (to_hex bytes);
      writes c (("\x05", "ignored"), true) "30060201050101ff"
  | Ok _ -> Alcotest.fail "wrong value"
  | Error e -> Alcotest.failf "%a" Error.pp e

(* The running example: AlgorithmIdentifier keyed by its OID. *)
let test_defined_by () =
  let c = Asn1.codec (alg_id ()) in
  let rsa_null = of_hex "300d06092a864886f70d0101010500" in
  accepts c rsa_null;
  let ed = of_hex "300506032b6570" in
  accepts c ed;
  (match Codec.decode c (Bytes.of_string ed) 0 with
  | Ok { params = Absent; _ } -> ()
  | _ -> Alcotest.fail "id-Ed25519 has no parameters");
  (* rsaEncryption with no NULL, Ed25519 with one, an unknown key. *)
  refuses c (of_hex "300b06092a864886f70d010101");
  refuses c (of_hex "300706032b65700500");
  refuses c (of_hex "300606022a030500");
  rejects_write ~msg:"not the case of its key" c { oid = rsa; params = Absent };
  rejects_write ~msg:"no case" c
    { oid = Asn1.Oid.(base 1 2 <| 3); params = Null }

(* A key with no case falls back to the declared type, OPTIONAL here, as
   everparse-asn1's AlgId vector schema does (DiffAny.asn). *)
type any_params = Null_123 | Other of Asn1.Oid.t * string option

let alg_123 = Asn1.Oid.(base 1 2 <| 3)

let any_alg_id =
  let algorithm = Asn1.required "algorithm" Asn1.oid in
  let parameters =
    Asn1.defined_by "parameters" ~on:algorithm
      Asn1.
        [
          case alg_123 null
            ~inject:(fun () -> Null_123)
            ~project:(function Null_123 -> Some () | Other _ -> None);
          fallback ~optional:true any
            ~inject:(fun k p -> Other (k, p))
            ~project:(function Other (k, p) -> Some (k, p) | Null_123 -> None);
        ]
  in
  Asn1.sequence "AlgId" mk (pair algorithm parameters)

let test_fallback () =
  let c = Asn1.codec any_alg_id in
  accepts c (of_hex "300606022a030500");
  accepts c (of_hex "300706022a04020105");
  accepts c (of_hex "300406022a04");
  refuses c (of_hex "300406022a03");
  refuses c (of_hex "300706022a03020105");
  rejects_write ~msg:"carries another key" c
    (Asn1.Oid.(base 1 2 <| 4), Other (alg_123, None))

(* -- The running example on real certificates -- *)

(* ocaml-x509's tests/testcertificates/cacert.pem (RSA) and fido.pem (EC on
   prime256v1), base64-decoded. *)
let cacert_der =
  of_hex
    (String.concat ""
       [
         "3082026b308201d4a003020102020900cedcd419548ce92c300d06092a864886";
         "f70d01010505003045310b30090603550406130241553113301106035504080c";
         "0a536f6d652d53746174653121301f060355040a0c18496e7465726e65742057";
         "69646769747320507479204c7464301e170d3134303630363132353632355a17";
         "0d3234303630333132353632355a3045310b3009060355040613024155311330";
         "1106035504080c0a536f6d652d53746174653121301f060355040a0c18496e74";
         "65726e6574205769646769747320507479204c746430819f300d06092a864886";
         "f70d010101050003818d0030818902818100e761b407ee916618eeaa4e3e2ca4";
         "0599ca376dfe5609ffcc72e2808f94a6a6c4e911fdc8cdef50a0b9ea83d394f2";
         "1ddde3d224f6aafc735382303c894fa2a2bdfa0cc5c9b9f4bbcae33f8528b91a";
         "9c192f70f993d83d6f10af05a07f8a4600b81695b4a6f159184ba67540042636";
         "b600bdeea9ba23645efdcd9b06b556223b010203010001a3633061301d060355";
         "1d0e041604149f73106142114d82caf0ff37e917c320c8db1e4a301f0603551d";
         "230418301680149f73106142114d82caf0ff37e917c320c8db1e4a3012060355";
         "1d130101ff040830060101ff020164300b0603551d0f040403020106300d0609";
         "2a864886f70d0101050500038181005f4a9c403af60f0eaa26431565952676b9";
         "c66749e99981bb98f1f820de39875212361a6c2c0c58d247cf17f81cc06991cf";
         "389f527019e21b83b9d52b39656f560ed68b9384d4917683c9c553357fcf40ac";
         "d6d5047121605a73fc6bc08d7d7397ab509277d21cb5fa0b27b0bf7bdbceab0f";
         "a7e4c4fce5309b1ebfc86f6798cda4";
       ])

let fido_der =
  of_hex
    (String.concat ""
       [
         "308201673082010da003020102020900d79549bd1a671751300a06082a8648ce";
         "3d04030230173115301306035504030c0c4654204649444f2030323030302017";
         "0d3139303232363030303030305a180f32303334303232353233353935395a30";
         "21311f301d06035504030c164654204649444f20503230343730303133343134";
         "31323059301306072a8648ce3d020106082a8648ce3d03010703420004d8465a";
         "43a2aca53fa6335eb58453e2bdde5e8ebbee53704dedeb9aa550dd56b793cc0a";
         "9dcf1f99ad7b138fa3c49d45f18b6fc60d274b93b41f9d0b20e7e9f800a33630";
         "34301d0603551d0e04160414525ea96c47b9a47933a09b4871c398df6407aaa4";
         "3013060b2b0601040182e51c020101040403020470300a06082a8648ce3d0403";
         "020348003045022100a7d0ff5e731f86a001509a09e345c233632897a45f4ff0";
         "a5a4f7f8e0abf51a7102204217fdbceeb8610a744d97ae4e80665058f1b17f8e";
         "c3ffb32623879ae178322b";
       ])

let decode_exn c s =
  match Codec.decode ~consume:`All c (Bytes.of_string s) 0 with
  | Ok v -> v
  | Error e -> Alcotest.failf "decode: %a" Error.pp e

(* A certificate reads, and writes back as the same bytes. Its
   SubjectPublicKeyInfo, kept as bytes, is what the docs/asn1.md codec writes
   for the value read. *)
let test_certificate name der () =
  let cert = decode_exn Rfc5280.certificate_codec der in
  Alcotest.(check string)
    (name ^ " re-encodes") (to_hex der)
    (to_hex (Codec.to_string Rfc5280.certificate_codec cert));
  let tbs, tbs_bytes = cert.Rfc5280.tbs_certificate in
  Alcotest.(check string)
    (name ^ " tbs bytes in place")
    (to_hex tbs_bytes)
    (to_hex (String.sub der 4 (String.length tbs_bytes)));
  let spki_value, spki_bytes = tbs.Rfc5280.subject_public_key_info in
  let doc_spki = Asn1.codec (spki ()) in
  let value = decode_exn doc_spki spki_bytes in
  Alcotest.(check string)
    (name ^ " SPKI") (to_hex spki_bytes)
    (to_hex (Codec.to_string doc_spki value));
  Alcotest.(check string)
    (name ^ " SPKI, both grammars")
    (to_hex spki_bytes)
    (to_hex (Codec.to_string Rfc5280.spki_codec spki_value));
  let again = decode_exn doc_spki (Codec.to_string doc_spki value) in
  Alcotest.(check bool) (name ^ " SPKI round trip") true (again = value)

let test_spki_params () =
  let params der =
    let cert = decode_exn Rfc5280.certificate_codec der in
    let tbs = fst cert.Rfc5280.tbs_certificate in
    (fst tbs.Rfc5280.subject_public_key_info).Rfc5280.alg.Rfc5280.params
  in
  (match params cacert_der with
  | Rfc5280.Null -> ()
  | _ -> Alcotest.fail "rsaEncryption parameters are NULL");
  match params fido_der with
  | Rfc5280.Curve c ->
      Alcotest.(check string)
        "prime256v1" "1.2.840.10045.3.1.7" (Asn1.Oid.to_string c)
  | _ -> Alcotest.fail "id-ecPublicKey parameters are a curve"

let test_ed25519_round_trip () =
  let c = Asn1.codec (spki ()) in
  let v =
    {
      alg = { oid = ed25519; params = Absent };
      key = (0, String.make 32 '\x42');
    }
  in
  let s = Codec.to_string c v in
  Alcotest.(check bool) "round trip" true (decode_exn c s = v)

(* Every cut of a certificate is an [Error]: nothing raises. *)
let test_truncations () =
  let b = Bytes.of_string cacert_der in
  for n = 0 to Bytes.length b - 1 do
    let cut = Bytes.sub b 0 n in
    match Codec.decode Rfc5280.certificate_codec cut 0 with
    | Ok _ -> Alcotest.failf "a certificate cut at %d decodes" n
    | Error _ -> ()
  done;
  for i = 0 to Bytes.length b - 1 do
    let flipped = Bytes.copy b in
    Bytes.set flipped i (Char.chr (Char.code (Bytes.get b i) lxor 0xFF));
    (match Codec.decode Rfc5280.certificate_codec flipped 0 with
    | Ok _ | Error _ -> ());
    match Codec.validate Rfc5280.certificate_codec flipped 0 with
    | Ok () | Error _ -> ()
  done

(* -- fold -- *)

let ints_record =
  let a = Asn1.required "a" Asn1.bool in
  let l = Asn1.required "l" (Asn1.sequence_of Asn1.integer) in
  let s = Asn1.required "s" (Asn1.implicit 0 (Asn1.set_of Asn1.integer)) in
  let m =
    Asn1.required "m"
      (Asn1.map ~decode:(fun s -> [ s ]) ~encode:List.hd Asn1.octet_string)
  in
  let a = Codec.(a $ fun (a, _, _, _) -> a) in
  let l = Codec.(l $ fun (_, l, _, _) -> l) in
  let s = Codec.(s $ fun (_, _, s, _) -> s) in
  let m = Codec.(m $ fun (_, _, _, m) -> m) in
  (Asn1.codec (Asn1.sequence "Ints" mk4 Codec.[ a; l; s; m ]), a, l, s, m)

let ints = (true, [ "\x01"; "\x02"; "\x03" ], [ "\x05"; "\x04" ], [ "m" ])

(* A component read in place, through Codec.get and View.checked_field. *)
let test_checked_field () =
  let c, a, l, s, m = ints_record in
  let bytes = Bytes.of_string (Codec.to_string c ints) in
  Alcotest.(check (result bool reject))
    "get a" (Ok true) (Codec.get c a bytes 0);
  (match Codec.get c s bytes 0 with
  | Ok v -> Alcotest.(check (list string)) "get s" [ "\x04"; "\x05" ] v
  | Error e -> Alcotest.failf "get s: %a" Error.pp e);
  (match Codec.get c m bytes 0 with
  | Ok v -> Alcotest.(check (list string)) "get m" [ "m" ] v
  | Error e -> Alcotest.failf "get m: %a" Error.pp e);
  let module V = (val Codec.View.codec c) in
  let read_l = Codec.View.checked_field V.codec l in
  let (Buf.Any b) = Buf.v bytes in
  let v = Codec.View.get (Codec.View.check V.codec b (Buf.length b) 0) in
  (match Codec.View.read read_l b v with
  | Ok l -> Alcotest.(check int) "checked_field l" 3 (List.length l)
  | Error e -> Alcotest.failf "checked_field l: %a" Error.pp e);
  (* Bytes written after the check: the read answers their error. *)
  Bytes.set bytes 4 '\x02';
  Alcotest.(check bool)
    "written after" true
    (Result.is_error (Codec.View.read read_l b v));
  rejects ~msg:"sets no field in place" (fun () -> Codec.set c a bytes 0 false);
  let other = Asn1.required "other" Asn1.bool in
  rejects ~msg:"does not belong to codec" (fun () ->
      Codec.get c Codec.(other $ fun _ -> true))

let test_fold () =
  let c, _, l, s, m = ints_record in
  let module V = (val Codec.View.codec c) in
  let (Buf.Any b) = Buf.v (Bytes.of_string (Codec.to_string c ints)) in
  let k = Codec.View.check V.codec b (Buf.length b) 0 in
  Alcotest.(check bool) "checked" true (Codec.View.ok k);
  let v = Codec.View.get k in
  let sum =
    Asn1.fold V.codec l b v ~init:0 ~f:(fun acc x -> acc + Char.code x.[0])
  in
  Alcotest.(check int) "SEQUENCE OF" 6 sum;
  let order =
    Asn1.fold V.codec s b v ~init:[] ~f:(fun acc x -> Char.code x.[0] :: acc)
  in
  Alcotest.(check (list int)) "SET OF in DER order" [ 5; 4 ] order;
  rejects ~msg:"is not a SEQUENCE OF or SET OF" (fun () -> Asn1.fold V.codec m)

(* -- Nesting -- *)

(* DER in binary: the DER validator runs on exactly the region and consumes
   all of it. *)
let framed_spki () =
  let len = Field.v "len" uint16be in
  let spki =
    Field.v "spki" (nested ~size:(Field.ref len) (codec (Asn1.codec (spki ()))))
  in
  Codec.v "Framed" mk Codec.[ len $ fst; spki $ snd ]

let test_der_in_binary () =
  let c = framed_spki () in
  let spki_bytes =
    Codec.to_string
      (Asn1.codec (spki ()))
      { alg = { oid = ed25519; params = Absent }; key = (0, "k") }
  in
  let n = String.length spki_bytes in
  let frame len body =
    let b = Bytes.create (2 + String.length body) in
    Bytes.set_uint16_be b 0 len;
    Bytes.blit_string body 0 b 2 (String.length body);
    b
  in
  (match Codec.decode c (frame n spki_bytes) 0 with
  | Ok (_, v) -> Alcotest.(check string) "key" "k" (snd v.key)
  | Error e -> Alcotest.failf "framed SPKI: %a" Error.pp e);
  Alcotest.(check bool)
    "validate" true
    (Result.is_ok (Codec.validate c (frame n spki_bytes) 0));
  (* A bad inner region: BIT STRING with 8 unused bits. *)
  let bad = Bytes.of_string spki_bytes in
  Bytes.set bad (n - 2) '\x08';
  Alcotest.(check bool)
    "bad inner refused" true
    (Result.is_error (Codec.validate c (frame n (Bytes.to_string bad)) 0));
  Alcotest.(check bool)
    "bad inner decode refused" true
    (Result.is_error (Codec.decode c (frame n (Bytes.to_string bad)) 0));
  (* A region one byte longer than the DER value. *)
  Alcotest.(check bool)
    "region not consumed" true
    (Result.is_error (Codec.validate c (frame (n + 1) (spki_bytes ^ "\x00")) 0));
  (* A region that cuts the DER value. *)
  Alcotest.(check bool)
    "region cut" true
    (Result.is_error (Codec.validate c (frame (n - 1) spki_bytes) 0))

let test_der_outside_nested () =
  let der = codec (Asn1.codec Asn1.bool) in
  rejects ~msg:"holds a DER codec outside nested ~size" (fun () ->
      Codec.v "Bare" Fun.id Codec.[ Field.v "b" der $ Fun.id ]);
  rejects ~msg:"holds a DER codec outside nested ~size" (fun () ->
      Codec.v "AtMost" Fun.id
        Codec.[ Field.v "b" (nested_at_most ~size:(int 3) der) $ Fun.id ]);
  rejects ~msg:"holds a DER codec outside nested ~size" (fun () ->
      Codec.v "Opt" Fun.id
        Codec.[ Field.optional "b" ~present:(bool true) der $ Fun.id ]);
  builds (fun () ->
      Codec.v "Nested" Fun.id
        Codec.[ Field.v "b" (nested ~size:(int 3) der) $ Fun.id ])

(* Binary in DER: an OCTET STRING whose contents the binary codec checks, in
   full. *)
let hdr =
  let version =
    Field.v "version" ~self_constraint:Expr.(fun v -> v = int 1) uint8
  in
  let length = Field.v "length" uint16be in
  Codec.v "Hdr" mk Codec.[ version $ fst; length $ snd ]

let tagged_hdr () =
  let label = Asn1.required "name" Asn1.utf8_string in
  let h = Asn1.required "hdr" (Asn1.containing (codec hdr)) in
  let n = Asn1.optional "n" (Asn1.containing uint16be) in
  Asn1.codec (Asn1.sequence "Tagged" mk3 (tuple3 label h n))

let test_binary_in_der () =
  let c = tagged_hdr () in
  let v = ("x", (UInt8.v 1, UInt16.v 7), Some (UInt16.v 9)) in
  let s = Codec.to_string c v in
  Alcotest.(check string) "bytes" "300c0c0178040301000704020009" (to_hex s);
  accepts c s;
  (* version 2 breaks the binary codec's constraint *)
  refuses c (of_hex "300c0c0178040302000704020009");
  (* a byte after the binary record inside the OCTET STRING *)
  refuses c (of_hex "300d0c017804040100070004020009");
  (* a cut binary record *)
  refuses c (of_hex "300b0c01780402010004020009");
  rejects_write ~msg:"rejected by its own constraints" c
    ("x", (UInt8.v 2, UInt16.v 7), None)

(* -- EverParse's vectors -- *)

type runner = Run : 'a Codec.t -> runner

let diff_seq =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.default "b" false Asn1.bool in
  let c = Asn1.optional "c" (Asn1.implicit 0 Asn1.octet_string) in
  let d = Asn1.optional "d" (Asn1.explicit 1 Asn1.integer) in
  Asn1.sequence "Seq" mk4 (tuple4 a b c d)

let int_seq = Asn1.sequence_of Asn1.integer

let nested_seq =
  let s = Asn1.required "s" diff_seq in
  let l = Asn1.required "l" int_seq in
  let ch = Asn1.optional "ch" choice_c in
  Asn1.sequence "Nested" mk3 (tuple3 s l ch)

let diff_set =
  let a = Asn1.required "a" (Asn1.implicit 0 Asn1.integer) in
  let b = Asn1.optional "b" (Asn1.implicit 1 Asn1.bool) in
  let c = Asn1.required "c" Asn1.octet_string in
  Asn1.set "Set" mk3 (tuple3 a b c)

let smoke =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.optional "b" Asn1.bool in
  let c = Asn1.required "c" Asn1.octet_string in
  Asn1.sequence "Smoke" mk3 (tuple3 a b c)

(* EXTERNAL, EMBEDDED PDV and CHARACTER STRING as spec/ASN1.UniversalTypes.fst
   writes them. *)
type encoding =
  | Single of string
  | Octet_aligned of string
  | Arbitrary of (int * string)

let external_ =
  let encoding =
    Asn1.choice
      [
        Asn1.alternative "single-ASN1-type" (Asn1.explicit 0 Asn1.any)
          ~inject:(fun s -> Single s)
          ~project:(function Single s -> Some s | _ -> None);
        Asn1.alternative "octet-aligned"
          (Asn1.implicit 1 Asn1.octet_string)
          ~inject:(fun s -> Octet_aligned s)
          ~project:(function Octet_aligned s -> Some s | _ -> None);
        Asn1.alternative "arbitrary"
          (Asn1.implicit 2 Asn1.bit_string)
          ~inject:(fun b -> Arbitrary b)
          ~project:(function Arbitrary b -> Some b | _ -> None);
      ]
  in
  let direct = Asn1.optional "direct-reference" Asn1.oid in
  let indirect = Asn1.optional "indirect-reference" Asn1.integer in
  let descriptor =
    Asn1.optional "data-value-descriptor" Asn1.object_descriptor
  in
  let encoding = Asn1.required "encoding" encoding in
  Asn1.implicit ~cls:`Universal 8
    (Asn1.sequence "EXTERNAL" mk4 (tuple4 direct indirect descriptor encoding))

type identification =
  | Syntaxes of (Asn1.Oid.t * Asn1.Oid.t)
  | Syntax of Asn1.Oid.t
  | Context_id of string
  | Negotiation of (string * Asn1.Oid.t)
  | Transfer of Asn1.Oid.t
  | Fixed

let identification =
  let two name na ta nb tb =
    let a = Asn1.required na ta and b = Asn1.required nb tb in
    Asn1.sequence name mk (pair a b)
  in
  let alt name t inject project = Asn1.alternative name t ~inject ~project in
  Asn1.choice
    [
      alt "syntaxes"
        (Asn1.implicit 0
           (two "syntaxes" "abstract" (Asn1.implicit 0 Asn1.oid) "transfer"
              (Asn1.implicit 1 Asn1.oid)))
        (fun x -> Syntaxes x)
        (function Syntaxes x -> Some x | _ -> None);
      alt "syntax" (Asn1.implicit 1 Asn1.oid)
        (fun x -> Syntax x)
        (function Syntax x -> Some x | _ -> None);
      alt "presentation-context-id"
        (Asn1.implicit 2 Asn1.integer)
        (fun x -> Context_id x)
        (function Context_id x -> Some x | _ -> None);
      alt "context-negotiation"
        (Asn1.implicit 3
           (two "context-negotiation" "presentation-context-id"
              (Asn1.implicit 0 Asn1.integer)
              "transfer-syntax" (Asn1.implicit 1 Asn1.oid)))
        (fun x -> Negotiation x)
        (function Negotiation x -> Some x | _ -> None);
      alt "transfer-syntax" (Asn1.implicit 4 Asn1.oid)
        (fun x -> Transfer x)
        (function Transfer x -> Some x | _ -> None);
      alt "fixed"
        (Asn1.implicit 5 Asn1.null)
        (fun () -> Fixed)
        (function Fixed -> Some () | _ -> None);
    ]

let presentation n name value =
  let id = Asn1.required "identification" (Asn1.explicit 0 identification) in
  let value = Asn1.required value (Asn1.implicit 2 Asn1.octet_string) in
  Asn1.implicit ~cls:`Universal n (Asn1.sequence name mk (pair id value))

let runners =
  let c t = Some (Run (Asn1.codec t)) in
  [
    ("DiffPrim.Bool", c Asn1.bool);
    ("DiffPrim.Int", c Asn1.integer);
    ("DiffPrim.Enum", c (Asn1.enumerated [ ("a", 0); ("b", 1); ("c", 5) ]));
    ("DiffPrim.Bits", c Asn1.bit_string);
    ("DiffPrim.Octets", c Asn1.octet_string);
    ("DiffPrim.Null", c Asn1.null);
    ("DiffPrim.Oid", c Asn1.oid);
    ("DiffDesc.Desc", c Asn1.object_descriptor);
    ("DiffConstraints.Sized", c (Asn1.size ~min:2 ~max:3 Asn1.octet_string));
    ("DiffConstraints.Byte", c (Asn1.bounded_integer ~min:0L ~max:255L));
    ( "DiffConstraints.NonEmptySeqOf",
      c (Asn1.sequence_of ~non_empty:true Asn1.integer) );
    ( "DiffConstraints.NonEmptySetOf",
      c (Asn1.set_of ~non_empty:true Asn1.integer) );
    ("DiffConstructed.Seq", c diff_seq);
    ("DiffConstructed.IntSeq", c int_seq);
    ("DiffConstructed.OctetsSet", c (Asn1.set_of Asn1.octet_string));
    ("DiffConstructed.Choice", c choice_c);
    ("DiffConstructed.App", c (Asn1.implicit ~cls:`Application 5 Asn1.integer));
    ("DiffConstructed.PrivExp", c (Asn1.explicit ~cls:`Private 3 Asn1.bool));
    ("DiffConstructed.HighCtx", c (Asn1.implicit 100 Asn1.integer));
    ( "DiffConstructed.HighApp",
      c (Asn1.explicit ~cls:`Application 1000 Asn1.octet_string) );
    ("DiffConstructed.Nested", c nested_seq);
    ("DiffSet.Set", c diff_set);
    ("DiffSmoke.Smoke", c smoke);
    ("DiffAny.AnyT", c Asn1.any);
    ("DiffAny.AlgId", c any_alg_id);
    ("DiffExternal.Ext", c external_);
    ("DiffPdv.Pdv", c (presentation 11 "EMBEDDED PDV" "data-value"));
    ( "DiffCharStr.CharStr",
      c (presentation 29 "CHARACTER STRING" "string-value") );
    ("DiffStrings.Utf8", c Asn1.utf8_string);
    ("DiffStrings.Numeric", c Asn1.numeric_string);
    ("DiffStrings.Printable", c Asn1.printable_string);
    ("DiffStrings.Ia5", c Asn1.ia5_string);
    ("DiffStrings.Visible", c Asn1.visible_string);
    ("DiffStrings.Bmp", c Asn1.bmp_string);
    ("DiffStrings.Universal", c Asn1.universal_string);
    ("DiffStrings.Teletex", c Asn1.teletex_string);
    ("DiffVideotex.Videotex", c Asn1.videotex_string);
    ("DiffStrings2.Graphic", c Asn1.graphic_string);
    ("DiffStrings2.General", c Asn1.general_string);
    ("DiffTime.Utc", c Asn1.utc_time);
    ("DiffTime.Gen", c Asn1.generalized_time);
    ("DiffRelOid.RelOid", c Asn1.relative_oid);
    ("DiffIRI.Iri", c Asn1.oid_iri);
    ("DiffIRI.RelIri", c Asn1.relative_oid_iri);
    ("DiffTime2.Date", c Asn1.date);
    ("DiffTime2.Tod", c Asn1.time_of_day);
    ("DiffTime2.DateTime", c Asn1.date_time);
  ]

let test_vectors () =
  let run = ref 0 and skipped = ref 0 in
  List.iter
    (fun (v : Asn1_vectors.t) ->
      match List.assoc_opt v.typ runners with
      | None -> Alcotest.failf "%s: no runner for %s" v.id v.typ
      | Some None -> incr skipped
      | Some (Some (Run c)) ->
          incr run;
          let msg = Fmt.str "%a" Asn1_vectors.pp v in
          if v.accept then accepts ~msg c (of_hex v.hex)
          else refuses ~msg c (of_hex v.hex))
    Asn1_vectors.all;
  Alcotest.(check int) "vectors run" 325 !run;
  Alcotest.(check int) "vectors skipped" 0 !skipped

let suite =
  ( "asn1",
    [
      Alcotest.test_case "der: definite minimal length (X.690 10.1)" `Quick
        test_rule_length;
      Alcotest.test_case "der: minimal tag number (X.690 8.1.2)" `Quick
        test_rule_tag;
      Alcotest.test_case "der: primitive strings (X.690 10.2)" `Quick
        test_rule_primitive_strings;
      Alcotest.test_case "der: minimal INTEGER (X.690 8.3.2)" `Quick
        test_rule_integer;
      Alcotest.test_case "der: BOOLEAN 00/FF (X.690 11.1)" `Quick
        test_rule_boolean;
      Alcotest.test_case "der: BIT STRING padding (X.690 11.2.1)" `Quick
        test_rule_bit_string;
      Alcotest.test_case "der: DEFAULT omitted (X.690 11.5)" `Quick
        test_rule_default;
      Alcotest.test_case "der: SET in tag order (X.690 10.3)" `Quick
        test_rule_set;
      Alcotest.test_case "der: SET OF sorted (X.690 11.6)" `Quick
        test_rule_set_of;
      Alcotest.test_case "der: minimal OID subidentifiers (X.690 8.19)" `Quick
        test_rule_oid;
      Alcotest.test_case "der: RELATIVE-OID (X.690 8.20)" `Quick
        test_rule_relative_oid;
      Alcotest.test_case "der: DATE (X.690 8.26)" `Quick test_rule_date;
      Alcotest.test_case "der: TIME-OF-DAY (X.690 8.26)" `Quick
        test_rule_time_of_day;
      Alcotest.test_case "der: DATE-TIME (X.690 8.26)" `Quick
        test_rule_date_time;
      Alcotest.test_case "der: OID-IRI (X.690 8.21)" `Quick test_rule_oid_iri;
      Alcotest.test_case "der: RELATIVE-OID-IRI (X.690 8.22)" `Quick
        test_rule_relative_oid_iri;
      Alcotest.test_case "der: character strings and times" `Quick test_strings;
      Alcotest.test_case "der: SIZE" `Quick test_size;
      Alcotest.test_case "der: bounded INTEGER and ENUMERATED" `Quick
        test_bounded_and_enumerated;
      Alcotest.test_case "der: CHOICE and OPTIONAL" `Quick
        test_choice_and_optional;
      Alcotest.test_case "der: keep_bytes" `Quick test_keep_bytes;
      Alcotest.test_case "der: defined_by" `Quick test_defined_by;
      Alcotest.test_case "der: defined_by fallback" `Quick test_fallback;
      Alcotest.test_case "spki: cacert.pem" `Quick
        (test_certificate "cacert" cacert_der);
      Alcotest.test_case "spki: fido.pem" `Quick
        (test_certificate "fido" fido_der);
      Alcotest.test_case "spki: parameters by key" `Quick test_spki_params;
      Alcotest.test_case "spki: Ed25519 round trip" `Quick
        test_ed25519_round_trip;
      Alcotest.test_case "der: no exception on cut or flipped bytes" `Quick
        test_truncations;
      Alcotest.test_case "fold" `Quick test_fold;
      Alcotest.test_case "checked_field" `Quick test_checked_field;
      Alcotest.test_case "nesting: DER in binary" `Quick test_der_in_binary;
      Alcotest.test_case "nesting: DER outside nested" `Quick
        test_der_outside_nested;
      Alcotest.test_case "nesting: binary in DER" `Quick test_binary_in_der;
      Alcotest.test_case "everparse vectors" `Quick test_vectors;
    ] )
