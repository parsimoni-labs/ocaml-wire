(* Tests for the projection of ASN.1 types to everparse-asn1 modules: the
   printed module of each type of Asn1_modules, its roots, the 3D schemas of
   the binary codecs under a containing, and what the projection refuses. The
   modules are those `make asn1` hands to everparse-asn1. *)

open Wire

let project m t = Asn1.project m (Asn1.codec t)

(* [f ()] raises [Invalid_argument] with a message that mentions [msg]. *)
let rejects ~msg f =
  match f () with
  | _ -> Alcotest.failf "expected Invalid_argument mentioning %S" msg
  | exception Invalid_argument m ->
      if not (Re.execp (Re.compile (Re.str msg)) m) then
        Alcotest.failf "Invalid_argument %S does not mention %S" m msg

let printed s = Fmt.str "%a" Asn1.pp s
let contained (Asn1_modules.Packed t) = Asn1.contained (Asn1.codec t)
let projected name (Asn1_modules.Packed t) = project name t

(* -- The printed modules -- *)

let spki_asn =
  {|SPKI DEFINITIONS EXPLICIT TAGS ::= BEGIN

--#DEFINED-BY AlgorithmIdentifier.parameters {
--#  oid-1-2-840-113549-1-1-1 : NULL,
--#  oid-1-2-840-10045-2-1 : OBJECT IDENTIFIER,
--#  oid-1-3-101-112 : ABSENT
--#} FALLBACK NONE

oid-1-2-840-113549-1-1-1 OBJECT IDENTIFIER ::= { 1 2 840 113549 1 1 1 }
oid-1-2-840-10045-2-1 OBJECT IDENTIFIER ::= { 1 2 840 10045 2 1 }
oid-1-3-101-112 OBJECT IDENTIFIER ::= { 1 3 101 112 }

AlgorithmIdentifier ::= SEQUENCE {
  algorithm OBJECT IDENTIFIER,
  parameters ANY DEFINED BY algorithm OPTIONAL
}

SubjectPublicKeyInfo ::= SEQUENCE {
  algorithm AlgorithmIdentifier,
  subjectPublicKey BIT STRING
}

END
|}

let constructs_asn =
  {|Constructs DEFINITIONS EXPLICIT TAGS ::= BEGIN

--#DEFINED-BY Keyed.value {
--#  oid-1-3-6-1-4-1-1 : INTEGER,
--#  oid-1-3-6-1-4-1-2 : [0] EXPLICIT UTF8String,
--#  oid-1-3-6-1-4-1-3 : ABSENT
--#} FALLBACK [1] EXPLICIT ANY OPTIONAL
--#DEFINED-BY Flagged.value {
--#  TRUE : INTEGER,
--#  FALSE : ABSENT
--#} FALLBACK NONE
--#DEFINED-BY Versioned.body {
--#  1 : UTF8String,
--#  -1 : OCTET STRING
--#} FALLBACK NONE
--#KEEP-BYTES Kept-elems-elt
--#KEEP-BYTES Kept.whole

oid-1-3-6-1-4-1-1 OBJECT IDENTIFIER ::= { 1 3 6 1 4 1 1 }
oid-1-3-6-1-4-1-2 OBJECT IDENTIFIER ::= { 1 3 6 1 4 1 2 }
oid-1-3-6-1-4-1-3 OBJECT IDENTIFIER ::= { 1 3 6 1 4 1 3 }

Prims ::= SEQUENCE {
  flag BOOLEAN,
  big INTEGER,
  small INTEGER (-128..127),
  colour ENUMERATED { red(0), green(1), blue(2) },
  nothing NULL,
  id OBJECT IDENTIFIER,
  bits BIT STRING,
  bytes OCTET STRING,
  mapped UTF8String
}

Strings ::= SEQUENCE {
  utf8 UTF8String,
  printable PrintableString,
  ia5 IA5String,
  numeric NumericString,
  visible VisibleString,
  teletex TeletexString,
  bmp BMPString,
  universal UniversalString,
  utc UTCTime,
  generalized GeneralizedTime,
  sized UTF8String (SIZE (1..16)),
  octets OCTET STRING (SIZE (4..4)),
  videotex VideotexString,
  graphic GraphicString,
  general GeneralString,
  descriptor ObjectDescriptor
}

Tags ::= SEQUENCE {
  application [APPLICATION 3] IMPLICIT OCTET STRING,
  private [PRIVATE 4] EXPLICIT INTEGER,
  contextImplicit [5] IMPLICIT INTEGER,
  contextExplicit [6] EXPLICIT BOOLEAN,
  universal [UNIVERSAL 40] EXPLICIT NULL
}

Optionals ::= SEQUENCE {
  opt [0] EXPLICIT BOOLEAN OPTIONAL,
  flag [1] IMPLICIT BOOLEAN DEFAULT FALSE,
  level [2] IMPLICIT INTEGER (0..7) DEFAULT 3,
  label [3] IMPLICIT UTF8String DEFAULT "a ""b""",
  version [4] EXPLICIT INTEGER DEFAULT 0,
  mask [5] IMPLICIT BIT STRING DEFAULT '1010'B,
  tail INTEGER
}

Keyed ::= SEQUENCE {
  kind OBJECT IDENTIFIER,
  value ANY DEFINED BY kind OPTIONAL,
  trailer INTEGER
}

Flagged ::= SEQUENCE {
  on BOOLEAN,
  value ANY DEFINED BY on OPTIONAL
}

Versioned ::= SEQUENCE {
  v INTEGER,
  body ANY DEFINED BY v
}

Inner ::= SEQUENCE {
  x INTEGER
}

Pair ::= SET {
  a [0] IMPLICIT BOOLEAN,
  b [1] IMPLICIT INTEGER
}

Lists ::= SEQUENCE {
  names SEQUENCE OF UTF8String,
  ids SET SIZE (1..MAX) OF OBJECT IDENTIFIER,
  inners SEQUENCE SIZE (1..MAX) OF Inner,
  pick CHOICE {
    num INTEGER,
    name CHOICE {
      utf8 UTF8String,
      ia5 IA5String
    }
  },
  pair Pair
}

Kept-elems-elt ::= Inner

Kept ::= SEQUENCE {
  whole Inner,
  elems SEQUENCE OF Kept-elems-elt,
  payload OCTET STRING,
  word OCTET STRING,
  rest ANY
}

Constructs ::= SEQUENCE {
  prims Prims,
  strings Strings,
  tags Tags,
  optionals Optionals,
  keyed Keyed,
  flagged Flagged,
  versioned Versioned,
  lists Lists,
  kept Kept
}

END
|}

let names_asn =
  {|Names DEFINITIONS EXPLICIT TAGS ::= BEGIN

Names ::= SEQUENCE SIZE (1..MAX) OF UTF8String

END
|}

let kept_root_asn =
  {|KeptRoot DEFINITIONS EXPLICIT TAGS ::= BEGIN

--#KEEP-BYTES KeptRoot

Inner ::= SEQUENCE {
  x INTEGER
}

KeptRoot ::= Inner

END
|}

let clash_asn =
  {|Clash DEFINITIONS EXPLICIT TAGS ::= BEGIN

Item ::= SEQUENCE {
  a INTEGER
}

Item-2 ::= SEQUENCE {
  a BOOLEAN
}

END-2 ::= SEQUENCE {
  e INTEGER
}

Clash ::= SEQUENCE {
  first Item,
  second-Item Item-2,
  again Item,
  weird-name NULL,
  weird-name-2 NULL,
  x-9colour ENUMERATED { dark-red(0), dark-red-2(1), x(2) },
  end END-2
}

END
|}

let x509lite_asn =
  {|X509Lite DEFINITIONS EXPLICIT TAGS ::= BEGIN

--#DEFINED-BY AlgId.parameters {
--#  oid-1-2-840-113549-1-1-11 : NULL,
--#  oid-1-2-840-10045-4-3-2 : ABSENT
--#} FALLBACK ANY OPTIONAL
--#DEFINED-BY Extension.extnValue { } FALLBACK OCTET STRING
--#KEEP-BYTES Cert.tbs

oid-1-2-840-113549-1-1-11 OBJECT IDENTIFIER ::= { 1 2 840 113549 1 1 11 }
oid-1-2-840-10045-4-3-2 OBJECT IDENTIFIER ::= { 1 2 840 10045 4 3 2 }

AlgId ::= SEQUENCE {
  algorithm OBJECT IDENTIFIER,
  parameters ANY DEFINED BY algorithm OPTIONAL
}

Extension ::= SEQUENCE {
  extnID OBJECT IDENTIFIER,
  critical BOOLEAN DEFAULT FALSE,
  extnValue ANY DEFINED BY extnID
}

TBS ::= SEQUENCE {
  version [0] EXPLICIT INTEGER DEFAULT 0,
  serial INTEGER,
  signature AlgId,
  subject SEQUENCE SIZE (1..MAX) OF UTF8String (SIZE (1..8)),
  extensions [3] EXPLICIT SEQUENCE SIZE (1..MAX) OF Extension OPTIONAL
}

Cert ::= SEQUENCE {
  tbs TBS,
  sigAlg AlgId,
  sig BIT STRING
}

END
|}

let times_asn =
  {|Times DEFINITIONS EXPLICIT TAGS ::= BEGIN

Times ::= SEQUENCE {
  day DATE DEFAULT "2024-02-29",
  clock TIME-OF-DAY DEFAULT "23:59:60",
  stamp DATE-TIME DEFAULT "1582-10-15T00:00:00",
  iri OID-IRI DEFAULT "/ISO/Registration_Authority/19785",
  relIri RELATIVE-OID-IRI DEFAULT "a-b/1",
  rel RELATIVE-OID
}

END
|}

(* A binary codec holding DER projects to the module of the DER types it
   holds; a DER value under a containing prints as such, its type in the same
   module. *)
let framed_asn = "Framed" ^ String.sub spki_asn 4 (String.length spki_asn - 4)

let wrapped_asn =
  {|Wrapped DEFINITIONS EXPLICIT TAGS ::= BEGIN

Inner ::= SEQUENCE {
  n INTEGER
}

Wrapped ::= SEQUENCE {
  body OCTET STRING (CONTAINING Inner),
  tail OCTET STRING
}

END
|}

let test_framed () =
  let s = Asn1.project "Framed" Asn1_modules.framed in
  Alcotest.(check string) "text" framed_asn (printed s);
  Alcotest.(check (list string))
    "roots" [ "SubjectPublicKeyInfo" ] (Asn1.roots s);
  Alcotest.(check (list string))
    "no binary under a containing" []
    (List.map Everparse.name (Asn1.contained Asn1_modules.framed))

(* The binary half of the same codec projects to 3D, the DER value a byte
   region of its declared size: 3D checks the framing, everparse-asn1 the DER
   held in it. *)
let framed_3d =
  {|entrypoint
typedef struct WireFramed
{
   UINT16BE len;
   UINT8 key[:byte-size len];
} Framed;

|}

let framed_3d_ffi =
  {|extern typedef struct WireWireCtx WireCtx

extern unit FramedSetU16BE(mutable WireCtx *ctx, UINT32 idx, UINT16BE v)

extern unit FramedSetBytes(mutable WireCtx *ctx, UINT32 idx, UINT32 v)

entrypoint
typedef struct WireFramed(mutable WireCtx *ctx)
{
   UINT16BE len {:on-success FramedSetU16BE(ctx, (UINT32) 0, len); return true; };
   UINT8 key[:byte-size len] {:on-success FramedSetBytes(ctx, (UINT32) 1, (UINT32) 2); return true; };
} Framed;

|}

let to_3d ~mode c =
  Private.Everparse.Raw.to_3d
    (Private.Everparse.module_ (Everparse.project ~mode c))

let test_framed_3d () =
  Alcotest.(check string)
    "standalone" framed_3d
    (to_3d ~mode:`Standalone Asn1_modules.framed);
  Alcotest.(check string)
    "ffi" framed_3d_ffi
    (to_3d ~mode:`Ffi Asn1_modules.framed)

(* A [where true] between the region and the DER value constrains nothing, so
   the region projects as it does without one. *)
let test_framed_where_3d () =
  let (Asn1_modules.Packed spki) = Asn1_modules.spki in
  let len = Field.v "len" uint16be in
  let key =
    Field.v "key"
      (nested ~size:(Field.ref len)
         (where Expr.(bool true) (codec (Asn1.codec spki))))
  in
  let c =
    Codec.v "Framed" (fun len key -> (len, key)) Codec.[ len $ fst; key $ snd ]
  in
  Alcotest.(check string) "standalone" framed_3d (to_3d ~mode:`Standalone c);
  Alcotest.(check string) "ffi" framed_3d_ffi (to_3d ~mode:`Ffi c)

(* A DER value under an optional is the same byte region, behind the gate
   the optional's condition sets; its ASN.1 half is the module of the DER it
   holds. *)
let opt_framed_3d =
  {|typedef struct WireOptRegion_selen_SubjectPublicKeyInfo(UINT32 len)
{
   UINT8 v[:byte-size len];
} OptRegion_selen_SubjectPublicKeyInfo;

casetype _Opt_selen_SubjectPublicKeyInfo(UINT8 present,
UINT32 len) {
               switch (present) {
               case 0: UINT8 v0[:byte-size 0];
               default: OptRegion_selen_SubjectPublicKeyInfo(len) v1;
               }
} Opt_selen_SubjectPublicKeyInfo;

entrypoint
typedef struct WireOptFramed
{
   UINT8 flag;
   UINT16BE len;
   Opt_selen_SubjectPublicKeyInfo((((flag != 0)) ? 1 : 0),
   len) key;
} OptFramed;

|}

let test_opt_framed () =
  Alcotest.(check string)
    "3D" opt_framed_3d
    (to_3d ~mode:`Standalone Asn1_modules.opt_framed);
  let s = Asn1.project "Framed" Asn1_modules.opt_framed in
  Alcotest.(check string) "ASN.1" framed_asn (printed s);
  Alcotest.(check (list string))
    "roots" [ "SubjectPublicKeyInfo" ] (Asn1.roots s)

let test_wrapped () =
  Alcotest.(check (list string))
    "the binary contents only" [ "Payload" ]
    (List.map Everparse.name (contained Asn1_modules.wrapped))

let check_module name t ~roots expected () =
  let s = projected name t in
  Alcotest.(check string) "text" expected (printed s);
  Alcotest.(check string) "name" name (Asn1.name s);
  Alcotest.(check string) "filename" (name ^ ".asn") (Asn1.filename s);
  Alcotest.(check (list string)) "roots" roots (Asn1.roots s)

(* The projection is a function of the type: a second projection prints the
   same text. *)
let test_deterministic () =
  List.iter
    (fun (name, Asn1_modules.Packed t) ->
      Alcotest.(check string)
        name
        (printed (project name t))
        (printed (project name t)))
    Asn1_modules.types

(* -- The binary codecs under a containing -- *)

let test_contained () =
  let names = List.map Everparse.name (contained Asn1_modules.constructs) in
  Alcotest.(check (list string))
    "binary contents"
    [ "Payload"; "Kept_word_contents" ]
    names;
  Alcotest.(check (list string))
    "none in SPKI" []
    (List.map Everparse.name (contained Asn1_modules.spki))

(* A codec held twice is one schema. *)
let test_contained_once () =
  let payload = Asn1.containing (codec Asn1_modules.payload_codec) in
  let a = Asn1.required "a" payload in
  let b = Asn1.required "b" (Asn1.explicit 0 payload) in
  let t =
    Asn1.sequence "Twice" (fun a b -> (a, b)) Codec.[ a $ fst; b $ snd ]
  in
  Alcotest.(check (list string))
    "one schema" [ "Payload" ]
    (List.map Everparse.name (Asn1.contained (Asn1.codec t)))

(* -- What the projection refuses -- *)

let test_module_names () =
  List.iter
    (fun m ->
      rejects ~msg:"is not an ASN.1 module name" (fun () ->
          project m Asn1.integer))
    [ "spki"; "A_b"; "A--b"; "A-"; "1A"; ""; "END"; "INTEGER" ]

let test_codec_without_asn1 () =
  rejects ~msg:"holds no ASN.1 type" (fun () ->
      Asn1.project "Payload" Asn1_modules.payload_codec);
  Alcotest.(check int)
    "contained" 0
    (List.length (Asn1.contained Asn1_modules.payload_codec))

(* -- Writing -- *)

let test_write () =
  let dir = Filename.temp_dir "asn1" "" in
  let s = projected "SPKI" Asn1_modules.spki in
  Asn1.write ~outdir:dir [ s; s ];
  let file = Filename.concat dir "SPKI.asn" in
  let text = In_channel.with_open_bin file In_channel.input_all in
  Sys.remove file;
  Sys.rmdir dir;
  Alcotest.(check string) "file" spki_asn text

let test_write_clash () =
  rejects ~msg:"two different modules are named \"M\"" (fun () ->
      Asn1.write ~outdir:"." [ project "M" Asn1.integer; project "M" Asn1.bool ])

let suite =
  ( "asn1-project",
    [
      Alcotest.test_case "SPKI" `Quick
        (check_module "SPKI" Asn1_modules.spki ~roots:[ "SubjectPublicKeyInfo" ]
           spki_asn);
      Alcotest.test_case "every construct" `Quick
        (check_module "Constructs" Asn1_modules.constructs
           ~roots:[ "Constructs" ] constructs_asn);
      Alcotest.test_case "root of no name" `Quick
        (check_module "Names" Asn1_modules.names ~roots:[ "Names" ] names_asn);
      Alcotest.test_case "root keeping its bytes" `Quick
        (check_module "KeptRoot" Asn1_modules.kept_root ~roots:[ "KeptRoot" ]
           kept_root_asn);
      Alcotest.test_case "names sanitised and unique" `Quick
        (check_module "Clash" Asn1_modules.clash ~roots:[ "Clash" ] clash_asn);
      Alcotest.test_case "EverParse's X509Lite" `Quick
        (check_module "X509Lite" Asn1_modules.x509lite ~roots:[ "Cert" ]
           x509lite_asn);
      Alcotest.test_case "times, IRIs and RELATIVE-OID" `Quick
        (check_module "Times" Asn1_modules.times ~roots:[ "Times" ] times_asn);
      Alcotest.test_case "DER in a binary codec" `Quick test_framed;
      Alcotest.test_case "DER in a binary codec: 3D" `Quick test_framed_3d;
      Alcotest.test_case "DER under a trivial where: 3D" `Quick
        test_framed_where_3d;
      Alcotest.test_case "DER under an optional" `Quick test_opt_framed;
      Alcotest.test_case "DER under a containing" `Quick
        (check_module "Wrapped" Asn1_modules.wrapped ~roots:[ "Wrapped" ]
           wrapped_asn);
      Alcotest.test_case "DER under a containing: contained" `Quick test_wrapped;
      Alcotest.test_case "deterministic" `Quick test_deterministic;
      Alcotest.test_case "contained" `Quick test_contained;
      Alcotest.test_case "contained once" `Quick test_contained_once;
      Alcotest.test_case "module names" `Quick test_module_names;
      Alcotest.test_case "codec without ASN.1" `Quick test_codec_without_asn1;
      Alcotest.test_case "write" `Quick test_write;
      Alcotest.test_case "write clash" `Quick test_write_clash;
    ] )

let () = Alcotest.run "wire-asn1" [ suite ]
