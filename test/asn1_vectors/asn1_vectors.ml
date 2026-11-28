(* EverParse's ASN.1 DER test vectors, from
   share/everparse/tests/asn1/difftest/vectors/*.json at EverParse 49875441c.
   Each verdict cites the X.690 or X.680 clause it comes from, never a
   parser's output (src/ASN1/difftest/README.md:16-26). *)

type t = {
  file : string;
  id : string;
  typ : string;
  hex : string;
  accept : bool;
  clause : string;
}

let pp ppf v = Fmt.pf ppf "%s/%s (%s)" v.file v.id v.clause

let vector file id typ hex accept clause =
  { file; id; typ; hex; accept; clause }

let accept = true
let reject = false

let any =
  let v = vector "any" in
  [
    v "any/accept/integer" "DiffAny.AnyT" "020105" accept "X.208 27";
    v "any/accept/sequence" "DiffAny.AnyT" "3003020105" accept "X.208 27";
    v "any/accept/null" "DiffAny.AnyT" "0500" accept "X.208 27";
    v "any/accept/high-tag" "DiffAny.AnyT" "9f6400" accept "X.208 27";
    v "any/reject/length-long-form" "DiffAny.AnyT" "048101aa" reject
      "X.690 10.1";
    v "any/reject/indefinite" "DiffAny.AnyT" "24800401aa0000" reject
      "X.690 10.1";
    v "any/reject/truncated" "DiffAny.AnyT" "0402aa" reject "X.690 8.1.3";
    v "any/reject/two" "DiffAny.AnyT" "05000500" reject "X.690 8.1.1";
    v "any/reject/tag-nonminimal" "DiffAny.AnyT" "9f800100" reject
      "X.690 8.1.2.4.2 c";
    v "any/reject/low-tag-in-high-form" "DiffAny.AnyT" "1f0500" reject
      "X.690 8.1.2.4.2 c";
    v "algid/accept/123-null" "DiffAny.AlgId" "300606022a030500" accept
      "X.208 27";
    v "algid/accept/124-int" "DiffAny.AlgId" "300706022a04020105" accept
      "X.208 27";
    v "algid/accept/124-absent" "DiffAny.AlgId" "300406022a04" accept "X.208 27";
    v "algid/reject/123-absent" "DiffAny.AlgId" "300406022a03" reject
      "schema (ASN1.DiffTest.Any)";
    v "algid/reject/123-int" "DiffAny.AlgId" "300706022a03020105" reject
      "schema (ASN1.DiffTest.Any)";
    v "algid/reject/124-two-params" "DiffAny.AlgId" "300906022a040201050500"
      reject "X.680 25 (no extension marker)";
    v "algid/reject/124-param-length-long-form" "DiffAny.AlgId"
      "300706022a04058100" reject "X.690 10.1";
    v "algid/reject/no-oid" "DiffAny.AlgId" "30020500" reject "X.680 25";
  ]

let constraints =
  let v = vector "constraints" in
  [
    v "sized/accept/2" "DiffConstraints.Sized" "0402abcd" accept "X.680 51.5";
    v "sized/accept/3" "DiffConstraints.Sized" "0403abcdef" accept "X.680 51.5";
    v "sized/reject/1" "DiffConstraints.Sized" "0401ab" reject "X.680 51.5";
    v "sized/reject/4" "DiffConstraints.Sized" "0404abcdef01" reject
      "X.680 51.5";
    v "byte/accept/0" "DiffConstraints.Byte" "020100" accept "X.680 51.4";
    v "byte/accept/255" "DiffConstraints.Byte" "020200ff" accept "X.680 51.4";
    v "byte/reject/256" "DiffConstraints.Byte" "02020100" reject "X.680 51.4";
    v "byte/reject/minus-1" "DiffConstraints.Byte" "0201ff" reject "X.680 51.4";
    v "nonemptyseqof/accept/one" "DiffConstraints.NonEmptySeqOf" "3003020101"
      accept "X.680 51.5";
    v "nonemptyseqof/reject/empty" "DiffConstraints.NonEmptySeqOf" "3000" reject
      "X.680 51.5";
    v "nonemptysetof/accept/one" "DiffConstraints.NonEmptySetOf" "3103020101"
      accept "X.680 51.5";
    v "nonemptysetof/accept/sorted" "DiffConstraints.NonEmptySetOf"
      "3106020101020102" accept "X.690 11.6";
    v "nonemptysetof/reject/empty" "DiffConstraints.NonEmptySetOf" "3100" reject
      "X.680 51.5";
    v "nonemptysetof/reject/unsorted" "DiffConstraints.NonEmptySetOf"
      "3106020102020101" reject "X.690 11.6";
  ]

let constructed =
  let v = vector "constructed" in
  [
    v "seq/accept/a" "DiffConstructed.Seq" "3003020101" accept "X.690 8.9";
    v "seq/accept/a-b" "DiffConstructed.Seq" "30060201010101ff" accept
      "X.690 8.9";
    v "seq/accept/a-c" "DiffConstructed.Seq" "30070201018002abcd" accept
      "X.690 8.9, 8.14";
    v "seq/accept/a-d" "DiffConstructed.Seq" "3008020101a103020105" accept
      "X.690 8.9, 8.14";
    v "seq/accept/all" "DiffConstructed.Seq"
      "300f0201010101ff8002abcda103020105" accept "X.690 8.9";
    v "seq/reject/default-encoded" "DiffConstructed.Seq" "3006020101010100"
      reject "X.690 11.5";
    v "seq/reject/out-of-order" "DiffConstructed.Seq"
      "300c020101a1030201058002abcd" reject "X.690 8.9.2";
    v "seq/reject/b-after-c" "DiffConstructed.Seq" "300a0201018002abcd0101ff"
      reject "X.690 8.9.2";
    v "seq/reject/missing-a" "DiffConstructed.Seq" "30030101ff" reject
      "X.680 25";
    v "seq/reject/c-constructed" "DiffConstructed.Seq" "3007020101a002abcd"
      reject "X.690 8.14.4, 8.7.1";
    v "seq/reject/d-implicit" "DiffConstructed.Seq" "3006020101810105" reject
      "X.690 8.14.2";
    v "seq/reject/extra-component" "DiffConstructed.Seq" "30050201010500" reject
      "X.680 25 (no extension marker)";
    v "seq/reject/trailing-after" "DiffConstructed.Seq" "300302010100" reject
      "X.690 8.1.1";
    v "seq/reject/length-short" "DiffConstructed.Seq" "3002020101" reject
      "X.690 8.1.3";
    v "seq/reject/inner-overrun" "DiffConstructed.Seq" "3003020201" reject
      "X.690 8.1.3";
    v "seq/reject/indefinite" "DiffConstructed.Seq" "30800201010000" reject
      "X.690 10.1";
    v "seq/reject/primitive" "DiffConstructed.Seq" "1003020101" reject
      "X.690 8.9.1";
    v "seq/reject/set-tag" "DiffConstructed.Seq" "3103020101" reject
      "X.690 8.9.1";
    v "intseq/accept/empty" "DiffConstructed.IntSeq" "3000" accept "X.690 8.10";
    v "intseq/accept/two" "DiffConstructed.IntSeq" "3006020101020102" accept
      "X.690 8.10";
    v "intseq/accept/unsorted-duplicates" "DiffConstructed.IntSeq"
      "3009020102020101020101" accept "X.690 8.10";
    v "intseq/reject/wrong-element" "DiffConstructed.IntSeq" "30030101ff" reject
      "X.690 8.10";
    v "intseq/reject/bad-element" "DiffConstructed.IntSeq" "300402020001" reject
      "X.690 8.3.2";
    v "intseq/reject/indefinite" "DiffConstructed.IntSeq" "30800201010000"
      reject "X.690 10.1";
    v "octetsset/accept/empty" "DiffConstructed.OctetsSet" "3100" accept
      "X.690 8.12";
    v "octetsset/accept/sorted" "DiffConstructed.OctetsSet" "31060401aa0401bb"
      accept "X.690 11.6";
    v "octetsset/accept/equal" "DiffConstructed.OctetsSet" "31060401aa0401aa"
      accept "X.690 11.6";
    v "octetsset/accept/shorter-first" "DiffConstructed.OctetsSet"
      "310504000401aa" accept "X.690 11.6";
    v "octetsset/accept/length-first" "DiffConstructed.OctetsSet"
      "31070401ff04020000" accept "X.690 11.6 (compares encodings)";
    v "octetsset/reject/unsorted" "DiffConstructed.OctetsSet" "31060401bb0401aa"
      reject "X.690 11.6";
    v "octetsset/reject/longer-first" "DiffConstructed.OctetsSet"
      "31050401aa0400" reject "X.690 11.6";
    v "octetsset/reject/by-content" "DiffConstructed.OctetsSet"
      "3107040200000401ff" reject "X.690 11.6 (compares encodings, not values)";
    v "octetsset/reject/sequence-tag" "DiffConstructed.OctetsSet" "30030401aa"
      reject "X.690 8.12.1";
    v "choice/accept/i" "DiffConstructed.Choice" "020105" accept "X.690 8.13";
    v "choice/accept/b" "DiffConstructed.Choice" "0101ff" accept "X.690 8.13";
    v "choice/accept/o" "DiffConstructed.Choice" "8002abcd" accept
      "X.690 8.13, 8.14";
    v "choice/reject/unknown" "DiffConstructed.Choice" "0400" reject
      "X.690 8.13";
    v "choice/reject/o-constructed" "DiffConstructed.Choice" "a0020400" reject
      "X.690 8.14";
    v "choice/reject/two" "DiffConstructed.Choice" "0201050101ff" reject
      "X.690 8.13";
    v "app/accept" "DiffConstructed.App" "450105" accept "X.690 8.14.4";
    v "app/reject/universal" "DiffConstructed.App" "020105" reject
      "X.690 8.14.4";
    v "app/reject/explicit" "DiffConstructed.App" "6503020105" reject
      "X.690 8.14.4";
    v "app/reject/context" "DiffConstructed.App" "850105" reject "X.690 8.1.2.2";
    v "privexp/accept" "DiffConstructed.PrivExp" "e3030101ff" accept
      "X.690 8.14.3";
    v "privexp/reject/implicit" "DiffConstructed.PrivExp" "c301ff" reject
      "X.690 8.14.3";
    v "privexp/reject/inner-bool" "DiffConstructed.PrivExp" "e303010101" reject
      "X.690 11.1";
    v "privexp/reject/inner-trailing" "DiffConstructed.PrivExp" "e3050101ff0500"
      reject "X.690 8.14.3";
    v "highctx/accept" "DiffConstructed.HighCtx" "9f640105" accept
      "X.690 8.1.2.4";
    v "highctx/reject/tag-nonminimal" "DiffConstructed.HighCtx" "9f80640105"
      reject "X.690 8.1.2.4.2 c";
    v "highctx/reject/tag-101" "DiffConstructed.HighCtx" "9f650105" reject
      "X.690 8.1.2.4";
    v "highctx/reject/truncated-tag" "DiffConstructed.HighCtx" "9f" reject
      "X.690 8.1.2.4";
    v "highapp/accept" "DiffConstructed.HighApp" "7f8768040402abcd" accept
      "X.690 8.1.2.4, 8.14.3";
    v "highapp/reject/tag-nonminimal" "DiffConstructed.HighApp"
      "7f808768040402abcd" reject "X.690 8.1.2.4.2 c";
    v "highapp/reject/primitive" "DiffConstructed.HighApp" "5f876802abcd" reject
      "X.690 8.14.3";
    v "nested/accept/no-ch" "DiffConstructed.Nested" "300730030201013000" accept
      "X.690 8.9";
    v "nested/accept/ch-b" "DiffConstructed.Nested" "300a300302010130000101ff"
      accept "X.690 8.9, 8.13";
    v "nested/accept/ch-o" "DiffConstructed.Nested"
      "300e300302010130030201028002abcd" accept "X.690 8.9, 8.13";
    v "nested/reject/extra-after-ch" "DiffConstructed.Nested"
      "300c300302010130000101ff0500" reject "X.680 25 (no extension marker)";
    v "nested/reject/inner-default-encoded" "DiffConstructed.Nested"
      "300a30060201010101003000" reject "X.690 11.5";
    v "nested/reject/inner-extra" "DiffConstructed.Nested"
      "3009300502010105003000" reject "X.680 25 (no extension marker)";
  ]

let presentation =
  let v = vector "presentation" in
  [
    v "external/accept/octet-aligned" "DiffExternal.Ext" "280806022a038102abcd"
      accept "X.690 8.18";
    v "external/accept/single-asn1-type" "DiffExternal.Ext"
      "280906022a03a003020105" accept "X.690 8.18";
    v "external/accept/arbitrary" "DiffExternal.Ext" "280806022a0382020780"
      accept "X.690 8.18";
    v "external/accept/indirect" "DiffExternal.Ext" "28070201058102abcd" accept
      "X.690 8.18";
    v "external/accept/descriptor" "DiffExternal.Ext"
      "280d06022a0307036162638102abcd" accept "X.690 8.18";
    v "external/reject/no-encoding" "DiffExternal.Ext" "280406022a03" reject
      "X.690 8.18.1";
    v "external/reject/sequence-tag" "DiffExternal.Ext" "300806022a038102abcd"
      reject "X.690 8.18.1";
    v "external/reject/out-of-order" "DiffExternal.Ext" "28088102abcd06022a03"
      reject "X.690 8.18.1";
    v "external/reject/arbitrary-unused-bits" "DiffExternal.Ext"
      "280806022a0382020781" reject "X.690 11.2.1";
    v "pdv/accept/syntax" "DiffPdv.Pdv" "2b0aa00481022a038202abcd" accept
      "X.690 8.10.1 (sic: 8.17), X.680 36.5";
    v "pdv/accept/fixed" "DiffPdv.Pdv" "2b08a00285008202abcd" accept
      "X.680 36.5";
    v "pdv/accept/syntaxes" "DiffPdv.Pdv" "2b10a00aa00880022a0381022a048202abcd"
      accept "X.680 36.5";
    v "pdv/accept/presentation-context-id" "DiffPdv.Pdv"
      "2b09a0038201058202abcd" accept "X.680 36.5";
    v "pdv/reject/data-value-descriptor" "DiffPdv.Pdv"
      "2b0fa00481022a0381036162638202abcd" reject
      "X.680 36.5 (data-value-descriptor ABSENT)";
    v "pdv/reject/no-data-value" "DiffPdv.Pdv" "2b06a00481022a03" reject
      "X.680 36.5";
    v "pdv/reject/identification-implicit" "DiffPdv.Pdv" "2b0881022a038202abcd"
      reject "X.680 36.5 (identification is EXPLICIT, being a CHOICE)";
    v "charstr/accept/syntax" "DiffCharStr.CharStr" "3d0aa00481022a038202abcd"
      accept "X.690 8.24, X.680 44.5";
    v "charstr/accept/fixed" "DiffCharStr.CharStr" "3d08a00285008202abcd" accept
      "X.680 44.5";
    v "charstr/reject/no-string-value" "DiffCharStr.CharStr" "3d06a00481022a03"
      reject "X.680 44.5";
    v "charstr/reject/descriptor" "DiffCharStr.CharStr"
      "3d0fa00481022a0381036162638202abcd" reject
      "X.680 44.5 (data-value-descriptor ABSENT)";
  ]

let prim =
  let v = vector "prim" in
  [
    v "bool/accept/true" "DiffPrim.Bool" "0101ff" accept "X.690 11.1";
    v "bool/accept/false" "DiffPrim.Bool" "010100" accept "X.690 8.2.2";
    v "bool/reject/true-01" "DiffPrim.Bool" "010101" reject "X.690 11.1";
    v "bool/reject/true-80" "DiffPrim.Bool" "010180" reject "X.690 11.1";
    v "bool/reject/two-octets" "DiffPrim.Bool" "0102ffff" reject "X.690 8.2.1";
    v "bool/reject/empty" "DiffPrim.Bool" "0100" reject "X.690 8.2.1";
    v "bool/reject/constructed" "DiffPrim.Bool" "21030101ff" reject
      "X.690 8.2.1";
    v "bool/reject/length-long-form" "DiffPrim.Bool" "018101ff" reject
      "X.690 10.1";
    v "bool/reject/trailing" "DiffPrim.Bool" "0101ff00" reject "X.690 8.1.1";
    v "bool/reject/wrong-tag" "DiffPrim.Bool" "0201ff" reject "X.690 8.1.2";
    v "int/accept/zero" "DiffPrim.Int" "020100" accept "X.690 8.3.2";
    v "int/accept/127" "DiffPrim.Int" "02017f" accept "X.690 8.3.3";
    v "int/accept/128" "DiffPrim.Int" "02020080" accept "X.690 8.3.2";
    v "int/accept/minus-128" "DiffPrim.Int" "020180" accept "X.690 8.3.3";
    v "int/accept/minus-129" "DiffPrim.Int" "0202ff7f" accept "X.690 8.3.2";
    v "int/accept/2-64-minus-1" "DiffPrim.Int" "020900ffffffffffffffff" accept
      "X.690 8.3";
    v "int/accept/20-octets" "DiffPrim.Int"
      "02147fffffffffffffffffffffffffffffffffffffff" accept "X.690 8.3";
    v "int/reject/leading-00" "DiffPrim.Int" "02020001" reject "X.690 8.3.2";
    v "int/reject/leading-ff" "DiffPrim.Int" "0202ff80" reject "X.690 8.3.2";
    v "int/reject/empty" "DiffPrim.Int" "0200" reject "X.690 8.3.1";
    v "int/reject/constructed" "DiffPrim.Int" "2203020100" reject "X.690 8.3.1";
    v "int/reject/truncated" "DiffPrim.Int" "020201" reject "X.690 8.1.3";
    v "int/accept/21-octets" "DiffPrim.Int"
      "02157fffffffffffffffffffffffffffffffffffffffff" accept "X.690 8.3";
    v "int/accept/129-octets" "DiffPrim.Int"
      "0281817fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      accept "X.690 8.3, 8.1.3.5";
    v "enum/accept/a" "DiffPrim.Enum" "0a0100" accept "X.690 8.4";
    v "enum/accept/c" "DiffPrim.Enum" "0a0105" accept "X.690 8.4";
    v "enum/reject/unknown-value" "DiffPrim.Enum" "0a0102" reject "X.680 20.1";
    v "enum/reject/leading-00" "DiffPrim.Enum" "0a020005" reject
      "X.690 8.4, 8.3.2";
    v "enum/reject/integer-tag" "DiffPrim.Enum" "020105" reject "X.690 8.4";
    v "bits/accept/empty" "DiffPrim.Bits" "030100" accept "X.690 8.6.2.3";
    v "bits/accept/one-bit" "DiffPrim.Bits" "03020780" accept "X.690 8.6.2.2";
    v "bits/accept/byte" "DiffPrim.Bits" "030200ff" accept "X.690 8.6.2";
    v "bits/reject/unused-without-content" "DiffPrim.Bits" "030101" reject
      "X.690 8.6.2.3";
    v "bits/reject/unused-bits-nonzero" "DiffPrim.Bits" "03020781" reject
      "X.690 11.2.1";
    v "bits/reject/unused-8" "DiffPrim.Bits" "03020800" reject "X.690 8.6.2.2";
    v "bits/reject/empty-contents" "DiffPrim.Bits" "0300" reject "X.690 8.6.2";
    v "bits/reject/constructed" "DiffPrim.Bits" "2304030200ff" reject
      "X.690 10.2";
    v "octets/accept/empty" "DiffPrim.Octets" "0400" accept "X.690 8.7";
    v "octets/accept/three" "DiffPrim.Octets" "0403010203" accept "X.690 8.7";
    v "octets/accept/128" "DiffPrim.Octets"
      "0481800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      accept "X.690 10.1, 8.1.3.5";
    v "octets/accept/256" "DiffPrim.Octets"
      "0482010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      accept "X.690 10.1, 8.1.3.5";
    v "octets/reject/long-form-127" "DiffPrim.Octets"
      "04817f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      reject "X.690 10.1";
    v "octets/reject/long-form-leading-00" "DiffPrim.Octets"
      "048200800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      reject "X.690 10.1";
    v "octets/reject/indefinite" "DiffPrim.Octets" "04800000" reject
      "X.690 10.1";
    v "octets/reject/length-ff" "DiffPrim.Octets" "04ff" reject
      "X.690 8.1.3.5 c";
    v "octets/reject/overrun" "DiffPrim.Octets" "04050102" reject "X.690 8.1.3";
    v "octets/reject/constructed" "DiffPrim.Octets" "24050403010203" reject
      "X.690 10.2";
    v "null/accept" "DiffPrim.Null" "0500" accept "X.690 8.8";
    v "null/reject/nonempty" "DiffPrim.Null" "050100" reject "X.690 8.8.2";
    v "null/reject/constructed" "DiffPrim.Null" "2500" reject "X.690 8.8.1";
    v "null/reject/length-long-form" "DiffPrim.Null" "058100" reject
      "X.690 10.1";
    v "oid/accept/1.2.3.4" "DiffPrim.Oid" "06032a0304" accept "X.690 8.19";
    v "oid/accept/1.2" "DiffPrim.Oid" "06012a" accept "X.690 8.19.4";
    v "oid/accept/0.0" "DiffPrim.Oid" "060100" accept "X.690 8.19.4";
    v "oid/accept/2.999.3" "DiffPrim.Oid" "0603883703" accept "X.690 8.19.4";
    v "oid/accept/rsa" "DiffPrim.Oid" "06092a864886f70d010101" accept
      "X.690 8.19";
    v "oid/accept/arc-2-32-minus-1" "DiffPrim.Oid" "06062a8fffffff7f" accept
      "X.690 8.19.2";
    v "oid/reject/leading-80" "DiffPrim.Oid" "06032a8003" reject "X.690 8.19.2";
    v "oid/reject/first-leading-80" "DiffPrim.Oid" "0602802a" reject
      "X.690 8.19.2";
    v "oid/reject/empty" "DiffPrim.Oid" "0600" reject "X.690 8.19.2";
    v "oid/reject/truncated-subid" "DiffPrim.Oid" "06022a83" reject
      "X.690 8.19.2";
    v "oid/reject/constructed" "DiffPrim.Oid" "26032a0304" reject "X.690 8.19.1";
    v "policy/oid/arc-2-32" "DiffPrim.Oid" "06062a9080808000" reject
      "policy: OID arcs are 32-bit (ASN1.Spec.Content.OIDU32)";
    v "reloid/accept/1.2.3" "DiffRelOid.RelOid" "0d03010203" accept "X.690 8.20";
    v "reloid/accept/large-arc" "DiffRelOid.RelOid" "0d02a43c" accept
      "X.690 8.20.2";
    v "reloid/reject/empty" "DiffRelOid.RelOid" "0d00" reject
      "X.680 33.1, X.690 8.20.2";
    v "reloid/reject/leading-80" "DiffRelOid.RelOid" "0d028001" reject
      "X.690 8.20.2";
    v "reloid/reject/oid-tag" "DiffRelOid.RelOid" "0603010203" reject
      "X.690 8.20.1";
    v "desc/accept/abc" "DiffDesc.Desc" "0703616263" accept "X.690 8.23";
    v "desc/accept/empty" "DiffDesc.Desc" "0700" accept "X.690 8.23";
    v "desc/reject/constructed" "DiffDesc.Desc" "27050703616263" reject
      "X.690 10.2";
    v "iri/accept/iso" "DiffIRI.Iri" "1f23042f49534f" accept
      "X.690 8.21, X.660 7.5";
    v "iri/accept/numeric" "DiffIRI.Iri" "1f23042f312f32" accept
      "X.690 8.21, X.660 7.5";
    v "iri/accept/unicode" "DiffIRI.Iri" "1f23032fc3a9" accept
      "X.690 8.21, X.660 7.5";
    v "iri/reject/empty" "DiffIRI.Iri" "1f2300" reject "X.680 34.3";
    v "iri/reject/no-slash" "DiffIRI.Iri" "1f230349534f" reject "X.680 34.3";
    v "iri/reject/leading-zero" "DiffIRI.Iri" "1f23032f3031" reject
      "X.660 7.5.2";
    v "iri/reject/empty-label" "DiffIRI.Iri" "1f23052f612f2f62" reject
      "X.680 34.3";
    v "iri/reject/space" "DiffIRI.Iri" "1f23042f612062" reject "X.660 7.5.3";
    v "iri/reject/bad-utf8" "DiffIRI.Iri" "1f23022fc3" reject "X.690 8.21.5";
    v "reliri/accept/a-b" "DiffIRI.RelIri" "1f2403612f62" accept
      "X.690 8.22, X.660 7.5";
    v "reliri/reject/empty" "DiffIRI.RelIri" "1f2400" reject "X.680 35.3";
    v "reliri/reject/leading-slash" "DiffIRI.RelIri" "1f24022f61" reject
      "X.680 35.3";
  ]

let set =
  let v = vector "set" in
  [
    v "set/accept/c-a" "DiffSet.Set" "31070402abcd800105" accept
      "X.690 10.3, 8.11";
    v "set/accept/c-a-b" "DiffSet.Set" "310a0402abcd8001058101ff" accept
      "X.690 10.3, 8.11";
    v "set/reject/a-c" "DiffSet.Set" "31078001050402abcd" reject "X.690 10.3";
    v "set/reject/c-b-a" "DiffSet.Set" "310a0402abcd8101ff800105" reject
      "X.690 10.3";
    v "set/reject/a-b-c" "DiffSet.Set" "310a8001058101ff0402abcd" reject
      "X.690 10.3";
    v "set/reject/missing-a" "DiffSet.Set" "31040402abcd" reject "X.680 27";
    v "set/reject/duplicate-a" "DiffSet.Set" "310a0402abcd800105800105" reject
      "X.680 27";
    v "set/reject/sequence-tag" "DiffSet.Set" "30070402abcd800105" reject
      "X.690 8.11.1";
    v "set/reject/unknown" "DiffSet.Set" "310a0402abcd800105820100" reject
      "X.680 27 (no extension marker)";
    v "set/reject/b-not-ff" "DiffSet.Set" "310a0402abcd800105810101" reject
      "X.690 11.1";
  ]

let smoke =
  let v = vector "smoke" in
  [
    v "smoke/accept/with-b" "DiffSmoke.Smoke" "300a0201050101ff0402abcd" accept
      "X.690 8.9";
    v "smoke/accept/without-b" "DiffSmoke.Smoke" "30070201050402abcd" accept
      "X.690 8.9";
    v "smoke/accept/a-zero" "DiffSmoke.Smoke" "30070201000402abcd" accept
      "X.690 8.3.2";
    v "smoke/accept/c-empty" "DiffSmoke.Smoke" "30050201050400" accept
      "X.690 8.7";
    v "smoke/reject/boolean-01" "DiffSmoke.Smoke" "300a0201050101010402abcd"
      reject "X.690 11.1";
    v "smoke/reject/integer-leading-00" "DiffSmoke.Smoke" "3008020200050402abcd"
      reject "X.690 8.3.2";
    v "smoke/reject/length-long-form" "DiffSmoke.Smoke" "3081070201050402abcd"
      reject "X.690 10.1";
    v "smoke/reject/missing-c" "DiffSmoke.Smoke" "3003020105" reject
      "X.680 25 (mandatory component absent)";
  ]

let strings =
  let v = vector "strings" in
  [
    v "utf8/accept/0" "DiffStrings.Utf8" "0c03616263" accept "X.690 8.23.10";
    v "utf8/accept/1" "DiffStrings.Utf8" "0c00" accept "X.690 8.23";
    v "utf8/accept/2" "DiffStrings.Utf8" "0c02c3a9" accept "X.690 8.23.10";
    v "utf8/accept/3" "DiffStrings.Utf8" "0c04f09f9880" accept "X.690 8.23.10";
    v "utf8/reject/0" "DiffStrings.Utf8" "0c02c328" reject
      "X.690 8.23.10, RFC 3629";
    v "utf8/reject/1" "DiffStrings.Utf8" "0c0180" reject
      "X.690 8.23.10, RFC 3629";
    v "utf8/reject/2" "DiffStrings.Utf8" "0c02c080" reject
      "X.690 8.23.10, RFC 3629 (overlong)";
    v "utf8/reject/3" "DiffStrings.Utf8" "0c03eda080" reject
      "X.690 8.23.10, RFC 3629 (surrogate)";
    v "utf8/reject/4" "DiffStrings.Utf8" "0c04f4908080" reject
      "X.690 8.23.10, RFC 3629 (> U+10FFFF)";
    v "utf8/reject/5" "DiffStrings.Utf8" "0c02e282" reject
      "X.690 8.23.10, RFC 3629 (truncated)";
    v "utf8/reject/constructed" "DiffStrings.Utf8" "2c030c0161" reject
      "X.690 10.2";
    v "numeric/accept/0" "DiffStrings.Numeric" "12053132203334" accept
      "X.680 41.2";
    v "numeric/accept/1" "DiffStrings.Numeric" "1200" accept "X.680 41.2";
    v "numeric/reject/0" "DiffStrings.Numeric" "12023161" reject "X.680 41.2";
    v "numeric/reject/1" "DiffStrings.Numeric" "12022d31" reject "X.680 41.2";
    v "numeric/reject/constructed" "DiffStrings.Numeric" "3203120161" reject
      "X.690 10.2";
    v "printable/accept/0" "DiffStrings.Printable"
      "130f416239202728292b2c2d2e2f3a3d3f" accept "X.680 41.4";
    v "printable/accept/1" "DiffStrings.Printable" "1300" accept "X.680 41.4";
    v "printable/reject/0" "DiffStrings.Printable" "13012a" reject "X.680 41.4";
    v "printable/reject/1" "DiffStrings.Printable" "130140" reject "X.680 41.4";
    v "printable/reject/2" "DiffStrings.Printable" "130126" reject "X.680 41.4";
    v "printable/reject/3" "DiffStrings.Printable" "1303615f62" reject
      "X.680 41.4";
    v "printable/reject/4" "DiffStrings.Printable" "1302c3a9" reject
      "X.680 41.4";
    v "printable/reject/constructed" "DiffStrings.Printable" "3303130161" reject
      "X.690 10.2";
    v "ia5/accept/0" "DiffStrings.Ia5" "1603616263" accept "X.680 41.2";
    v "ia5/accept/1" "DiffStrings.Ia5" "1603000a7f" accept
      "X.680 41.2 (control characters are IA5)";
    v "ia5/reject/0" "DiffStrings.Ia5" "160180" reject "X.680 41.2";
    v "ia5/reject/1" "DiffStrings.Ia5" "1602c3a9" reject "X.680 41.2";
    v "ia5/reject/constructed" "DiffStrings.Ia5" "3603160161" reject
      "X.690 10.2";
    v "visible/accept/0" "DiffStrings.Visible" "1a046120627e" accept
      "X.680 41.4";
    v "visible/accept/1" "DiffStrings.Visible" "1a00" accept "X.680 41.4";
    v "visible/reject/0" "DiffStrings.Visible" "1a017f" reject "X.680 41.4";
    v "visible/reject/1" "DiffStrings.Visible" "1a010a" reject "X.680 41.4";
    v "visible/reject/2" "DiffStrings.Visible" "1a0180" reject "X.680 41.4";
    v "visible/reject/constructed" "DiffStrings.Visible" "3a031a0161" reject
      "X.690 10.2";
    v "bmp/accept/0" "DiffStrings.Bmp" "1e020041" accept "X.690 8.23.8";
    v "bmp/accept/1" "DiffStrings.Bmp" "1e00" accept "X.690 8.23.8";
    v "bmp/accept/2" "DiffStrings.Bmp" "1e0400e9fffd" accept "X.690 8.23.8";
    v "bmp/reject/0" "DiffStrings.Bmp" "1e03004100" reject
      "X.690 8.23.8 (odd length)";
    v "bmp/reject/1" "DiffStrings.Bmp" "1e02d83d" reject
      "X.680 41.16 (surrogate code point)";
    v "bmp/reject/constructed" "DiffStrings.Bmp" "3e031e0161" reject
      "X.690 10.2";
    v "universal/accept/0" "DiffStrings.Universal" "1c0400000041" accept
      "X.690 8.23.7";
    v "universal/accept/1" "DiffStrings.Universal" "1c00" accept "X.690 8.23.7";
    v "universal/accept/2" "DiffStrings.Universal" "1c040001f600" accept
      "X.690 8.23.7";
    v "universal/reject/0" "DiffStrings.Universal" "1c03000041" reject
      "X.690 8.23.7 (length not a multiple of 4)";
    v "universal/reject/1" "DiffStrings.Universal" "1c0400110000" reject
      "X.680 41.15 (> U+10FFFF)";
    v "universal/reject/2" "DiffStrings.Universal" "1c040000d800" reject
      "X.680 41.15 (surrogate code point)";
    v "universal/reject/constructed" "DiffStrings.Universal" "3c031c0161" reject
      "X.690 10.2";
    v "teletex/accept/0" "DiffStrings.Teletex" "1403616263" accept
      "X.690 8.23.5";
    v "teletex/accept/1" "DiffStrings.Teletex" "1400" accept "X.690 8.23.5";
    v "teletex/reject/constructed" "DiffStrings.Teletex" "3403140161" reject
      "X.690 10.2";
    v "videotex/accept/0" "DiffVideotex.Videotex" "1503616263" accept
      "X.690 8.23.5";
    v "videotex/accept/1" "DiffVideotex.Videotex" "1500" accept "X.690 8.23.5";
    v "videotex/reject/constructed" "DiffVideotex.Videotex" "3503150161" reject
      "X.690 10.2";
    v "graphic/accept/0" "DiffStrings2.Graphic" "1903616263" accept
      "X.690 8.23.5";
    v "graphic/accept/1" "DiffStrings2.Graphic" "1900" accept "X.690 8.23.5";
    v "graphic/reject/constructed" "DiffStrings2.Graphic" "3903190161" reject
      "X.690 10.2";
    v "general/accept/0" "DiffStrings2.General" "1b03616263" accept
      "X.690 8.23.5";
    v "general/accept/1" "DiffStrings2.General" "1b00" accept "X.690 8.23.5";
    v "general/reject/constructed" "DiffStrings2.General" "3b031b0161" reject
      "X.690 10.2";
  ]

let time =
  let v = vector "time" in
  [
    v "utc/accept/250101120000Z" "DiffTime.Utc" "170d3235303130313132303030305a"
      accept "X.690 11.8";
    v "utc/accept/500101000000Z" "DiffTime.Utc" "170d3530303130313030303030305a"
      accept "X.690 11.8";
    v "utc/accept/491231235959Z" "DiffTime.Utc" "170d3439313233313233353935395a"
      accept "X.690 11.8";
    v "utc/accept/240229120000Z" "DiffTime.Utc" "170d3234303232393132303030305a"
      accept "X.680 47.3";
    v "utc/accept/161231235960Z" "DiffTime.Utc" "170d3136313233313233353936305a"
      accept "X.690 11.8, ISO 8601 (leap second)";
    v "utc/reject/2501011200Z" "DiffTime.Utc" "170b323530313031313230305a"
      reject "X.690 11.8.2 (seconds required)";
    v "utc/reject/250101120000+0100" "DiffTime.Utc"
      "17113235303130313132303030302b30313030" reject
      "X.690 11.8.1 (Z required)";
    v "utc/reject/250101120000" "DiffTime.Utc" "170c323530313031313230303030"
      reject "X.690 11.8.1";
    v "utc/reject/250230120000Z" "DiffTime.Utc" "170d3235303233303132303030305a"
      reject "X.680 47.3 (no Feb 30)";
    v "utc/reject/250229120000Z" "DiffTime.Utc" "170d3235303232393132303030305a"
      reject "X.680 47.3 (2025 is not a leap year)";
    v "utc/reject/250101126000Z" "DiffTime.Utc" "170d3235303130313132363030305a"
      reject "X.680 47.3";
    v "utc/reject/250101240000Z" "DiffTime.Utc" "170d3235303130313234303030305a"
      reject "X.680 47.3";
    v "utc/reject/251301120000Z" "DiffTime.Utc" "170d3235313330313132303030305a"
      reject "X.680 47.3";
    v "utc/reject/250100120000Z" "DiffTime.Utc" "170d3235303130303132303030305a"
      reject "X.680 47.3";
    v "utc/reject/250101120060Z" "DiffTime.Utc" "170d3235303130313132303036305a"
      reject "ISO 8601 (leap second only at 23:59:60 UTC)";
    v "utc/reject/250101120000.5Z" "DiffTime.Utc"
      "170f3235303130313132303030302e355a" reject "X.690 11.8 (no fraction)";
    v "gen/accept/20250101120000Z" "DiffTime.Gen"
      "180f32303235303130313132303030305a" accept "X.690 11.7";
    v "gen/accept/20250101120000.5Z" "DiffTime.Gen"
      "181132303235303130313132303030302e355a" accept "X.690 11.7.3";
    v "gen/accept/20250101120000.123Z" "DiffTime.Gen"
      "181332303235303130313132303030302e3132335a" accept "X.690 11.7.3";
    v "gen/accept/20240229120000Z" "DiffTime.Gen"
      "180f32303234303232393132303030305a" accept "X.680 46.3";
    v "gen/accept/20000229000000Z" "DiffTime.Gen"
      "180f32303030303232393030303030305a" accept "X.680 46.3";
    v "gen/accept/20161231235960Z" "DiffTime.Gen"
      "180f32303136313233313233353936305a" accept
      "X.690 11.7, ISO 8601 (leap second)";
    v "gen/reject/20250101120000.50Z" "DiffTime.Gen"
      "181232303235303130313132303030302e35305a" reject
      "X.690 11.7.3 (trailing 0)";
    v "gen/reject/20250101120000.Z" "DiffTime.Gen"
      "181032303235303130313132303030302e5a" reject "X.690 11.7.3";
    v "gen/reject/20250101120000.0Z" "DiffTime.Gen"
      "181132303235303130313132303030302e305a" reject "X.690 11.7.3";
    v "gen/reject/20250101120000,5Z" "DiffTime.Gen"
      "181132303235303130313132303030302c355a" reject
      "X.690 11.7.4 (decimal point is .)";
    v "gen/reject/202501011200Z" "DiffTime.Gen" "180d3230323530313031313230305a"
      reject "X.690 11.7.2 (seconds required)";
    v "gen/reject/20250101120000" "DiffTime.Gen"
      "180e3230323530313031313230303030" reject "X.690 11.7.1 (Z required)";
    v "gen/reject/20250101120000+0100" "DiffTime.Gen"
      "181332303235303130313132303030302b30313030" reject "X.690 11.7.1";
    v "gen/reject/20250229120000Z" "DiffTime.Gen"
      "180f32303235303232393132303030305a" reject "X.680 46.3";
    v "gen/reject/19000229120000Z" "DiffTime.Gen"
      "180f31393030303232393132303030305a" reject
      "X.680 46.3 (1900 is not a leap year)";
    v "gen/reject/20250101250000Z" "DiffTime.Gen"
      "180f32303235303130313235303030305a" reject "X.680 46.3";
    v "gen/reject/2025010112Z" "DiffTime.Gen" "180b323032353031303131325a"
      reject "X.690 11.7.2";
    v "date/accept/20250101" "DiffTime2.Date" "1f1f083230323530313031" accept
      "X.690 8.26.2, X.680 38.4.1";
    v "date/accept/15821015" "DiffTime2.Date" "1f1f083135383231303135" accept
      "X.680 38.4.1 (Year=Basic)";
    v "date/accept/20240229" "DiffTime2.Date" "1f1f083230323430323239" accept
      "X.680 38.4.1";
    v "date/reject/2025-01-01" "DiffTime2.Date" "1f1f0a323032352d30312d3031"
      reject "X.690 8.26.2 (basic format)";
    v "date/reject/15811231" "DiffTime2.Date" "1f1f083135383131323331" reject
      "X.680 38.4.1 (Year=Basic is 1582..9999)";
    v "date/reject/20250230" "DiffTime2.Date" "1f1f083230323530323330" reject
      "X.680 38.4.1";
    v "date/reject/202501" "DiffTime2.Date" "1f1f06323032353031" reject
      "X.690 8.26.2";
    v "date/reject/tag-nonminimal" "DiffTime2.Date" "1f801f083230323530313031"
      reject "X.690 8.1.2.4.2 c";
    v "tod/accept/120000" "DiffTime2.Tod" "1f2006313230303030" accept
      "X.690 8.26.2, X.680 38.4.2";
    v "tod/accept/235960" "DiffTime2.Tod" "1f2006323335393630" accept
      "X.680 38.4.2 (local leap second)";
    v "tod/reject/240000" "DiffTime2.Tod" "1f2006323430303030" reject
      "X.680 38.4.2";
    v "tod/reject/12:00:00" "DiffTime2.Tod" "1f200831323a30303a3030" reject
      "X.690 8.26.2 (basic format)";
    v "tod/reject/1200" "DiffTime2.Tod" "1f200431323030" reject
      "X.680 38.4.2 (Time=HMS)";
    v "tod/reject/120000Z" "DiffTime2.Tod" "1f20073132303030305a" reject
      "X.680 38.4.2 (Local)";
    v "datetime/accept/20250101120000" "DiffTime2.DateTime"
      "1f210e3230323530313031313230303030" accept "X.690 8.26.2, X.680 38.4.3";
    v "datetime/reject/20250101120000Z" "DiffTime2.DateTime"
      "1f210f32303235303130313132303030305a" reject "X.680 38.4.3 (Local)";
    v "datetime/reject/2025-01-01T12:00:00" "DiffTime2.DateTime"
      "1f2113323032352d30312d30315431323a30303a3030" reject
      "X.690 8.26.2 (basic format)";
    v "datetime/reject/20250101T120000" "DiffTime2.DateTime"
      "1f210f323032353031303154313230303030" reject "X.690 8.26.2";
    v "datetime/reject/15000101120000" "DiffTime2.DateTime"
      "1f210e3135303030313031313230303030" reject "X.680 38.4.3 (Year=Basic)";
  ]

let all =
  List.concat
    [
      any;
      constraints;
      constructed;
      presentation;
      prim;
      set;
      smoke;
      strings;
      time;
    ]
