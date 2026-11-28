open Wire

(* SubjectPublicKeyInfo, the running example of docs/asn1.md. *)

type params = Null | Curve of Asn1.Oid.t | Absent
type alg = { oid : Asn1.Oid.t; params : params }
type spki = { alg : alg; key : int * string }

let rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 1 ])
let ec = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 2; 1 ])
let ed25519 = Asn1.Oid.(base 1 3 <|| [ 101; 112 ])
let f_oid = Asn1.required "algorithm" Asn1.oid

let f_params =
  Asn1.(
    defined_by "parameters" ~on:f_oid
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
      ])

let alg_id =
  Asn1.sequence "AlgorithmIdentifier"
    (fun oid params -> { oid; params })
    Codec.[ (f_oid $ fun a -> a.oid); (f_params $ fun a -> a.params) ]

let f_alg = Asn1.required "algorithm" alg_id
let f_key = Asn1.required "subjectPublicKey" Asn1.bit_string

let spki =
  Asn1.sequence "SubjectPublicKeyInfo"
    (fun alg key -> { alg; key })
    Codec.[ (f_alg $ fun s -> s.alg); (f_key $ fun s -> s.key) ]

let spki_codec = Asn1.codec spki

(* Certificate (RFC 5280 4.1). *)

type any_alg = { algorithm : Asn1.Oid.t; parameters : string option }
type atv = { attr : Asn1.Oid.t; value : string }
type time = Utc of string | Generalized of string
type validity = { from : time; until : time }
type extension = { extn_id : Asn1.Oid.t; critical : bool; extn_value : string }

type tbs = {
  version : string;
  serial : string;
  signature : any_alg;
  issuer : atv list list;
  validity : validity;
  subject : atv list list;
  subject_public_key_info : spki * string;
  issuer_unique_id : (int * string) option;
  subject_unique_id : (int * string) option;
  extensions : extension list option;
}

type certificate = {
  tbs_certificate : tbs * string;
  signature_algorithm : any_alg;
  signature_value : int * string;
}

(* The parameters of a signature algorithm: ANY DEFINED BY without a table,
   which everparse-asn1 reads as ANY (README:322-323). *)
let any_alg =
  let algorithm = Asn1.required "algorithm" Asn1.oid in
  let parameters = Asn1.optional "parameters" Asn1.any in
  Asn1.sequence "AlgorithmIdentifier"
    (fun algorithm parameters -> { algorithm; parameters })
    Codec.
      [
        (algorithm $ fun a -> a.algorithm); (parameters $ fun a -> a.parameters);
      ]

let atv =
  let attr = Asn1.required "type" Asn1.oid in
  let value = Asn1.required "value" Asn1.any in
  Asn1.sequence "AttributeTypeAndValue"
    (fun attr value -> { attr; value })
    Codec.[ (attr $ fun a -> a.attr); (value $ fun a -> a.value) ]

let name = Asn1.sequence_of (Asn1.set_of ~non_empty:true atv)

let time =
  Asn1.choice
    [
      Asn1.alternative "utcTime" Asn1.utc_time
        ~inject:(fun s -> Utc s)
        ~project:(function Utc s -> Some s | Generalized _ -> None);
      Asn1.alternative "generalTime" Asn1.generalized_time
        ~inject:(fun s -> Generalized s)
        ~project:(function Generalized s -> Some s | Utc _ -> None);
    ]

let validity =
  let from = Asn1.required "notBefore" time in
  let until = Asn1.required "notAfter" time in
  Asn1.sequence "Validity"
    (fun from until -> { from; until })
    Codec.[ (from $ fun v -> v.from); (until $ fun v -> v.until) ]

let extension =
  let extn_id = Asn1.required "extnID" Asn1.oid in
  let critical = Asn1.default "critical" false Asn1.bool in
  let extn_value = Asn1.required "extnValue" Asn1.octet_string in
  Asn1.sequence "Extension"
    (fun extn_id critical extn_value -> { extn_id; critical; extn_value })
    Codec.
      [
        (extn_id $ fun e -> e.extn_id);
        (critical $ fun e -> e.critical);
        (extn_value $ fun e -> e.extn_value);
      ]

let tbs =
  let version = Asn1.default "version" "\x00" (Asn1.explicit 0 Asn1.integer) in
  let serial = Asn1.required "serialNumber" Asn1.integer in
  let signature = Asn1.required "signature" any_alg in
  let issuer = Asn1.required "issuer" name in
  let validity = Asn1.required "validity" validity in
  let subject = Asn1.required "subject" name in
  let spki = Asn1.required "subjectPublicKeyInfo" (Asn1.keep_bytes spki) in
  let issuer_uid =
    Asn1.optional "issuerUniqueID" (Asn1.implicit 1 Asn1.bit_string)
  in
  let subject_uid =
    Asn1.optional "subjectUniqueID" (Asn1.implicit 2 Asn1.bit_string)
  in
  let extensions =
    Asn1.optional "extensions"
      (Asn1.explicit 3 (Asn1.sequence_of ~non_empty:true extension))
  in
  Asn1.sequence "TBSCertificate"
    (fun version serial signature issuer validity subject
         subject_public_key_info issuer_unique_id subject_unique_id extensions
       ->
      {
        version;
        serial;
        signature;
        issuer;
        validity;
        subject;
        subject_public_key_info;
        issuer_unique_id;
        subject_unique_id;
        extensions;
      })
    Codec.
      [
        (version $ fun t -> t.version);
        (serial $ fun t -> t.serial);
        (signature $ fun t -> t.signature);
        (issuer $ fun t -> t.issuer);
        (validity $ fun t -> t.validity);
        (subject $ fun t -> t.subject);
        (spki $ fun t -> t.subject_public_key_info);
        (issuer_uid $ fun t -> t.issuer_unique_id);
        (subject_uid $ fun t -> t.subject_unique_id);
        (extensions $ fun t -> t.extensions);
      ]

let certificate =
  let tbs = Asn1.required "tbsCertificate" (Asn1.keep_bytes tbs) in
  let signature_algorithm = Asn1.required "signatureAlgorithm" any_alg in
  let signature_value = Asn1.required "signatureValue" Asn1.bit_string in
  Asn1.sequence "Certificate"
    (fun tbs_certificate signature_algorithm signature_value ->
      { tbs_certificate; signature_algorithm; signature_value })
    Codec.
      [
        (tbs $ fun c -> c.tbs_certificate);
        (signature_algorithm $ fun c -> c.signature_algorithm);
        (signature_value $ fun c -> c.signature_value);
      ]

let certificate_codec = Asn1.codec certificate
