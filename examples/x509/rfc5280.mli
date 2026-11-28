(** RFC 5280 X.509 certificates as {!Wire.Asn1} types.

    [spki] is the running example of docs/asn1.md, its parameters keyed by the
    algorithm; [certificate] reads a whole certificate, its signature parameters
    as ANY, as RFC 5280 4.1 declares them. *)

open Wire

(** {1:spki SubjectPublicKeyInfo} *)

(** The parameters of a public key algorithm, by its object identifier. *)
type params = Null | Curve of Asn1.Oid.t | Absent

type alg = { oid : Asn1.Oid.t; params : params }
(** The type for AlgorithmIdentifier. *)

type spki = { alg : alg; key : int * string }
(** The type for SubjectPublicKeyInfo. *)

val rsa : Asn1.Oid.t
(** [rsa] is rsaEncryption, whose parameters are NULL. *)

val ec : Asn1.Oid.t
(** [ec] is id-ecPublicKey, whose parameters are a named curve. *)

val ed25519 : Asn1.Oid.t
(** [ed25519] is id-Ed25519, which has no parameters. *)

val spki : spki Asn1.t
(** [spki] is SubjectPublicKeyInfo. *)

val spki_codec : spki Codec.t
(** [spki_codec] is [Asn1.codec spki]. *)

(** {1:certificate Certificate} *)

type any_alg = { algorithm : Asn1.Oid.t; parameters : string option }
(** The type for AlgorithmIdentifier with its parameters as ANY. *)

type atv = { attr : Asn1.Oid.t; value : string }
(** The type for AttributeTypeAndValue. *)

(** The type for Time. *)
type time = Utc of string | Generalized of string

type validity = { from : time; until : time }
(** The type for Validity: notBefore and notAfter. *)

type extension = { extn_id : Asn1.Oid.t; critical : bool; extn_value : string }
(** The type for Extension. *)

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
(** The type for TBSCertificate, its SubjectPublicKeyInfo with its bytes. *)

type certificate = {
  tbs_certificate : tbs * string;
  signature_algorithm : any_alg;
  signature_value : int * string;
}
(** The type for Certificate, its TBSCertificate with the bytes it signs. *)

val certificate : certificate Asn1.t
(** [certificate] is Certificate. *)

val certificate_codec : certificate Codec.t
(** [certificate_codec] is [Asn1.codec certificate]. *)
