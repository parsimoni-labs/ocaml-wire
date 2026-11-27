(** ASN.1 types whose [.asn] modules the tests print and [make asn1] hands to
    everparse-asn1. *)

open Wire

(** The type for ASN.1 types with their values erased. *)
type packed = Packed : 'a Asn1.t -> packed

val spki : packed
(** [spki] is the running example of docs/asn1.md, RFC 5280
    SubjectPublicKeyInfo. *)

val constructs : packed
(** [constructs] uses every construct of docs/asn1.md's table. *)

val payload_codec : UInt16.t Codec.t
(** [payload_codec] is the binary codec {!val-constructs} holds under a
    [containing]. *)

val names : packed
(** [names] is a root that is not a SEQUENCE or SET. *)

val kept_root : packed
(** [kept_root] is a root that keeps its bytes. *)

val clash : packed
(** [clash] has two SEQUENCEs of one name and names to sanitise. *)

val x509lite : packed
(** [x509lite] describes EverParse's
    [share/everparse/tests/asn1/tool/X509Lite.asn]. *)

val types : (string * packed) list
(** [types] is each type with the name of its module. *)

val all : Asn1.schema list
(** [all] is the module of each of {!types}. *)
