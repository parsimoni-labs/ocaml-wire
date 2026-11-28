(** CBOR types whose [.cddl] modules the tests print and evercddl reads. *)

open Wire

(** The type for CBOR types with their values erased. *)
type packed = Packed : 'a Cbor.t -> packed

val cose_sign1 : packed
(** [cose_sign1] is the running example of docs/cbor.md, RFC 9052 COSE_Sign1
    under its tag. *)

val cose_key : packed
(** [cose_key] is EverParse's [cose-sign.cddl] COSE_Key_OKP. *)

val containing : packed
(** [containing] holds {!payload_codec} and a binary integer under a
    [containing]. *)

val clash : packed
(** [clash] has two tuples of one name, a record named after a prelude type, and
    names to sanitise. *)

val payload_codec : UInt16.t Codec.t
(** [payload_codec] is the binary codec {!val-containing} holds. *)

type framed
(** The type for records of {!val-framed}. *)

val framed : framed Codec.t
(** [framed] is a binary record holding {!val-cose_sign1}'s CBOR under its
    length. *)

type enveloped
(** The type for records of {!val-enveloped}. *)

val enveloped : enveloped Codec.t
(** [enveloped] is a DER SEQUENCE holding a CBOR tuple in an OCTET STRING, the
    tuple holding {!payload_codec} in a byte string. *)

val types : (string * packed) list
(** [types] is each type with the name of its module: {!val-cose_sign1},
    {!val-cose_key}, and one module per row of docs/cbor.md's table. *)

val project : string -> packed -> Cbor.schema
(** [project m t] is the module [m] of the codec of [t]. *)

val all : Cbor.schema list
(** [all] is the module of each of {!types}, and of {!val-framed} and
    {!val-enveloped}. *)
