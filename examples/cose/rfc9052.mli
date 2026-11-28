(** RFC 9052 COSE structures as {!Wire.Cbor} types.

    [sign1] is the running example of docs/cbor.md, COSE_Sign1 (RFC 9052 section
    4.2); [key] is COSE_Key as EverParse's [src/cose/cose-sign.cddl] declares
    it, the OKP key type of RFC 9053 section 7.2. Both read deterministic CBOR
    only. *)

open Wire

(** {1:headers Headers} *)

(** The type for labels: an integer or a text string. *)
type label = Int of Cbor.integer | Text of string

(** The type for the initialisation vector of a header: full or partial, never
    both (RFC 9052 section 3.1). *)
type iv = Iv of string | Partial_iv of string

type header = {
  alg : label option;
  crit : label list option;
  content_type : label option;
  kid : string option;
  iv : iv option;
  others : (label * string) list;
}
(** The type for header maps (RFC 9052 section 3): the generic headers, and
    every other entry with its value as the bytes of one data item. *)

val label : label Cbor.t
(** [label] is [int / tstr]. *)

val header_map : header Cbor.t
(** [header_map] is the header map of RFC 9052 section 3. *)

(** {1:sign1 COSE_Sign1} *)

type sign1 = {
  protected : (header * string) option;
  unprotected : header;
  payload : string option;
  signature : string;
}
(** The type for COSE_Sign1 messages. [protected] is [None] for an empty
    protected bucket, else the header and the bytes of the byte string that
    carries it. [payload] is [None] for a detached payload. *)

val sign1 : sign1 Cbor.t
(** [sign1] is COSE_Sign1, untagged. *)

val sign1_codec : sign1 Codec.t
(** [sign1_codec] is COSE_Sign1_Tagged, [Cbor.codec (Cbor.tag 18 sign1)]. *)

(** {1:key COSE_Key} *)

type key = {
  crv : label;
  x : string option;
  d : string option;
  key_others : (label * string) list;
}
(** The type for OKP keys (kty [1]): the curve, the public key, the private key,
    and every other entry. *)

val key : key Cbor.t
(** [key] is COSE_Key_OKP. *)

val key_codec : key Codec.t
(** [key_codec] is [Cbor.codec key]. *)
