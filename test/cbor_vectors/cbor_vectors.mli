(** Deterministic CBOR test vectors. *)

val appendix_a :
  (string * [ `Accept | `Float of int | `Indefinite of int ] * string) list
(** [appendix_a] is RFC 8949 Appendix A: the diagnostic notation, the verdict of
    a deterministic reader with the offset of a refusal, and the hex. *)

val evercbor :
  (string * [ `Accept | `Prefix of int | `Refuse | `Refuse_text ] * string) list
(** [evercbor] is EverCBOR's conformance vectors under its deterministic API:
    the test name, the verdict, and the hex. *)
