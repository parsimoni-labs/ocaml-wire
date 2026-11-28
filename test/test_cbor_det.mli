(** Tests for the deterministic CBOR core: RFC 8949 Appendix A, EverCBOR's
    vectors, and the rules each refusal names. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the test suite for Cbor_det. *)
