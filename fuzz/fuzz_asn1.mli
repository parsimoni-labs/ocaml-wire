(** Fuzz tests for {!Wire.Asn1}. *)

val suite : string * Alcobar.test_case list
(** [suite] is the fuzz test suite for the DER codec. *)
