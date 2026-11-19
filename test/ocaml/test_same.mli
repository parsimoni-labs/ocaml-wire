(** The signature tests: a generated module and the interpreted codec run the
    same checks through {!Wire.S}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the signature test suite. *)
