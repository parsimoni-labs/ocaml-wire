(** The scan signature tests: the generated scanner and the interpreted codec
    run the same consumer through {!Wire.Scan}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the scan signature test suite. *)
