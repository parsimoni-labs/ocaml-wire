(** The scan swap tests: the generated module and its interpreted twin fold the
    same visitor alike. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the scan swap test suite. *)
