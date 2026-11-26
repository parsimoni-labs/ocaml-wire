(** Tests of the walk {!Wire_rust} writes from a plan: folds, arithmetic, enums,
    optional fields and decision trees. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the test suite. *)
