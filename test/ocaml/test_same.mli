(** The swap tests: a generated module and its interpreted twin answer alike,
    and code written against one runs on the other by changing one line. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the swap test suite. *)
