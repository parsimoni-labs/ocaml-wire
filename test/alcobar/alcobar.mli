(** Thin wrapper over upstream Crowbar providing an Alcotest-style suite API. *)

include module type of Crowbar

type test_case

val test_case : string -> ('f, unit) gens -> 'f -> test_case
(** [test_case name gens f] creates a named fuzz test case. *)

val run : string -> (string * test_case list) list -> unit
(** [run name suites] registers all test cases and runs them. *)
