(** The F* a scanner is proved in against the OCaml that runs. *)

val suite : string * unit Alcotest.test_case list
(** The F* each accepted codec's scanner is proved in and the OCaml it runs, one
    tree up to the token table; and three mutants of the OCaml that must not be:
    a literal changed, an operator changed, two labels swapped. *)
