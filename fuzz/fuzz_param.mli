(** Focused fuzz tests for parameterised descriptions.

    Broad validation is folded into {!Fuzz_gen.test_cases}; this module keeps a
    small env-bearing validation suite for parameter-specific regressions. *)

val suite : string * Alcobar.test_case list
(** [suite] validates registry codecs that bind a {!Wire.Param.env}. *)
