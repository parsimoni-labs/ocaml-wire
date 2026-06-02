(** Fuzz tests for the Wire library, driven entirely by {!Fuzz_gen}.

    Every codec in the canonical {!Fuzz_gen.registry} is exercised on the OCaml
    round-trip and crash-safety path ({!Fuzz_gen.test_cases}); the alternate
    entry points ([of_string] / [of_reader] / [validate]) are fuzzed over the
    registry ({!Fuzz_gen.entry_point_cases}); [Action.abort] is reject-only;
    {!Fuzz_gen.sized_cases} covers cross-field sizes and
    {!Fuzz_gen.nested_cases} arbitrary compositions no curated list enumerates.
    There are no hand-written per-type cases: the registry is the single source.
*)

open Fuzz_gen

(* Positive round-trip plus random / adversarial crash-safety for every
   combinator in the registry. *)
let registry_tests =
  List.concat_map (fun (name, Pack g) -> test_cases name g) registry

(* The alternate entry points, fuzzed over the registry: each sample picks a
   random gen rather than pinning a fixed subset. *)
let entry_point_tests = entry_point_cases "entry"

let nested_tests =
  nested_cases "nested(d2)" 2
  @ nested_cases "nested(d3)" 3
  @ nested_cases "nested(d4)" 4

let suite =
  ( "wire",
    registry_tests
    @ reject_cases "action_abort" action_abort
    @ sized_cases "sized" @ nested_tests @ entry_point_tests )
