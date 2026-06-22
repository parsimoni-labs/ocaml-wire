(** Fuzz tests for the Wire library, driven entirely by {!Fuzz_gen}.

    Every codec in the canonical {!Fuzz_gen.registry} is exercised on the OCaml
    codec round-trip, validation, and crash-safety paths
    ({!Fuzz_gen.test_cases}); direct entry points are sampled from that same
    registry ({!Fuzz_gen.entry_point_cases}); [Action.abort] is reject-only;
    {!Fuzz_gen.sized_cases} covers cross-field sizes, {!Fuzz_gen.nested_cases}
    covers arbitrary compositions no curated list enumerates, and
    {!Fuzz_gen.invariant_cases} keeps the shared generator vocabulary aligned
    with the Wire API. There are no hand-written per-type cases outside that
    infrastructure: the registry is the single source. *)

open Fuzz_gen

let file_input_mode () =
  let argv = Sys.argv in
  let n = Array.length argv in
  n > 1
  && (not (Array.exists (String.equal "--gen-corpus") argv))
  &&
  let path = argv.(n - 1) in
  Sys.file_exists path
  && try not (Sys.is_directory path) with Sys_error _ -> false

(* Positive round-trip plus random / adversarial crash-safety for every
   combinator in the registry. *)
let registry_tests () =
  List.concat_map (fun (name, Pack g) -> test_cases name g) registry

let nested_tests () =
  nested_cases "nested(d2)" 2
  @ nested_cases "nested(d3)" 3
  @ nested_cases "nested(d4)" 4

let entry_point_tests () = entry_point_cases "entry"
let api_tests () = api_cases "api"

let suite =
  if file_input_mode () then ("wire", afl_cases "wire")
  else
    ( "wire",
      registry_tests ()
      @ reject_cases "action_abort" action_abort
      @ sized_cases "sized" @ nested_tests () @ entry_point_tests ()
      @ api_tests ()
      @ invariant_cases "invariants" )
