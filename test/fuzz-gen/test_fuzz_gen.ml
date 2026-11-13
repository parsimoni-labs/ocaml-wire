(** Tests for Fuzz_gen -- the generators the fuzz executables hold. *)

(* A fuzz executable builds its test cases once, at start-up, and holds them
   for the whole run, so what a nested-composition case reaches is memory the
   run never gets back. Each depth adds one layer of combinators over the layer
   below, so each level should add the same footprint as the one before it. A
   level that adds more is rebuilt for every combinator that draws from it,
   which is exponential in the depth. *)
let live_words () =
  Gc.full_major ();
  (Gc.stat ()).live_words

(* The words that stay live while the cases [cases_of depth] are held. *)
let held cases_of depth =
  let before = live_words () in
  let cases = cases_of depth in
  let after = live_words () in
  ignore (Sys.opaque_identity cases);
  after - before

let check_linear name cases =
  let d2 = held cases 2 and d3 = held cases 3 and d4 = held cases 4 in
  let step = d3 - d2 and next = d4 - d3 in
  if next > 2 * step then
    Alcotest.failf
      "%s: depth 4 holds %d words more than depth 3, which holds %d more than \
       depth 2"
      name next step

let test_nested_cases () =
  check_linear "nested_cases" (Fuzz_gen.nested_cases "nested")

let test_everparse_nested_cases () =
  check_linear "everparse_nested_cases"
    (Fuzz_gen.everparse_nested_cases "nested")

let suite =
  ( "fuzz_gen",
    [
      Alcotest.test_case "nested_cases grows linearly with depth" `Quick
        test_nested_cases;
      Alcotest.test_case "everparse_nested_cases grows linearly with depth"
        `Quick test_everparse_nested_cases;
    ] )
