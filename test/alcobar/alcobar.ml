(** Thin wrapper over upstream Crowbar providing an Alcotest-style suite API. *)

include Crowbar

type test_case =
  | TC : { name : string; gens : ('f, unit) gens; f : 'f } -> test_case

let test_case name gens f = TC { name; gens; f }

let run _name suites =
  List.iter
    (fun (_suite_name, tcs) ->
      List.iter (fun (TC { name; gens; f }) -> add_test ~name gens f) tcs)
    suites
