open Fuzz_gen

let run name cases =
  List.iter
    (fun (label, _, reason) ->
      Fmt.epr "%s: skipping %s (%s)@." name label
        (Diff_codecs.string_of_exclusion reason))
    Diff_codecs.excluded;
  let suite =
    if afl_mode () then
      afl_contract_cases ~max_len:256 name cases ~check:(fun _ check input ->
          check input.payload)
    else
      List.map
        (fun (label, check) ->
          Alcobar.test_case label [ Alcobar.bytes ] (fun buf ->
              check (Bytes.of_string buf)))
        cases
  in
  Alcobar.run name [ (name, suite) ]
