let oracle label =
  if C_oracle.enabled then Some (C_oracle.oracle label) else None

let report_oracle name =
  if Sys.getenv_opt "BUILD_EVERPARSE" = Some "1" && not C_oracle.enabled then
    Fuzz_gen.failf "%s: required EverParse C oracle is absent" name;
  Fmt.epr "%s: structured oracle: %s@." name
    (if C_oracle.enabled then
       "EverParse C, verified .3d (acceptance and consumed prefix of the \
        standalone and FFI projections, plug scalars)"
     else "C skipped (set BUILD_EVERPARSE=1)")

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
