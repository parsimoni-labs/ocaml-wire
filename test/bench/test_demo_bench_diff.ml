let () =
  Alcotest.run "demo_bench_diff"
    [
      ( "projection diffs",
        List.map
          (fun (c : Demo_bench_diff.case) ->
            Alcotest.test_case c.label `Quick c.verify)
          Demo_bench_diff.cases );
    ]
