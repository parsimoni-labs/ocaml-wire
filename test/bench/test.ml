let () =
  Alcotest.run "bench"
    [
      Test_bench_lib.suite;
      Test_application_benches.suite;
      Test_demo_bench_cases.suite;
    ]
