let () =
  Alcotest.run "bench"
    [
      Test_bench_lib.suite; Test_bench_verify.suite; Test_demo_bench_cases.suite;
    ]
