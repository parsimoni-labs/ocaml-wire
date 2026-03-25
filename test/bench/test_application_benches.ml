let () =
  Alcotest.run "application_benches"
    [
      ( "routing",
        [
          Alcotest.test_case "counts match" `Quick
            (Routing_bench.verify ~n_pkts:10_000);
        ] );
      ( "clcw",
        [
          Alcotest.test_case "anomalies match" `Quick
            (Clcw_bench.verify ~n_words:10_000);
        ] );
      ( "gateway",
        [
          Alcotest.test_case "checksums match" `Quick
            (Gateway_bench.verify ~n_frames:1_000);
        ] );
    ]
