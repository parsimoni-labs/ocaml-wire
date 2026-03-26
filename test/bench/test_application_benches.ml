let suite =
  ( "application",
    [
      Alcotest.test_case "routing counts" `Quick
        (Routing_bench.verify ~n_pkts:10_000);
      Alcotest.test_case "clcw anomalies" `Quick
        (Clcw_bench.verify ~n_words:10_000);
      Alcotest.test_case "gateway checksums" `Quick
        (Gateway_bench.verify ~n_frames:1_000);
    ] )
