let routing () = Routing_bench.check ~n_pkts:10_000
let clcw () = Clcw_bench.check ~n_words:10_000
let gateway () = Gateway_bench.check ~n_frames:1_000

let () =
  Alcotest.run "bench"
    [
      ( "bench",
        [
          Alcotest.test_case "verify: routing counts" `Quick routing;
          Alcotest.test_case "verify: clcw anomalies" `Quick clcw;
          Alcotest.test_case "verify: gateway checksums" `Quick gateway;
        ] );
    ]
