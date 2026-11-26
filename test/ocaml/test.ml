let () =
  Alcotest.run "wire_ocaml"
    [
      Test_wire_ocaml.suite;
      Test_get.suite;
      Test_view_module.suite;
      Test_writer.suite;
      Test_value_module.suite;
      Test_same.suite;
      Test_scan.suite;
    ]
