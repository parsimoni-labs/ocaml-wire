let () =
  Alcotest.run "wire_ocaml"
    [ Test_wire_ocaml.suite; Test_view_module.suite; Test_same.suite ]
