let () =
  Alcotest.run "wire_rust"
    [ Test_wire_rust.suite; Test_rust_module.suite; Test_walk.suite ]
