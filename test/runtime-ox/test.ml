let () =
  Alcotest.run "wire.runtime OxCaml" [ Test_target.suite; Test_vec.suite ]
