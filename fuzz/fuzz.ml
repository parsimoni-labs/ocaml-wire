let () =
  Alcobar.run "wire" [ Fuzz_wire.suite; Fuzz_everparse.suite; Fuzz_param.suite ]
