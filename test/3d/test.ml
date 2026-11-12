let () =
  Alcotest.run "wire_3d"
    [
      Test_wire_3d.suite;
      Test_wire_3d_corpus.suite;
      Test_wire_3d_proc.suite;
      Test_wire_3d_shapes.suite;
    ]
