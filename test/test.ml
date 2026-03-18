let () =
  Alcotest.run "wire"
    [
      Test_wire.suite;
      Test_action.suite;
      Test_codec.suite;
      Test_c.suite;
      Test_param.suite;
      Test_ascii.suite;
      Test_staged.suite;
      Test_uInt32.suite;
      Test_uInt63.suite;
      Test_types.suite;
    ]
