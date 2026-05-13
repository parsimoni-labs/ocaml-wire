let () =
  Alcotest.run "wire"
    [
      Test_wire.suite;
      Test_action.suite;
      Test_codec.suite;
      Test_everparse.suite;
      Test_param.suite;
      Test_ascii.suite;
      Test_staged.suite;
      Test_uint32.suite;
      Test_uint63.suite;
      Test_types.suite;
      Test_eval.suite;
      Test_bitfield.suite;
      Test_field.suite;
      Test_uint_var.suite;
    ]
