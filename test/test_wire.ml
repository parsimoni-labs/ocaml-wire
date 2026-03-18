let suite =
  let _, types_tests = Test_types.suite in
  let _, codec_tests = Test_codec.suite in
  let _, action_tests = Test_action.suite in
  ("wire", types_tests @ codec_tests @ action_tests)
