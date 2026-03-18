let suite =
  let _, types_tests = Test_types.suite in
  let _, codec_tests = Test_codec.suite in
  ("wire", types_tests @ codec_tests)
