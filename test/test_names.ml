module Names = Wire.Private.Names

let clash = Alcotest.(option (triple string string string))

let test_clash () =
  Alcotest.check clash "distinct" None (Names.clash [ ("a", "a"); ("b", "b") ]);
  Alcotest.check clash "one source, several identifiers" None
    (Names.clash [ ("a", "get_a"); ("a", "read_a"); ("a", "get_a") ]);
  Alcotest.check clash "the first shared identifier"
    (Some ("x", "X", "x"))
    (Names.clash [ ("x", "x"); ("y", "y"); ("X", "x"); ("Y", "y") ])

let test_escape () =
  Alcotest.(check string) "keyword" "int_" (Names.escape Names.c_keywords "int");
  Alcotest.(check string) "name" "end" (Names.escape Names.c_keywords "end");
  Alcotest.(check string)
    "already escaped" "int__"
    (Names.escape [ "int_" ] "int_")

let test_ocaml_label () =
  let check name want =
    Alcotest.(check string) name want (Names.ocaml_label name)
  in
  check "Length" "length";
  check "done" "done_";
  check "effect" "effect_";
  check "local_" "local__";
  check "a-b" "a_b";
  check "9lives" "f_9lives";
  Alcotest.(check string)
    "stock compiler only" "local_"
    (Names.ocaml_label ~oxcaml:false "local_")

let suite =
  ( "names",
    [
      Alcotest.test_case "clash" `Quick test_clash;
      Alcotest.test_case "escape" `Quick test_escape;
      Alcotest.test_case "ocaml_label" `Quick test_ocaml_label;
    ] )
