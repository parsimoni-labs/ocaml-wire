(* Tests for Types: internal representation, exercised through Wire.Private. *)

open Wire.Private

let test_field_wire_size () =
  Alcotest.(check (option int))
    "uint8" (Some 1)
    (Types.field_wire_size Types.uint8);
  Alcotest.(check (option int))
    "uint16 be" (Some 2)
    (Types.field_wire_size Types.uint16be);
  Alcotest.(check (option int))
    "uint32 be" (Some 4)
    (Types.field_wire_size Types.uint32be);
  Alcotest.(check (option int))
    "unit" (Some 0)
    (Types.field_wire_size Types.unit)

let test_struct_name () =
  let s = Types.struct_ "Foo" [ Types.field "x" Types.uint8 ] in
  Alcotest.(check string) "name" "Foo" (Types.struct_name s)

let suite =
  ( "types",
    [
      Alcotest.test_case "field_wire_size" `Quick test_field_wire_size;
      Alcotest.test_case "struct_name" `Quick test_struct_name;
    ] )
