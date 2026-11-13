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

(* Generated code raises through wire.runtime, without wire: a codec's caller
   must catch what it raises as the codec's own error, with the same value. *)
let test_runtime_errors () =
  let e =
    {
      Wire.at = 3;
      field = [ "TTL" ];
      kind = Constraint_failed { which = Field; value = Some 0L };
    }
  in
  match Wire_runtime.Errors.field_constraint ~at:3 ~field:"TTL" 0 with
  | _ -> Alcotest.fail "no exception raised"
  | exception Wire.Parse_error got ->
      Alcotest.(check bool) "same error" true (Wire.equal_parse_error e got)

let suite =
  ( "types",
    [
      Alcotest.test_case "runtime errors are Wire's" `Quick test_runtime_errors;
      Alcotest.test_case "field_wire_size" `Quick test_field_wire_size;
      Alcotest.test_case "struct_name" `Quick test_struct_name;
    ] )
