module Field = Wire__Field
module Types = Wire__Types

let test_v_creates_named_field () =
  let f = Field.v "Version" Types.uint8 in
  Alcotest.(check string) "name" "Version" (Field.name f)

let test_name_returns_name () =
  let f = Field.v "Length" Types.uint16be in
  Alcotest.(check string) "name" "Length" (Field.name f)

let test_anon () =
  let _a = Field.anon Types.uint8 in
  ()

let test_ref_returns_ref_expr () =
  let f = Field.v "Tag" Types.uint8 in
  match Field.ref f with
  | Types.Ref name -> Alcotest.(check string) "ref name" "Tag" name
  | _ -> Alcotest.fail "expected Ref expression"

let test_pp_prints_name () =
  let f = Field.v "MyField" Types.uint8 in
  let s = Fmt.str "%a" Field.pp f in
  Alcotest.(check string) "pp" "MyField" s

let test_to_decl_named () =
  let f = Field.v "Flags" Types.uint8 in
  let packed = Field.Named f in
  match Field.decl_of_packed packed with
  | Types.Field { field_name = Some name; _ } ->
      Alcotest.(check string) "decl name" "Flags" name
  | _ -> Alcotest.fail "expected named field declaration"

let test_to_decl_anon () =
  let a = Field.anon Types.uint8 in
  let packed = Field.Anon a in
  match Field.decl_of_packed packed with
  | Types.Field { field_name = None; _ } -> ()
  | _ -> Alcotest.fail "expected anonymous field declaration"

let test_typ () =
  let f = Field.v "X" Types.uint8 in
  let _t = Field.typ f in
  ()

let test_constraint () =
  let f = Field.v "Y" ~constraint_:(Types.Bool true) Types.uint8 in
  match Field.constraint_ f with
  | Some _ -> ()
  | None -> Alcotest.fail "expected constraint"

let test_no_constraint () =
  let f = Field.v "Z" Types.uint8 in
  match Field.constraint_ f with
  | None -> ()
  | Some _ -> Alcotest.fail "expected no constraint"

(* ref on non-int fields: bool, mapped, uint64. The expression is always
   Ref name -- the type system no longer restricts the field's OCaml type. *)

let test_ref_bool_field () =
  let f = Field.v "Flag" (Types.bool Types.uint8) in
  match Field.ref f with
  | Types.Ref name -> Alcotest.(check string) "ref name" "Flag" name
  | _ -> Alcotest.fail "expected Ref"

let test_ref_mapped_field () =
  let f =
    Field.v "Code"
      (Types.map
         (fun n -> string_of_int n)
         (fun s -> int_of_string s)
         Types.uint8)
  in
  match Field.ref f with
  | Types.Ref name -> Alcotest.(check string) "ref name" "Code" name
  | _ -> Alcotest.fail "expected Ref"

let test_ref_uint64_field () =
  let f = Field.v "Timestamp" (Types.Uint64 Types.Big) in
  match Field.ref f with
  | Types.Ref name -> Alcotest.(check string) "ref name" "Timestamp" name
  | _ -> Alcotest.fail "expected Ref"

let suite =
  ( "field",
    [
      Alcotest.test_case "v creates named field" `Quick
        test_v_creates_named_field;
      Alcotest.test_case "name returns name" `Quick test_name_returns_name;
      Alcotest.test_case "anon" `Quick test_anon;
      Alcotest.test_case "ref returns Ref expr" `Quick test_ref_returns_ref_expr;
      Alcotest.test_case "ref on bool field" `Quick test_ref_bool_field;
      Alcotest.test_case "ref on mapped field" `Quick test_ref_mapped_field;
      Alcotest.test_case "ref on uint64 field" `Quick test_ref_uint64_field;
      Alcotest.test_case "pp prints name" `Quick test_pp_prints_name;
      Alcotest.test_case "to_decl named" `Quick test_to_decl_named;
      Alcotest.test_case "to_decl anon" `Quick test_to_decl_anon;
      Alcotest.test_case "typ" `Quick test_typ;
      Alcotest.test_case "constraint present" `Quick test_constraint;
      Alcotest.test_case "no constraint" `Quick test_no_constraint;
    ] )
