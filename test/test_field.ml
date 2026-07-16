module Field = Wire.Field
module Types = Wire.Private.Types

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
  | Types.Ref (Types.I, name) -> Alcotest.(check string) "ref name" "Tag" name
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
   Ref (I, name) -- the type system no longer restricts the field's OCaml
   type. *)

let test_ref_bool_field () =
  let f = Field.v "Flag" (Types.bool Types.uint8) in
  match Field.ref f with
  | Types.Ref (Types.I, name) -> Alcotest.(check string) "ref name" "Flag" name
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
  | Types.Ref (Types.I, name) -> Alcotest.(check string) "ref name" "Code" name
  | _ -> Alcotest.fail "expected Ref"

let test_ref_uint64_field () =
  let f = Field.v "Timestamp" (Types.Uint64 Types.Big) in
  match Field.ref f with
  | Types.Ref (Types.I, name) ->
      Alcotest.(check string) "ref name" "Timestamp" name
  | _ -> Alcotest.fail "expected Ref"

let test_int64_uint64_field () =
  let f = Field.v "Timestamp" (Types.Uint64 Types.Big) in
  match Field.int64 f with
  | Types.Ref (Types.I64, name) ->
      Alcotest.(check string) "ref name" "Timestamp" name
  | _ -> Alcotest.fail "expected int64 Ref"

let test_int64_rejects_field_without_int64_slot () =
  let check_invalid label f =
    match f () with
    | () -> Alcotest.failf "%s: expected Invalid_argument" label
    | exception Invalid_argument _ -> ()
  in
  let mapped =
    Field.v "Mapped" (Types.map Int64.of_int Int64.to_int Types.uint8)
  in
  check_invalid "mapped field" (fun () ->
      ignore (Field.int64 mapped : int64 Types.expr));
  check_invalid "self_int64 on uint8" (fun () ->
      ignore
        (Field.v "Tiny" Types.uint8 ~self_int64:(fun self ->
             Types.Expr.(self = int64 0L))
          : int Field.t))

let test_int64_accepts_map_over_uint64 () =
  (* A map over uint64 keeps its int64 slot: [build_populate] fills it with the
     raw pre-map wire value, so an int64 self-constraint over the raw word is
     well defined and is accepted (unlike a map over uint8). *)
  let mapped = Types.map Int64.neg Int64.neg Types.uint64be in
  ignore
    (Field.v "Seek" mapped ~self_int64:(fun self ->
         Types.Expr.(self <= int64 0L))
      : int64 Field.t);
  ignore (Field.int64 (Field.v "Seek2" mapped) : int64 Types.expr)

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
      Alcotest.test_case "int64 on uint64 field" `Quick test_int64_uint64_field;
      Alcotest.test_case "int64 rejects missing slot" `Quick
        test_int64_rejects_field_without_int64_slot;
      Alcotest.test_case "int64 accepts map over uint64" `Quick
        test_int64_accepts_map_over_uint64;
      Alcotest.test_case "pp prints name" `Quick test_pp_prints_name;
      Alcotest.test_case "to_decl named" `Quick test_to_decl_named;
      Alcotest.test_case "to_decl anon" `Quick test_to_decl_anon;
      Alcotest.test_case "typ" `Quick test_typ;
      Alcotest.test_case "constraint present" `Quick test_constraint;
      Alcotest.test_case "no constraint" `Quick test_no_constraint;
    ] )
