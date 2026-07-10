(* Tests for Eval: expression evaluator and value-to-int conversion.

   The historical [bind]/[get]/[action]/[set_pos] tests are gone -- the
   String-Map context machinery they exercised was retired when struct
   decoding moved to the [Codec.validator_of_struct] int-array kernel. *)

open Wire.Private

let test_int_of () =
  Alcotest.(check (option int)) "uint8" (Some 7) (Eval.int_of Types.uint8 7);
  Alcotest.(check (option int))
    "uint16be" (Some 300)
    (Eval.int_of Types.uint16be 300);
  Alcotest.(check (option int))
    "uint64 small" (Some 42)
    (Eval.int_of Types.uint64be 42L);
  Alcotest.(check (option int))
    "uint64 overflow" None
    (Eval.int_of Types.uint64be 0xFFFF_FFFF_FFFF_FFFFL);
  Alcotest.(check (option int)) "unit" None (Eval.int_of Types.Unit ())

(* [int_of_exn] returns the same value as [int_of] on the representable cases.
   Where [int_of] returns [None] it does not silently coerce to 0: an
   out-of-range 64-bit value is adversarial input and raises [Parse_error],
   while a non-integer type is a schema mistake and raises [Invalid_argument]. *)
let raises_constraint name f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Parse_error, got a value" name
  | exception Types.Parse_error (Types.Constraint_failed _) -> ()
  | exception e ->
      Alcotest.failf "%s: expected Parse_error, got %s" name
        (Printexc.to_string e)

let raises_invalid_arg name f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Invalid_argument, got a value" name
  | exception Invalid_argument _ -> ()
  | exception e ->
      Alcotest.failf "%s: expected Invalid_argument, got %s" name
        (Printexc.to_string e)

let test_int_of_exn_ok () =
  Alcotest.(check int) "uint8" 7 (Eval.int_of_exn Types.uint8 7);
  Alcotest.(check int) "uint16be" 300 (Eval.int_of_exn Types.uint16be 300);
  Alcotest.(check int) "uint64 small" 42 (Eval.int_of_exn Types.uint64be 42L);
  Alcotest.(check int)
    "int64 small" 100
    (Eval.int_of_exn Types.(Int64 Big) 100L);
  (* [max_int] is the largest value [Int64.unsigned_to_int] accepts. *)
  Alcotest.(check int)
    "uint64 = max_int" max_int
    (Eval.int_of_exn Types.uint64be (Int64.of_int max_int))

let test_int_of_exn_overflow () =
  (* Adversarial 64-bit lengths that do not fit a native int must fail the
     parse rather than be read as 0. *)
  raises_constraint "uint64 all-ones" (fun () ->
      Eval.int_of_exn Types.uint64be 0xFFFF_FFFF_FFFF_FFFFL);
  raises_constraint "uint64 = 2^63" (fun () ->
      Eval.int_of_exn Types.uint64be 0x8000_0000_0000_0000L);
  raises_constraint "uint64 = max_int + 1" (fun () ->
      Eval.int_of_exn Types.uint64be (Int64.add (Int64.of_int max_int) 1L));
  (* A signed int64 with the top bit set is a huge unsigned value. *)
  raises_constraint "int64 = -1" (fun () ->
      Eval.int_of_exn Types.(Int64 Big) (-1L));
  raises_constraint "int64 = min_int" (fun () ->
      Eval.int_of_exn Types.(Int64 Big) Int64.min_int)

let test_int_of_exn_non_integer () =
  raises_invalid_arg "unit" (fun () -> Eval.int_of_exn Types.Unit ());
  raises_invalid_arg "float64" (fun () -> Eval.int_of_exn Types.float64be 1.5);
  raises_invalid_arg "byte_array" (fun () ->
      Eval.int_of_exn (Types.byte_array ~size:(Types.Int 4)) "abcd")

let test_expr_const () =
  let ctx = Eval.empty in
  Alcotest.(check int) "int" 42 (Eval.expr ctx (Types.Int 42));
  Alcotest.(check int)
    "add" 30
    (Eval.expr ctx (Types.Add (Types.Int 10, Types.Int 20)));
  Alcotest.(check bool)
    "lt" true
    (Eval.expr ctx (Types.Lt (Types.Int 1, Types.Int 2)))

let test_expr_ref_fails () =
  Alcotest.check_raises "Ref at top level should fail"
    (Failure
       "Eval.expr: unbound field x (cross-field references are only valid \
        inside a struct)") (fun () ->
      ignore (Eval.expr Eval.empty (Types.ref "x")))

let test_cast_u8 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U8, Types.Int 0x1234)) in
  Alcotest.(check int) "cast U8" 0x34 v

let test_cast_u16 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U16, Types.Int 0x12345678)) in
  Alcotest.(check int) "cast U16" 0x5678 v

let test_cast_u32 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U32, Types.Int 0x123456789)) in
  Alcotest.(check int) "cast U32" 0x23456789 v

let test_cast_u64 () =
  let v = Eval.expr Eval.empty (Types.Cast (`U64, Types.Int 42)) in
  Alcotest.(check int) "cast U64 (identity)" 42 v

let test_cast_negative () =
  let v = Eval.expr Eval.empty (Types.Cast (`U8, Types.Int (-1))) in
  Alcotest.(check int) "cast U8 of -1" 0xFF v

let suite =
  ( "eval",
    [
      Alcotest.test_case "int_of" `Quick test_int_of;
      Alcotest.test_case "int_of_exn representable" `Quick test_int_of_exn_ok;
      Alcotest.test_case "int_of_exn overflow raises" `Quick
        test_int_of_exn_overflow;
      Alcotest.test_case "int_of_exn non-integer raises" `Quick
        test_int_of_exn_non_integer;
      Alcotest.test_case "expr constants" `Quick test_expr_const;
      Alcotest.test_case "expr Ref fails" `Quick test_expr_ref_fails;
      Alcotest.test_case "cast U8" `Quick test_cast_u8;
      Alcotest.test_case "cast U16" `Quick test_cast_u16;
      Alcotest.test_case "cast U32" `Quick test_cast_u32;
      Alcotest.test_case "cast U64" `Quick test_cast_u64;
      Alcotest.test_case "cast negative" `Quick test_cast_negative;
    ] )
