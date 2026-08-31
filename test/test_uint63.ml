(* Tests for UInt63, the carrier [uint size] decodes into. Widths are built
   from [Int64] literals and compared through [Optint.Int63.to_int64]: a
   7-byte maximum written as an int literal would itself truncate on a
   narrow-int target (wasm_of_ocaml, js_of_ocaml), and these tests run there
   too. *)

open Wire.Private

let of_int64 = Optint.Int63.of_int64
let sizes = [ 1; 2; 3; 4; 5; 6; 7 ]

let refuses name ~size v =
  match UInt63.check_encode ~size v with
  | () -> Alcotest.failf "%s: expected Invalid_argument" name
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (name ^ ": message names the field width")
        true
        (String.ends_with
           ~suffix:(Fmt.str "does not fit an unsigned %d-byte field" size)
           msg)

let test_of_int_identity () =
  Alcotest.(check int) "identity" 42 (UInt63.to_int (UInt63.of_int 42))

(* The carrier is wider than every width it serves -- seven bytes need 56 bits,
   the carrier holds 62 -- so the widest value of each width passes the encode
   guard untouched. The first value past it is refused rather than truncated to
   a legal smaller number the wire cannot be told apart from. *)
let test_check_encode_widths () =
  List.iter
    (fun size ->
      let widest = Int64.sub (Int64.shift_left 1L (8 * size)) 1L in
      UInt63.check_encode ~size (of_int64 widest);
      refuses
        (Fmt.str "uint(%d) <- 0x%Lx" size (Int64.add widest 1L))
        ~size
        (of_int64 (Int64.add widest 1L)))
    sizes

(* A negative value would reach the wire as its low bytes, a legal positive
   number of that width. *)
let test_check_encode_rejects_negative () =
  List.iter
    (fun size ->
      refuses (Fmt.str "uint(%d) <- -1" size) ~size (UInt63.of_int (-1)))
    sizes

let suite =
  ( "uint63",
    [
      Alcotest.test_case "of_int identity" `Quick test_of_int_identity;
      Alcotest.test_case "check_encode widths" `Quick test_check_encode_widths;
      Alcotest.test_case "check_encode rejects negative" `Quick
        test_check_encode_rejects_negative;
    ] )
