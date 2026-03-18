(* Tests for Staged: identity wrapper for staged computation. *)

open Wire

let test_stage_unstage () =
  let f x = x + 1 in
  let staged = Staged.stage f in
  let f' = Staged.unstage staged in
  Alcotest.(check int) "roundtrip" 43 (f' 42)

let test_stage_value () =
  let v = Staged.stage 99 in
  Alcotest.(check int) "value" 99 (Staged.unstage v)

let suite =
  ( "staged",
    [
      Alcotest.test_case "stage/unstage function" `Quick test_stage_unstage;
      Alcotest.test_case "stage/unstage value" `Quick test_stage_value;
    ] )
