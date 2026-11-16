(* Tests for Arith: each operation is the native one when the exact result
   fits the native int, and otherwise fails as it documents. The exact result
   is computed on [int64], which holds every sum and difference of two
   63-bit ints, and every product whose [Int64.div] check passes. *)

open Wire_runtime

let edges =
  [
    0;
    1;
    -1;
    2;
    -2;
    3;
    1 lsl 30;
    -(1 lsl 30);
    (1 lsl 31) - 1;
    1 lsl 31;
    -(1 lsl 31);
    (1 lsl 31) + 1;
    max_int;
    max_int - 1;
    max_int / 2;
    max_int / 3;
    min_int;
    min_int + 1;
    min_int / 2;
    min_int / 3;
  ]

let fits n =
  Int64.compare n (Int64.of_int min_int) >= 0
  && Int64.compare n (Int64.of_int max_int) <= 0

(* The exact result, when [int64] holds it. *)
let exact op a b =
  let a = Int64.of_int a and b = Int64.of_int b in
  match op with
  | `Add -> Some (Int64.add a b)
  | `Sub -> Some (Int64.sub a b)
  | `Mul ->
      let p = Int64.mul a b in
      if a = 0L || (Int64.div p a = b && not (a = -1L && b = Int64.min_int))
      then Some p
      else None

let name = function `Add -> "add" | `Sub -> "sub" | `Mul -> "mul"

let ops = function
  | `Add -> (Arith.add, Arith.checked_add)
  | `Sub -> (Arith.sub, Arith.checked_sub)
  | `Mul -> (Arith.mul, Arith.checked_mul)

(* The value a checked overflow carries: just past the side it went over. *)
let past op a b =
  let positive =
    match op with `Add | `Sub -> a >= 0 | `Mul -> Bool.equal (a < 0) (b < 0)
  in
  if positive then Int64.succ (Int64.of_int max_int)
  else Int64.pred (Int64.of_int min_int)

let one op a b =
  let hot, checked = ops op in
  let what = Fmt.str "%s %d %d" (name op) a b in
  match exact op a b with
  | Some n when fits n ->
      Alcotest.(check int) what (Int64.to_int n) (hot a b);
      Alcotest.(check int) (what ^ ", checked") (Int64.to_int n) (checked a b)
  | _ -> (
      (match hot a b with
      | v -> Alcotest.failf "%s: %d, not an overflow" what v
      | exception Arith.Overflow -> ());
      match checked a b with
      | v -> Alcotest.failf "%s, checked: %d, not an overflow" what v
      | exception
          Errors.Parse_error
            { at = 0; field = []; kind = Value_out_of_range { value } } ->
          Alcotest.(check int64) (what ^ ", reported") (past op a b) value)

let edges_test () =
  List.iter
    (fun op -> List.iter (fun a -> List.iter (one op a) edges) edges)
    [ `Add; `Sub; `Mul ]

let random () =
  let rand = Random.State.make [| 11 |] in
  let draw () =
    let n = Random.State.bits rand lor (Random.State.bits rand lsl 30) in
    let n = n lor (Random.State.bits rand lsl 60) in
    if Random.State.bool rand then n asr Random.State.int rand 62 else n
  in
  for _ = 1 to 20_000 do
    let a = draw () and b = draw () in
    List.iter (fun op -> one op a b) [ `Add; `Sub; `Mul ]
  done

let suite =
  ( "arith",
    [
      Alcotest.test_case "at the edges of the int" `Quick edges_test;
      Alcotest.test_case "random operands" `Quick random;
    ] )
