open Wire_runtime

let mask n = n land 0xFFFF_FFFF
let lanes a = [ Vec.get a 0; Vec.get a 1; Vec.get a 2; Vec.get a 3 ]
let check msg expected a = Alcotest.(check (list int)) msg expected (lanes a)

(* The edges of a 32-bit lane, and ints wider than one, which a lane holds
   modulo 2^32. *)
let edges =
  [
    0;
    1;
    0x7FFF_FFFF;
    0x8000_0000;
    0xFFFF_FFFF;
    0x1_0000_0000;
    0x1_2345_6789;
    -1;
    min_int;
    max_int;
  ]

let rng = Random.State.make [| 0x5eed |]

let int () =
  if Random.State.int rng 4 = 0 then
    List.nth edges (Random.State.int rng (List.length edges))
  else Random.State.bits rng lor (Random.State.bits rng lsl 30)

let quad () = [ int (); int (); int (); int () ]

let of_list = function
  | [ a; b; c; d ] -> Vec.v4 a b c d
  | _ -> invalid_arg "of_list"

let rounds = 1000

let splat () =
  List.iter
    (fun n ->
      let m = mask n in
      check (Fmt.str "v %d" n) [ m; m; m; m ] (Vec.v n))
    edges

let v4 () =
  for _ = 1 to rounds do
    let q = quad () in
    check "v4" (List.map mask q) (of_list q)
  done

let lane_op name f g =
  ( name,
    `Quick,
    fun () ->
      for _ = 1 to rounds do
        let a = quad () and b = quad () in
        check name
          (List.map2 (fun x y -> mask (g (mask x) (mask y))) a b)
          (f (of_list a) (of_list b))
      done )

let all_ones = 0xFFFF_FFFF

(* Half the lanes of [b] are those of [a], so equal and unequal lanes both
   occur, in every position. *)
let equal () =
  for _ = 1 to rounds do
    let a = quad () in
    let b = List.map (fun x -> if Random.State.bool rng then x else int ()) a in
    check "equal"
      (List.map2 (fun x y -> if mask x = mask y then all_ones else 0) a b)
      (Vec.equal (of_list a) (of_list b))
  done

let zeroes () =
  let cases =
    [ [ 0; 0; 0; 0 ]; [ 0x1_0000_0000; 0; 0; 0 ]; [ 0; 0; 0; 0x8000_0000 ] ]
    @ List.init rounds (fun _ ->
        List.init 4 (fun _ -> if Random.State.bool rng then 0 else int ()))
  in
  List.iter
    (fun q ->
      let a = of_list q in
      check "is_zero"
        (List.map (fun x -> if mask x = 0 then all_ones else 0) q)
        (Vec.is_zero a);
      Alcotest.(check bool)
        "is_all_zero"
        (List.for_all (fun x -> mask x = 0) q)
        (Vec.is_all_zero a))
    cases

(* The shift is a constant of the instruction, so each amount is spelled
   out. *)
let shifts () =
  for _ = 1 to rounds do
    let q = quad () in
    let a = of_list q in
    let right n = List.map (fun x -> mask x lsr n) q in
    let left n = List.map (fun x -> mask (x lsl n)) q in
    check "lsr 1" (right 1) (Vec.shift_right_logical 1 a);
    check "lsr 8" (right 8) (Vec.shift_right_logical 8 a);
    check "lsr 31" (right 31) (Vec.shift_right_logical 31 a);
    check "lsl 1" (left 1) (Vec.shift_left 1 a);
    check "lsl 8" (left 8) (Vec.shift_left 8 a);
    check "lsl 31" (left 31) (Vec.shift_left 31 a)
  done

let before () =
  check "in order" [ 6; 1; 2; 3 ] (Vec.before (Vec.v4 9 8 7 6) (Vec.v4 1 2 3 4));
  for _ = 1 to rounds do
    let p = quad () and q = quad () in
    check "before"
      (List.map mask [ List.nth p 3; List.nth q 0; List.nth q 1; List.nth q 2 ])
      (Vec.before (of_list p) (of_list q))
  done

let sum () =
  check "max lanes" [ all_ones; all_ones; all_ones; all_ones ] (Vec.v (-1));
  Alcotest.(check int) "sum max" (4 * all_ones) (Vec.sum (Vec.v (-1)));
  for _ = 1 to rounds do
    let q = quad () in
    Alcotest.(check int)
      "sum"
      (List.fold_left (fun acc x -> acc + mask x) 0 q)
      (Vec.sum (of_list q))
  done

(* Lane [k] of a load at [off] is the four bytes at [off + 4k], little-endian;
   a store writes those 16 bytes and no other. Every offset where 16 bytes fit
   is tried, up to the last byte of the buffer. *)
let lanes_at get off = List.init 4 (fun k -> mask (get (off + (4 * k))))
let fill len = Bytes.init len (fun _ -> Char.chr (Random.State.int rng 256))

let bytes () =
  for len = 16 to 40 do
    let b = fill len in
    for off = 0 to len - 16 do
      let get i = Int32.to_int (Bytes.get_int32_le b i) in
      check "load" (lanes_at get off) (Vec.load b off);
      let q = quad () in
      let b' = Bytes.copy b in
      Vec.store b' off (of_list q);
      let get' i = Int32.to_int (Bytes.get_int32_le b' i) in
      Alcotest.(check (list int)) "store" (List.map mask q) (lanes_at get' off);
      Alcotest.(check string)
        "before the store" (Bytes.sub_string b 0 off)
        (Bytes.sub_string b' 0 off);
      Alcotest.(check string)
        "after the store"
        (Bytes.sub_string b (off + 16) (len - off - 16))
        (Bytes.sub_string b' (off + 16) (len - off - 16))
    done
  done

let suite =
  ( "vec",
    [
      Alcotest.test_case "v" `Quick splat;
      Alcotest.test_case "v4 and get" `Quick v4;
      lane_op "add" Vec.add ( + );
      lane_op "sub" Vec.sub ( - );
      lane_op "logand" Vec.logand ( land );
      lane_op "logor" Vec.logor ( lor );
      Alcotest.test_case "equal" `Quick equal;
      Alcotest.test_case "is_zero and is_all_zero" `Quick zeroes;
      Alcotest.test_case "shifts" `Quick shifts;
      Alcotest.test_case "before" `Quick before;
      Alcotest.test_case "sum" `Quick sum;
      Alcotest.test_case "load and store" `Quick bytes;
    ] )
