(* Tests for Bench_lib: the shared benchmark framework. *)

open Bench_lib

(* ── cycling ── *)

let test_cycling_sequence () =
  let buf = Bytes.of_string "AABBCC" in
  let log = ref [] in
  let fn, _reset =
    cycling ~data:buf ~n_items:3 ~size:2 (fun _buf off -> log := off :: !log)
  in
  fn ();
  fn ();
  fn ();
  Alcotest.(check (list int)) "offsets" [ 4; 2; 0 ] !log

let test_cycling_wraps () =
  let buf = Bytes.of_string "AABB" in
  let log = ref [] in
  let fn, _reset =
    cycling ~data:buf ~n_items:2 ~size:2 (fun _buf off -> log := off :: !log)
  in
  for _ = 1 to 5 do
    fn ()
  done;
  (* 0, 2, 0, 2, 0 *)
  Alcotest.(check (list int)) "wraps" [ 0; 2; 0; 2; 0 ] !log

let test_cycling_reset () =
  let buf = Bytes.of_string "AABB" in
  let log = ref [] in
  let fn, reset =
    cycling ~data:buf ~n_items:2 ~size:2 (fun _buf off -> log := off :: !log)
  in
  fn ();
  fn ();
  reset ();
  fn ();
  (* After reset, starts from 0 again *)
  Alcotest.(check (list int)) "reset" [ 0; 2; 0 ] !log

(* ── v / with_c / with_ffi ── *)

let noop () = ()

let test_v_ocaml_only () =
  let called = ref false in
  let t = v "test" ~size:4 (fun () -> called := true) in
  ignore (Sys.opaque_identity t);
  Alcotest.(check bool) "not called yet" false !called

let test_check_ocaml_only () =
  let called = ref false in
  let t = v "test" ~size:4 (fun () -> called := true) in
  check t;
  Alcotest.(check bool) "called by check" true !called

let test_check_with_ffi () =
  let t =
    v "test" ~size:4 noop |> with_ffi (fun _buf -> true) (Bytes.create 4)
  in
  check t

let test_check_ffi_failure () =
  let t =
    v "test" ~size:4 noop |> with_ffi (fun _buf -> false) (Bytes.create 4)
  in
  match check t with
  | exception Failure _ -> ()
  | () -> Alcotest.fail "expected Failure"

(* ── reset ── *)

let test_reset_called () =
  let reset_count = ref 0 in
  let reset () = incr reset_count in
  let t = v "test" ~size:4 ~reset noop in
  check t;
  Alcotest.(check bool) "reset called" true (!reset_count > 0)

(* ── pack ── *)

let test_pack () =
  let a = Bytes.of_string "AB" in
  let b = Bytes.of_string "CD" in
  let c = Bytes.of_string "EF" in
  let buf, n = pack [| a; b; c |] ~size:2 in
  Alcotest.(check int) "count" 3 n;
  Alcotest.(check int) "length" 6 (Bytes.length buf);
  Alcotest.(check string) "content" "ABCDEF" (Bytes.to_string buf)

(* ── time_ns / alloc_words ── *)

let test_time_ns_non_negative () =
  let ns = time_ns 1 noop in
  Alcotest.(check bool) "non-negative" true (ns >= 0.0)

let test_alloc_words_zero () =
  let w = alloc_words 100 noop in
  Alcotest.(check bool) "zero alloc" true (w < 1.0)

let test_alloc_words_nonzero () =
  let w =
    alloc_words 100 (fun () -> ignore (Sys.opaque_identity (Bytes.create 64)))
  in
  Alcotest.(check bool) "nonzero alloc" true (w >= 1.0)

(* ── run_table smoke test ── *)

let test_run_table_smoke () =
  let fn, reset =
    cycling ~data:(Bytes.create 8) ~n_items:2 ~size:4 (fun _buf _off -> ())
  in
  run_table ~title:"smoke" ~n:100 [ v "noop" ~size:4 ~reset fn ]

let test_run_table_with_unit () =
  run_table ~title:"pkt test" ~n:100 ~unit:"pkt" [ v "noop" ~size:4 noop ]

let () =
  Alcotest.run "bench_lib"
    [
      ( "bench_lib",
        [
          Alcotest.test_case "cycling sequence" `Quick test_cycling_sequence;
          Alcotest.test_case "cycling wraps" `Quick test_cycling_wraps;
          Alcotest.test_case "cycling reset" `Quick test_cycling_reset;
          Alcotest.test_case "v ocaml only" `Quick test_v_ocaml_only;
          Alcotest.test_case "check ocaml only" `Quick test_check_ocaml_only;
          Alcotest.test_case "check with ffi" `Quick test_check_with_ffi;
          Alcotest.test_case "check ffi failure" `Quick test_check_ffi_failure;
          Alcotest.test_case "reset called" `Quick test_reset_called;
          Alcotest.test_case "pack" `Quick test_pack;
          Alcotest.test_case "time_ns non-negative" `Quick
            test_time_ns_non_negative;
          Alcotest.test_case "alloc_words zero" `Quick test_alloc_words_zero;
          Alcotest.test_case "alloc_words nonzero" `Quick
            test_alloc_words_nonzero;
          Alcotest.test_case "run_table smoke" `Quick test_run_table_smoke;
          Alcotest.test_case "run_table with unit" `Quick
            test_run_table_with_unit;
        ] );
    ]
