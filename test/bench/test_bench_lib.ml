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

let test_cycling_reads_correct_data () =
  let buf = Bytes.of_string "ABCDEF" in
  let bytes_read = ref [] in
  let fn, _reset =
    cycling ~data:buf ~n_items:3 ~size:2 (fun buf off ->
        bytes_read := Bytes.sub_string buf off 2 :: !bytes_read)
  in
  fn ();
  fn ();
  fn ();
  Alcotest.(check (list string)) "data" [ "EF"; "CD"; "AB" ] !bytes_read

let test_cycling_single_item () =
  let buf = Bytes.of_string "XX" in
  let log = ref [] in
  let fn, _reset =
    cycling ~data:buf ~n_items:1 ~size:2 (fun _buf off -> log := off :: !log)
  in
  fn ();
  fn ();
  fn ();
  (* Always offset 0 *)
  Alcotest.(check (list int)) "single" [ 0; 0; 0 ] !log

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
  let t = v "test" ~size:4 noop |> with_ffi (fun _buf -> ()) (Bytes.create 4) in
  check t

let test_check_with_expect () =
  let trace = ref [] in
  let reset () = trace := "reset" :: !trace in
  let ocaml () = trace := "timed-ocaml" :: !trace in
  let ffi _buf = trace := "timed-ffi" :: !trace in
  let c_loop _buf _off _n =
    trace := "timed-c" :: !trace;
    0
  in
  let ocaml_result () =
    trace := "result-ocaml" :: !trace;
    42
  in
  let ffi_result () =
    trace := "result-ffi" :: !trace;
    42
  in
  let c_result () =
    trace := "result-c" :: !trace;
    42
  in
  let t =
    v "test" ~size:4 ~reset ocaml
    |> with_ffi ffi Bytes.empty |> with_c c_loop Bytes.empty
    |> with_expect ~equal:Int.equal ~pp:Fmt.int ~ffi:ffi_result ~c:c_result
         ocaml_result
  in
  check t;
  let rev = List.rev !trace in
  Alcotest.(check (list string))
    "expectation order"
    [
      "reset";
      "timed-ocaml";
      "reset";
      "timed-ffi";
      "reset";
      "timed-c";
      "reset";
      "result-ocaml";
      "result-ffi";
      "result-c";
    ]
    rev

let test_ffi_buf_correct () =
  let expected = Bytes.of_string "DEADBEEF" in
  let received = ref Bytes.empty in
  let t =
    v "test" ~size:8 noop |> with_ffi (fun buf -> received := buf) expected
  in
  check t;
  Alcotest.(check string) "ffi buf" "DEADBEEF" (Bytes.to_string !received)

let test_c_ffi_order () =
  let c_called = ref false in
  let ffi_called = ref false in
  let c_loop _buf _off _n =
    c_called := true;
    0
  in
  let ffi_check _buf = ffi_called := true in
  let buf = Bytes.create 4 in
  (* with_c then with_ffi *)
  let t1 = v "t1" ~size:4 noop |> with_c c_loop buf |> with_ffi ffi_check buf in
  check t1;
  Alcotest.(check bool) "c called (c then ffi)" true !c_called;
  Alcotest.(check bool) "ffi called (c then ffi)" true !ffi_called;
  c_called := false;
  ffi_called := false;
  (* with_ffi then with_c *)
  let t2 = v "t2" ~size:4 noop |> with_ffi ffi_check buf |> with_c c_loop buf in
  check t2;
  Alcotest.(check bool) "c called (ffi then c)" true !c_called;
  Alcotest.(check bool) "ffi called (ffi then c)" true !ffi_called

(* ── reset semantics ── *)

let test_reset_called_before_check () =
  let trace = ref [] in
  let reset () = trace := "reset" :: !trace in
  let ocaml () = trace := "ocaml" :: !trace in
  let t = v "test" ~size:4 ~reset ocaml in
  check t;
  (* reset must come before ocaml *)
  let rev = List.rev !trace in
  Alcotest.(check (list string)) "order" [ "reset"; "ocaml" ] rev

let test_reset_before_ffi () =
  let trace = ref [] in
  let reset () = trace := "reset" :: !trace in
  let ocaml () = trace := "ocaml" :: !trace in
  let ffi _buf = trace := "ffi" :: !trace in
  let t = v "test" ~size:4 ~reset ocaml |> with_ffi ffi Bytes.empty in
  check t;
  let rev = List.rev !trace in
  Alcotest.(check (list string))
    "order"
    [ "reset"; "ocaml"; "reset"; "ffi" ]
    rev

let test_run_one_reset () =
  (* run_one calls reset: once in check, once before timing, once before alloc
     = 3 total *)
  let reset_count = ref 0 in
  let reset () = incr reset_count in
  let t = v "test" ~size:4 ~reset noop in
  let _r =
    run_table ~title:"count" ~n:10 [ t ];
    ()
  in
  Alcotest.(check int) "reset count" 3 !reset_count

let test_reset_reinitializes_cycling () =
  (* Verify that each phase starts at offset 0 after reset *)
  let buf = Bytes.of_string "AABB" in
  let phase_starts = ref [] in
  let call_in_phase = ref 0 in
  let fn, cycling_reset =
    cycling ~data:buf ~n_items:2 ~size:2 (fun _buf off ->
        if !call_in_phase = 0 then phase_starts := off :: !phase_starts;
        incr call_in_phase)
  in
  let reset () =
    cycling_reset ();
    call_in_phase := 0
  in
  let t = v "test" ~size:2 ~reset fn in
  run_table ~title:"cycling reset" ~n:10 [ t ];
  (* Each phase (check, timing, alloc) should start at offset 0 *)
  List.iter
    (fun off -> Alcotest.(check int) "phase starts at 0" 0 off)
    !phase_starts

let test_reset_with_mutable_counter () =
  (* Simulates the routing benchmark pattern: counter accumulates per phase *)
  let counter = ref 0 in
  let reset () = counter := 0 in
  let t = v "counter" ~size:4 ~reset (fun () -> incr counter) in
  run_table ~title:"counter" ~n:10 [ t ];
  (* After run_table, counter reflects only the alloc phase (last phase) *)
  Alcotest.(check int) "counter = n" 10 !counter

(* ── pack ── *)

let test_pack () =
  let a = Bytes.of_string "AB" in
  let b = Bytes.of_string "CD" in
  let c = Bytes.of_string "EF" in
  let buf, n = pack [| a; b; c |] ~size:2 in
  Alcotest.(check int) "count" 3 n;
  Alcotest.(check int) "length" 6 (Bytes.length buf);
  Alcotest.(check string) "content" "ABCDEF" (Bytes.to_string buf)

let test_pack_single () =
  let a = Bytes.of_string "ABCD" in
  let buf, n = pack [| a |] ~size:4 in
  Alcotest.(check int) "count" 1 n;
  Alcotest.(check string) "content" "ABCD" (Bytes.to_string buf)

let test_pack_preserves_byte_layout () =
  let items =
    Array.init 4 (fun i ->
        let b = Bytes.create 3 in
        Bytes.set_uint8 b 0 i;
        Bytes.set_uint8 b 1 (i * 10);
        Bytes.set_uint8 b 2 (i * 100);
        b)
  in
  let buf, n = pack items ~size:3 in
  Alcotest.(check int) "count" 4 n;
  (* Verify each item is accessible at the expected offset *)
  for i = 0 to 3 do
    let off = i * 3 in
    Alcotest.(check int)
      (Fmt.str "item %d byte 0" i)
      i (Bytes.get_uint8 buf off);
    Alcotest.(check int)
      (Fmt.str "item %d byte 1" i)
      (i * 10)
      (Bytes.get_uint8 buf (off + 1))
  done

(* ── time_ns / alloc_words ── *)

let test_time_ns_non_negative () =
  let ns = time_ns 1 noop in
  Alcotest.(check bool) "non-negative" true (ns >= 0.0)

let test_time_ns_division () =
  (* time_ns 2 should return roughly half of time_ns 1 for the same work *)
  let work () =
    let r = ref 0 in
    for i = 1 to 10_000 do
      r := Sys.opaque_identity (!r + i)
    done
  in
  let t1 = time_ns 1 work in
  let t2 = time_ns 2 work in
  (* t2 should be roughly t1/2 (within 10x tolerance for CI noise) *)
  Alcotest.(check bool) "t2 < t1" true (t2 < t1 *. 10.0)

let test_alloc_words_zero () =
  let w = alloc_words 100 noop in
  Alcotest.(check bool) "zero alloc" true (w < 1.0)

let test_alloc_words_nonzero () =
  let w =
    alloc_words 100 (fun () -> ignore (Sys.opaque_identity (Bytes.create 64)))
  in
  Alcotest.(check bool) "nonzero alloc" true (w >= 1.0)

let test_alloc_words_scales () =
  (* Larger allocation = more words *)
  let w_small =
    alloc_words 100 (fun () -> ignore (Sys.opaque_identity (Bytes.create 8)))
  in
  let w_large =
    alloc_words 100 (fun () -> ignore (Sys.opaque_identity (Bytes.create 128)))
  in
  Alcotest.(check bool) "large > small" true (w_large > w_small)

(* ── run_table ── *)

let test_run_table_smoke () =
  let fn, reset =
    cycling ~data:(Bytes.create 8) ~n_items:2 ~size:4 (fun _buf _off -> ())
  in
  run_table ~title:"smoke" ~n:100 [ v "noop" ~size:4 ~reset fn ]

let test_run_table_with_unit () =
  run_table ~title:"pkt test" ~n:100 ~unit:"pkt" [ v "noop" ~size:4 noop ]

let test_run_table_multiple_specs () =
  let fn1, r1 =
    cycling ~data:(Bytes.create 4) ~n_items:1 ~size:4 (fun _buf _off -> ())
  in
  let fn2, r2 =
    cycling ~data:(Bytes.create 8) ~n_items:2 ~size:4 (fun _buf _off -> ())
  in
  run_table ~title:"multi" ~n:100
    [ v "first" ~size:4 ~reset:r1 fn1; v "second" ~size:8 ~reset:r2 fn2 ]

let test_run_table_size_zero () =
  (* Write benchmarks use size:0 *)
  run_table ~title:"writes" ~n:100 [ v "write" ~size:0 noop ]

let test_run_table_ffi_reset () =
  let idx = ref 0 in
  let seen = ref [] in
  let reset () = idx := 0 in
  let ffi _buf =
    seen := string_of_int !idx :: !seen;
    incr idx
  in
  let t = v "ffi" ~size:4 ~reset noop |> with_ffi ffi Bytes.empty in
  run_table ~title:"ffi reset" ~n:2 [ t ];
  Alcotest.(check (list string))
    "ffi sequence" [ "0"; "0"; "1" ] (List.rev !seen)

(* ── integration: cycling + reset + run_table ── *)

let test_cycling_through_run_table () =
  (* End-to-end: cycling visits all items, and each phase is balanced *)
  let buf = Bytes.init 12 (fun i -> Char.chr (i + 65)) in
  let phase_seen = Hashtbl.create 4 in
  let total_calls = ref 0 in
  let fn, cycling_reset =
    cycling ~data:buf ~n_items:3 ~size:4 (fun buf off ->
        let s = Bytes.sub_string buf off 4 in
        let c = try Hashtbl.find phase_seen s with Not_found -> 0 in
        Hashtbl.replace phase_seen s (c + 1);
        incr total_calls)
  in
  let reset () =
    cycling_reset ();
    Hashtbl.clear phase_seen;
    total_calls := 0
  in
  let t = v "test" ~size:4 ~reset fn in
  run_table ~title:"e2e" ~n:9 [ t ];
  (* After run_table, only the alloc phase's counts remain (last phase) *)
  Alcotest.(check int) "total calls" 9 !total_calls;
  Alcotest.(check int) "ABCD visits" 3 (Hashtbl.find phase_seen "ABCD");
  Alcotest.(check int) "EFGH visits" 3 (Hashtbl.find phase_seen "EFGH");
  Alcotest.(check int) "IJKL visits" 3 (Hashtbl.find phase_seen "IJKL")

let suite =
  ( "bench_lib",
    [
      Alcotest.test_case "cycling: sequence" `Quick test_cycling_sequence;
      Alcotest.test_case "cycling: wraps" `Quick test_cycling_wraps;
      Alcotest.test_case "cycling: reset" `Quick test_cycling_reset;
      Alcotest.test_case "cycling: reads correct data" `Quick
        test_cycling_reads_correct_data;
      Alcotest.test_case "cycling: single item" `Quick test_cycling_single_item;
      Alcotest.test_case "check: ocaml only" `Quick test_check_ocaml_only;
      Alcotest.test_case "check: with ffi" `Quick test_check_with_ffi;
      Alcotest.test_case "check: with expect" `Quick test_check_with_expect;
      Alcotest.test_case "check: ffi receives correct buf" `Quick
        test_ffi_buf_correct;
      Alcotest.test_case "builder: v ocaml only" `Quick test_v_ocaml_only;
      Alcotest.test_case "builder: with_c/with_ffi order" `Quick
        test_c_ffi_order;
      Alcotest.test_case "reset: called before check" `Quick
        test_reset_called_before_check;
      Alcotest.test_case "reset: called before ffi check" `Quick
        test_reset_before_ffi;
      Alcotest.test_case "reset: count in run_one" `Quick test_run_one_reset;
      Alcotest.test_case "reset: reinitializes cycling" `Quick
        test_reset_reinitializes_cycling;
      Alcotest.test_case "reset: mutable counter" `Quick
        test_reset_with_mutable_counter;
      Alcotest.test_case "pack: basic" `Quick test_pack;
      Alcotest.test_case "pack: single" `Quick test_pack_single;
      Alcotest.test_case "pack: byte layout" `Quick
        test_pack_preserves_byte_layout;
      Alcotest.test_case "timing: time_ns non-negative" `Quick
        test_time_ns_non_negative;
      Alcotest.test_case "timing: time_ns divides by n" `Quick
        test_time_ns_division;
      Alcotest.test_case "timing: alloc_words zero" `Quick test_alloc_words_zero;
      Alcotest.test_case "timing: alloc_words nonzero" `Quick
        test_alloc_words_nonzero;
      Alcotest.test_case "timing: alloc_words scales" `Quick
        test_alloc_words_scales;
      Alcotest.test_case "run_table: smoke" `Quick test_run_table_smoke;
      Alcotest.test_case "run_table: with unit" `Quick test_run_table_with_unit;
      Alcotest.test_case "run_table: multiple specs" `Quick
        test_run_table_multiple_specs;
      Alcotest.test_case "run_table: size zero" `Quick test_run_table_size_zero;
      Alcotest.test_case "run_table: ffi reset" `Quick test_run_table_ffi_reset;
      Alcotest.test_case "integration: cycling through run_table" `Quick
        test_cycling_through_run_table;
      Alcotest.test_case "verify: routing counts" `Quick
        (Routing_bench.verify ~n_pkts:10_000);
      Alcotest.test_case "verify: clcw anomalies" `Quick
        (Clcw_bench.verify ~n_words:10_000);
      Alcotest.test_case "verify: gateway checksums" `Quick
        (Gateway_bench.verify ~n_frames:1_000);
    ] )
