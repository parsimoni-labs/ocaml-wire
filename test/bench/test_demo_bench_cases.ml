open Demo_bench_cases

let test_write_cases () =
  List.iter (fun case -> case.verify ()) write_benchmark_cases

let test_projection_case_ids_unique () =
  let seen = Hashtbl.create 17 in
  List.iter
    (fun (Read_case case) ->
      if Hashtbl.mem seen case.id then
        Alcotest.failf "duplicate projection case id";
      Hashtbl.add seen case.id ())
    projection_cases

let test_read_cases_subset () =
  let projection_ids =
    List.fold_left
      (fun acc (Read_case case) -> case.id :: acc)
      [] projection_cases
  in
  List.iter
    (fun (Read_case case) ->
      Alcotest.(check bool) case.label true (List.mem case.id projection_ids))
    read_benchmark_cases

let suite =
  ( "demo_bench_cases",
    [
      Alcotest.test_case "write verifiers" `Quick test_write_cases;
      Alcotest.test_case "projection ids unique" `Quick
        test_projection_case_ids_unique;
      Alcotest.test_case "read cases subset" `Quick test_read_cases_subset;
    ] )
