(** Tests for Diff_gen -- EverParse differential test generation. *)

open Wire
open Wire.Everparse.Raw

let simple_struct =
  struct_ "TestDiffGen" [ field "A" uint8; field "B" uint16be ]

let simple_module = module_ [ typedef ~entrypoint:true simple_struct ]

let test_schema_creation () =
  let s =
    Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
      ~module_:simple_module
  in
  Alcotest.(check bool) "some" true (Option.is_some s)

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "diff_gen_test" "" in
  let s =
    Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
      ~module_:simple_module
    |> Option.get
  in
  Wire_diff_gen.generate_3d_files ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestDiffGen.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let has_everparse () =
  let ic = Unix.open_process_in "command -v 3d.exe 2>/dev/null" in
  let found =
    try
      ignore (input_line ic);
      true
    with End_of_file -> false
  in
  ignore (Unix.close_process_in ic);
  found
  ||
  let local =
    Filename.concat (Sys.getenv "HOME") ".local/everparse/bin/3d.exe"
  in
  Sys.file_exists local

let test_generate_c_stubs () =
  (* generate_c_stubs reads EverParse-generated .h files, so it requires
     EverParse to have been run first -- skip when 3d.exe is unavailable *)
  if has_everparse () then begin
    let schema_dir = Filename.temp_dir "diff_gen_c_stubs_schema" "" in
    let outdir = Filename.temp_dir "diff_gen_c_stubs_out" "" in
    let s =
      Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
        ~module_:simple_module
      |> Option.get
    in
    Wire_diff_gen.generate_3d_files ~outdir:schema_dir [ s ];
    Wire_diff_gen.run_everparse ~outdir:schema_dir [ s ];
    Wire_diff_gen.generate_c_stubs ~schema_dir ~outdir [ s ];
    let path = Filename.concat outdir "stubs.c" in
    Alcotest.(check bool) "stubs.c exists" true (Sys.file_exists path)
  end

let test_generate_ml_stubs () =
  let outdir = Filename.temp_dir "diff_gen_ml_stubs" "" in
  let s =
    Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
      ~module_:simple_module
    |> Option.get
  in
  Wire_diff_gen.generate_ml_stubs ~outdir [ s ];
  let path = Filename.concat outdir "stubs.ml" in
  Alcotest.(check bool) "stubs.ml exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir outdir

let test_generate_test_runner () =
  let outdir = Filename.temp_dir "diff_gen_test_runner" "" in
  let s =
    Wire_diff_gen.schema ~name:"TestDiffGen" ~struct_:simple_struct
      ~module_:simple_module
    |> Option.get
  in
  Wire_diff_gen.generate_test_runner ~outdir [ s ];
  let path = Filename.concat outdir "diff_test.ml" in
  Alcotest.(check bool) "diff_test.ml exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir outdir

let suite =
  ( "wire_diff_gen",
    [
      Alcotest.test_case "schema creation" `Quick test_schema_creation;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "generate_c_stubs" `Quick test_generate_c_stubs;
      Alcotest.test_case "generate_ml_stubs" `Quick test_generate_ml_stubs;
      Alcotest.test_case "generate_test_runner" `Quick test_generate_test_runner;
    ] )
