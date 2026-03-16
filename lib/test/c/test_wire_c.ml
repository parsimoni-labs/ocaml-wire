(** Tests for Wire_c — C library generation from Wire codecs. *)

open Wire

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let simple_module =
  module_ "TestSimple" [ typedef ~entrypoint:true simple_struct ]

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "wire_c_test" "" in
  let s =
    Wire_c.schema ~name:"TestSimple" ~module_:simple_module ~wire_size:3
  in
  Wire_c.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_schema_of_struct () =
  let s = Wire_c.schema_of_struct simple_struct in
  (* generate_3d uses the schema — check we can produce a .3d file *)
  let tmpdir = Filename.temp_dir "wire_c_test2" "" in
  Wire_c.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool)
    "3d file from schema_of_struct" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_ensure_dir () =
  let tmpdir = Filename.temp_dir "wire_c_ensure" "" in
  Unix.rmdir tmpdir;
  Wire_c.ensure_dir tmpdir;
  Alcotest.(check bool) "dir created" true (Sys.file_exists tmpdir);
  Unix.rmdir tmpdir

let test_generate_c () =
  (* generate_c requires 3d.exe; skip when not available *)
  if Wire_c.has_3d_exe () then begin
    let tmpdir = Filename.temp_dir "wire_c_gen_c" "" in
    let s = Wire_c.schema_of_struct simple_struct in
    Wire_c.generate_3d ~outdir:tmpdir [ s ];
    Wire_c.generate_c ~outdir:tmpdir [ s ];
    let c_path = Filename.concat tmpdir "TestSimple.c" in
    Alcotest.(check bool) "C file generated" true (Sys.file_exists c_path)
  end

let test_has_3d_exe () =
  (* Just check the function is callable and returns a bool *)
  let result = Wire_c.has_3d_exe () in
  Alcotest.(check bool) "has_3d_exe returns bool" result result

let test_main_exists () =
  (* main dispatches on Sys.argv; calling it in tests would exit, so we only
     verify that the value exists and has the right type. *)
  let _ = (Wire_c.main : package:string -> Wire_c.schema list -> unit) in
  ()

let suite =
  ( "wire_c",
    [
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "schema_of_struct" `Quick test_schema_of_struct;
      Alcotest.test_case "ensure_dir" `Quick test_ensure_dir;
      Alcotest.test_case "generate_c (needs 3d.exe)" `Quick test_generate_c;
      Alcotest.test_case "has_3d_exe" `Quick test_has_3d_exe;
      Alcotest.test_case "main exists" `Quick test_main_exists;
    ] )
