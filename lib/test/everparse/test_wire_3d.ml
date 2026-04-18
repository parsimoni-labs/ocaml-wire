(** Tests for the EverParse pipeline -- schema generation and 3D output. *)

open Wire
open Wire.Everparse.Raw

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let simple_module = module_ [ typedef ~entrypoint:true simple_struct ]

let test_everparse_name () =
  (* All-caps names get lowercased with first letter capitalized *)
  Alcotest.(check string) "CLCW -> Clcw" "Clcw" (Wire_3d.everparse_name "CLCW");
  Alcotest.(check string)
    "TMFrame -> Tmframe" "Tmframe"
    (Wire_3d.everparse_name "TMFrame");
  (* Standard camelCase is preserved *)
  Alcotest.(check string) "Foo -> Foo" "Foo" (Wire_3d.everparse_name "Foo");
  Alcotest.(check string)
    "myField -> myField" "myField"
    (Wire_3d.everparse_name "myField");
  (* Single uppercase letter at start is not >=2 consecutive uppercase *)
  Alcotest.(check string)
    "Hello -> Hello" "Hello"
    (Wire_3d.everparse_name "Hello");
  (* Empty string *)
  Alcotest.(check string) "empty" "" (Wire_3d.everparse_name "")

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "wire_3d_test" "" in
  let s =
    Wire.Everparse.Raw.of_module ~name:"TestSimple" ~module_:simple_module
      ~wire_size:3
  in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_schema_of_struct () =
  let s = Wire.Everparse.schema_of_struct simple_struct in
  (* generate_3d uses the schema -- check we can produce a .3d file *)
  let tmpdir = Filename.temp_dir "wire_3d_test2" "" in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool)
    "3d file from schema_of_struct" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_ensure_dir () =
  let tmpdir = Filename.temp_dir "wire_3d_ensure" "" in
  Unix.rmdir tmpdir;
  Wire_3d.ensure_dir tmpdir;
  Alcotest.(check bool) "dir created" true (Sys.file_exists tmpdir);
  Unix.rmdir tmpdir

let test_generate_c () =
  (* generate_c requires 3d.exe; skip when not available *)
  if Wire_3d.has_3d_exe () then begin
    let tmpdir = Filename.temp_dir "wire_3d_gen_c" "" in
    let s = Wire.Everparse.schema_of_struct simple_struct in
    Wire_3d.generate_3d ~outdir:tmpdir [ s ];
    Wire_3d.generate_c ~outdir:tmpdir [ s ];
    let c_path = Filename.concat tmpdir "TestSimple.c" in
    Alcotest.(check bool) "C file generated" true (Sys.file_exists c_path);
    let ext_path = Filename.concat tmpdir "TestSimple_ExternalTypedefs.h" in
    Alcotest.(check bool)
      "ExternalTypedefs.h generated" true (Sys.file_exists ext_path)
  end

let test_uses_wire_ctx () =
  let s = Wire.Everparse.schema_of_struct simple_struct in
  Alcotest.(check bool)
    "schema_of_struct uses WireCtx" true
    (Wire.Everparse.uses_wire_ctx s);
  let raw =
    Wire.Everparse.Raw.of_module ~name:"TestSimple" ~module_:simple_module
      ~wire_size:3
  in
  Alcotest.(check bool)
    "raw module without extern WireCtx" false
    (Wire.Everparse.uses_wire_ctx raw)

let test_has_3d_exe () =
  (* Just check the function is callable and returns a bool *)
  let result = Wire_3d.has_3d_exe () in
  Alcotest.(check bool) "has_3d_exe returns bool" result result

let test_main_exists () =
  (* main dispatches on Sys.argv; calling it in tests would exit, so we only
     verify that the value exists and has the right type. *)
  let _ = (Wire_3d.main : package:string -> Wire.Everparse.t list -> unit) in
  ()

let suite =
  ( "wire_3d",
    [
      Alcotest.test_case "everparse_name" `Quick test_everparse_name;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "schema_of_struct" `Quick test_schema_of_struct;
      Alcotest.test_case "ensure_dir" `Quick test_ensure_dir;
      Alcotest.test_case "generate_c (needs 3d.exe)" `Quick test_generate_c;
      Alcotest.test_case "uses_wire_ctx" `Quick test_uses_wire_ctx;
      Alcotest.test_case "has_3d_exe" `Quick test_has_3d_exe;
      Alcotest.test_case "main exists" `Quick test_main_exists;
    ] )
