(* Tests for wire_3d.ml: starting EverParse and the job pools. *)

open Wire
open Wire.Everparse.Raw

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let needs_3d_exe () = if not (Wire_3d.has_3d_exe ()) then Alcotest.skip ()
let read_file path = In_channel.with_open_text path In_channel.input_all

(* The fake EverParse below is a shell script, which only a POSIX system runs. *)
let needs_posix () = if Sys.win32 then Alcotest.skip ()

(* [with_fake_3d f] puts a fake [3d.exe] first on PATH, one that writes its cwd
   and then each argument on a line of [observed], in the directory [--odir]
   names or else in that cwd, and calls [f ~outdir ~exe ~observed] with an
   output directory whose name needs quoting. *)
let with_fake_3d f =
  needs_posix ();
  let outdir = Filename.temp_dir "wire 3d;out " "" in
  let bindir = Filename.temp_dir "wire 3d;bin " "" in
  let exe = Filename.concat bindir "3d.exe" in
  let observed = Filename.concat outdir "observed" in
  let old_path = Sys.getenv_opt "PATH" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" (Option.value ~default:"" old_path);
      Wire_3d.rm_rf outdir;
      Wire_3d.rm_rf bindir)
    (fun () ->
      Out_channel.with_open_text exe (fun oc ->
          output_string oc
            "#!/bin/sh\n\
             out=observed\n\
             if [ \"$2\" = --odir ]; then out=\"$3/observed\"; fi\n\
             { pwd -P; for arg do printf '%s\\n' \"$arg\"; done; } > \"$out\"\n");
      Unix.chmod exe 0o755;
      (* The fake must come first, but the rest of PATH stays: [printf] is not
         a builtin of every /bin/sh (OpenBSD's ksh runs /usr/bin/printf). *)
      Unix.putenv "PATH"
        (match old_path with
        | Some p when p <> "" -> bindir ^ ":" ^ p
        | _ -> bindir);
      f ~outdir ~exe ~observed)

let fake_schema = "schema name;literal.3d"

let test_parse_3d_uses_argv_and_cwd () =
  with_fake_3d (fun ~outdir ~exe:_ ~observed ->
      (match Wire_3d.parse_3d ~batch:true ~outdir fake_schema with
      | Ok () -> ()
      | Error e -> Alcotest.failf "fake EverParse failed: %s" e);
      Alcotest.(check (list string))
        "cwd and literal argv"
        [ Unix.realpath outdir; "--batch"; fake_schema ]
        (In_channel.with_open_text observed In_channel.input_lines))

(* Windows has no [fork]: there 3d.exe is spawned where the process is, with
   its output directory and its input by absolute path. *)
let spawn_fake ~exe outdir =
  let log = Filename.concat outdir "log" in
  let code =
    Wire_3d.Private.run_3d_spawned ~outdir ~log exe [ "--batch" ] fake_schema
  in
  ( code,
    In_channel.with_open_text
      (Filename.concat outdir "observed")
      In_channel.input_lines )

let spawned ~here outdir =
  ( 0,
    [
      Unix.realpath here;
      "--batch";
      "--odir";
      outdir;
      Filename.concat outdir fake_schema;
    ] )

let observation = Alcotest.(pair int (list string))

let test_run_3d_spawned () =
  with_fake_3d (fun ~outdir ~exe ~observed:_ ->
      let here = Sys.getcwd () in
      Alcotest.check observation "exit code, cwd and literal argv"
        (spawned ~here outdir) (spawn_fake ~exe outdir))

(* Two threads spawn at once while a third reads the directory. Each child
   reports the directory it started in, which is the parent's if the parent
   never moved. Threads, since [Unix.fork] refuses to run once a domain has
   been spawned and the tests after this one fork. *)
let test_spawn_keeps_cwd () =
  with_fake_3d (fun ~outdir ~exe ~observed:_ ->
      let here = Sys.getcwd () in
      let stop = Atomic.make false and moved = Atomic.make false in
      let watcher =
        Thread.create
          (fun () ->
            while not (Atomic.get stop) do
              if Sys.getcwd () <> here then Atomic.set moved true;
              Thread.yield ()
            done)
          ()
      in
      let spawns name =
        let outdir = Filename.concat outdir name in
        let runs = ref [] in
        Unix.mkdir outdir 0o700;
        let thread =
          Thread.create
            (fun () -> runs := List.init 5 (fun _ -> spawn_fake ~exe outdir))
            ()
        in
        (name, outdir, runs, thread)
      in
      let all = List.map spawns [ "a"; "b" ] in
      List.iter (fun (_, _, _, thread) -> Thread.join thread) all;
      Atomic.set stop true;
      Thread.join watcher;
      Alcotest.(check bool) "the parent's cwd moved" false (Atomic.get moved);
      Alcotest.(check string) "the parent's cwd" here (Sys.getcwd ());
      List.iter
        (fun (name, outdir, runs, _) ->
          Wire_3d.rm_rf outdir;
          Alcotest.(check int) (name ^ ": runs") 5 (List.length !runs);
          List.iter
            (Alcotest.check observation name (spawned ~here outdir))
            !runs)
        all)

(* The search for 3d.exe, with the environment and the file system given. *)
let locate ~win32 env exes =
  Wire_3d.Private.locate_3d_exe ~win32
    ~getenv:(fun var -> List.assoc_opt var env)
    ~executable:(fun path -> List.mem path exes)

let exe_in dir = Filename.concat dir "3d.exe"

let home_exe home =
  List.fold_left Filename.concat home [ ".local"; "everparse"; "bin"; "3d.exe" ]

let test_locate_3d_exe () =
  let check name expected found =
    Alcotest.(check (option string)) name expected found
  in
  let tools = {|C:\tools\everparse\bin|} in
  check "Windows: PATH splits on ';'"
    (Some (exe_in tools))
    (locate ~win32:true [ ("PATH", {|C:\Windows;|} ^ tools) ] [ exe_in tools ]);
  check "Windows: a quoted PATH entry"
    (Some (exe_in {|C:\Program Files\ep|}))
    (locate ~win32:true
       [ ("PATH", {|C:\Windows;"C:\Program Files\ep"|}) ]
       [ exe_in {|C:\Program Files\ep|} ]);
  check "Windows: USERPROFILE without HOME"
    (Some (home_exe {|C:\Users\me|}))
    (locate ~win32:true
       [ ("PATH", {|C:\Windows|}); ("USERPROFILE", {|C:\Users\me|}) ]
       [ home_exe {|C:\Users\me|} ]);
  check "POSIX: PATH splits on ':'"
    (Some (exe_in "/opt/a;b/bin"))
    (locate ~win32:false
       [ ("PATH", "/usr/bin:/opt/a;b/bin") ]
       [ exe_in "/opt/a;b/bin" ]);
  check "POSIX: an empty PATH entry is the current directory"
    (Some (Filename.concat (Sys.getcwd ()) (exe_in ".")))
    (locate ~win32:false
       [ ("PATH", "/usr/bin:") ]
       [ Filename.concat (Sys.getcwd ()) (exe_in ".") ]);
  check "PATH comes before HOME"
    (Some (exe_in "/opt/bin"))
    (locate ~win32:false
       [ ("PATH", "/opt/bin"); ("HOME", "/home/me") ]
       [ exe_in "/opt/bin"; home_exe "/home/me" ]);
  check "HOME"
    (Some (home_exe "/home/me"))
    (locate ~win32:false
       [ ("PATH", "/usr/bin"); ("HOME", "/home/me") ]
       [ home_exe "/home/me" ]);
  check "no PATH, HOME or USERPROFILE" None (locate ~win32:false [] []);
  check "nothing executable" None
    (locate ~win32:true
       [ ("PATH", tools); ("HOME", "/home/me"); ("USERPROFILE", "/home/me") ]
       [])

(* A job that raises fails alone, whatever it raises, in both pools. *)
let test_pools_agree () =
  let jobs =
    [|
      ignore;
      (fun () -> failwith "refused");
      ignore;
      (fun () -> raise Not_found);
      (fun () -> invalid_arg "bad");
      ignore;
    |]
  in
  let verdicts = [| true; false; true; false; false; true |] in
  Alcotest.(check (array bool))
    "in process" verdicts
    (Wire_3d.Private.pool_in_process jobs);
  Alcotest.(check (array bool))
    "forked" verdicts
    (Wire_3d.fork_pool ~max_jobs:2 jobs)

(* The real 3d.exe writes the same C spawned from here with [--odir] as it
   does started in its output directory. *)
let test_spawned_3d_same_c () =
  needs_3d_exe ();
  let exe =
    Wire_3d.Private.locate_3d_exe ~win32:Sys.win32 ~getenv:Sys.getenv_opt
      ~executable:Sys.file_exists
    |> Option.get
  in
  let s = Wire.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
  let generated run =
    let outdir = Filename.temp_dir "wire_3d_spawned" "" in
    Fun.protect
      ~finally:(fun () -> Wire_3d.rm_rf outdir)
      (fun () ->
        Wire_3d.generate_3d ~outdir [ s ];
        run ~outdir;
        List.map
          (fun file -> read_file (Filename.concat outdir file))
          [ "TestSimple.c"; "TestSimple.h"; "TestSimpleWrapper.c" ])
  in
  let in_outdir ~outdir =
    match Wire_3d.parse_3d ~batch:true ~outdir "TestSimple.3d" with
    | Ok () -> ()
    | Error e -> Alcotest.failf "3d.exe in its output directory: %s" e
  in
  let spawned ~outdir =
    let log = Filename.concat outdir "log" in
    Alcotest.(check int)
      "exit code" 0
      (Wire_3d.Private.run_3d_spawned ~outdir ~log exe [ "--batch" ]
         "TestSimple.3d")
  in
  Alcotest.(check (list string))
    "generated C" (generated in_outdir) (generated spawned)

let suite =
  ( "wire_3d_proc",
    [
      Alcotest.test_case "parse_3d uses argv and cwd" `Quick
        test_parse_3d_uses_argv_and_cwd;
      Alcotest.test_case "run_3d_spawned uses argv, not cwd" `Quick
        test_run_3d_spawned;
      Alcotest.test_case "two spawns leave the parent's cwd alone" `Quick
        test_spawn_keeps_cwd;
      Alcotest.test_case "locate 3d.exe" `Quick test_locate_3d_exe;
      Alcotest.test_case "pools give the same verdicts" `Quick test_pools_agree;
      Alcotest.test_case "spawned 3d.exe writes the same C (needs 3d.exe)"
        `Quick test_spawned_3d_same_c;
    ] )
