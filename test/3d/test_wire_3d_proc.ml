(* Tests for wire_3d.ml: starting EverParse and the job pools. *)

open Wire
open Wire.Private.Everparse.Raw

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let read_file path = In_channel.with_open_text path In_channel.input_all

(* The fake EverParse below is a shell script, which only a POSIX system runs. *)
let needs_posix () = if Sys.win32 then Alcotest.skip ()

(* [with_fake_3d f] puts a fake [everparse-3d.exe] first on PATH, one that reports no
   version, so it is run with no flag of Wire's own, and otherwise writes its cwd
   and then each argument on a line of [observed], in the directory [--odir]
   names or else in that cwd, and calls [f ~outdir ~exe ~observed] with an
   output directory whose name needs quoting. *)
let with_fake_3d f =
  needs_posix ();
  let outdir = Filename.temp_dir "wire 3d;out " "" in
  let bindir = Filename.temp_dir "wire 3d;bin " "" in
  let exe = Filename.concat bindir "everparse-3d.exe" in
  let observed = Filename.concat outdir "observed" in
  let old_path = Sys.getenv_opt "PATH" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" (Option.value ~default:"" old_path);
      Wire_3d.Private.rm_rf outdir;
      Wire_3d.Private.rm_rf bindir)
    (fun () ->
      Out_channel.with_open_text exe (fun oc ->
          output_string oc
            "#!/bin/sh\n\
             if [ \"$1\" = --version ]; then exit 1; fi\n\
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
          Wire_3d.Private.rm_rf outdir;
          Alcotest.(check int) (name ^ ": runs") 5 (List.length !runs);
          List.iter
            (Alcotest.check observation name (spawned ~here outdir))
            !runs)
        all)

(* The search for everparse-3d.exe, with the environment and the file system
   given. *)
let locate ~win32 env exes =
  Wire_3d.Private.locate_3d_exe ~win32
    ~getenv:(fun var -> List.assoc_opt var env)
    ~executable:(fun path -> List.mem path exes)

let exe_in dir = Filename.concat dir "everparse-3d.exe"

let home_exe home =
  List.fold_left Filename.concat home
    [ ".local"; "everparse"; "bin"; "everparse-3d.exe" ]

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
  check "an earlier PATH directory wins"
    (Some (exe_in "/usr/bin"))
    (locate ~win32:false
       [ ("PATH", "/usr/bin:/opt/bin") ]
       [ exe_in "/usr/bin"; exe_in "/opt/bin" ]);
  check "nothing executable" None
    (locate ~win32:true
       [ ("PATH", tools); ("HOME", "/home/me"); ("USERPROFILE", "/home/me") ]
       [])

(* In a real directory on PATH, everparse-3d.exe is found and 3d.exe, the name
   of EverParse's 3D compiler before v2026.10, is not. *)
let test_locate_3d_exe_names () =
  let dir = Filename.temp_dir "wire_3d_names" "" in
  Fun.protect
    ~finally:(fun () -> Wire_3d.Private.rm_rf dir)
    (fun () ->
      let touch name =
        let path = Filename.concat dir name in
        Out_channel.with_open_bin path (fun oc ->
            output_string oc "#!/bin/sh\n");
        Unix.chmod path 0o755;
        path
      in
      let locate () =
        Wire_3d.Private.locate_3d_exe ~win32:Sys.win32
          ~getenv:(function "PATH" -> Some dir | _ -> None)
          ~executable:Sys.file_exists
      in
      let old = touch "3d.exe" in
      Alcotest.(check (option string)) "3d.exe alone" None (locate ());
      let current = touch "everparse-3d.exe" in
      Alcotest.(check (option string))
        "everparse-3d.exe next to 3d.exe" (Some current) (locate ());
      Sys.remove old;
      Alcotest.(check (option string))
        "everparse-3d.exe alone" (Some current) (locate ()))

(* A job that raises fails alone, whatever it raises, in both pools, and the
   failure names it with its output and the message of what it raised. Each
   job runs in a directory of its own, empty when it starts and gone when it
   ends, and its output is its log. *)
let test_pools_agree () =
  let failing =
    [
      ("out", fun _ -> print_string "out");
      ("refused", fun _ -> failwith "refused");
      ("not found", fun _ -> raise Not_found);
      ( "bad",
        fun _ ->
          Fmt.epr "err@.";
          invalid_arg "bad" );
      ("quiet", ignore);
    ]
  in
  let in_own_dir dir =
    if Sys.readdir dir <> [||] then failwith "the directory is not empty";
    Out_channel.with_open_bin (Filename.concat dir "f") ignore;
    print_string dir
  in
  let check name pool =
    Alcotest.(check (result (list string) string))
      (name ^ ": failures")
      (Error "refused:\nrefused\n\nnot found:\nNot_found\n\nbad:\nerr\nbad\n")
      (pool failing);
    match pool [ ("a", in_own_dir); ("b", in_own_dir) ] with
    | Error e -> Alcotest.failf "%s: %s" name e
    | Ok logs ->
        Alcotest.(check int) (name ^ ": logs") 2 (List.length logs);
        List.iter
          (fun dir ->
            Alcotest.(check bool)
              (name ^ ": directory removed")
              false (Sys.file_exists dir))
          logs
  in
  check "in process" Wire_3d.Private.pool_in_process;
  check "forked" Wire_3d.Private.pool

(* [WIRE_3D_JOBS] sets how many jobs run at once, and a value that is not a
   positive integer is refused rather than replaced. *)
let test_max_jobs () =
  let old = Sys.getenv_opt "WIRE_3D_JOBS" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "WIRE_3D_JOBS" (Option.value ~default:"" old))
    (fun () ->
      Unix.putenv "WIRE_3D_JOBS" "1";
      Alcotest.(check int) "WIRE_3D_JOBS=1" 1 (Wire_3d.Private.max_jobs ());
      Unix.putenv "WIRE_3D_JOBS" "7";
      Alcotest.(check int) "WIRE_3D_JOBS=7" 7 (Wire_3d.Private.max_jobs ());
      Unix.putenv "WIRE_3D_JOBS" "";
      Alcotest.(check bool)
        "unset: at most 4" true
        (Wire_3d.Private.max_jobs () >= 1 && Wire_3d.Private.max_jobs () <= 4);
      List.iter
        (fun v ->
          Unix.putenv "WIRE_3D_JOBS" v;
          match Wire_3d.Private.max_jobs () with
          | n -> Alcotest.failf "WIRE_3D_JOBS=%S: got %d" v n
          | exception Failure _ -> ())
        [ "0"; "-2"; "four" ])

(* The real 3d.exe writes the same C spawned from here with [--odir] as it
   does started in its output directory. *)
let test_spawned_3d_same_c () =
  Gate.needs_3d_exe ();
  let exe =
    Wire_3d.Private.locate_3d_exe ~win32:Sys.win32 ~getenv:Sys.getenv_opt
      ~executable:Sys.file_exists
    |> Option.get
  in
  let s = Wire.Private.Everparse.Raw.project_struct ~mode:`Ffi simple_struct in
  let generated run =
    let outdir = Filename.temp_dir "wire_3d_spawned" "" in
    Fun.protect
      ~finally:(fun () -> Wire_3d.Private.rm_rf outdir)
      (fun () ->
        Wire_3d.Private.generate_3d ~outdir [ s ];
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

(* Only a release that extracts through Custard gets the larger budget, so
   v2026.08.21 runs F* as it always did. *)
let test_has_custard () =
  List.iter
    (fun (version, want) ->
      Alcotest.(check bool) version want (Wire_3d.Private.has_custard version))
    [
      ("EverParse/3d v2026.10.27", true);
      ("EverParse/3d v2027.01.05", true);
      ("EverParse/3d v2026.08.21", false);
      ("EverParse/3d v2025.12.01", false);
      ("EverParse/3d", false);
      ("", false);
    ]

let suite =
  ( "wire_3d_proc",
    [
      Alcotest.test_case "parse_3d uses argv and cwd" `Quick
        test_parse_3d_uses_argv_and_cwd;
      Alcotest.test_case "run_3d_spawned uses argv, not cwd" `Quick
        test_run_3d_spawned;
      Alcotest.test_case "two spawns leave the parent's cwd alone" `Quick
        test_spawn_keeps_cwd;
      Alcotest.test_case "locate everparse-3d.exe" `Quick test_locate_3d_exe;
      Alcotest.test_case "locate the 3D compiler by its one name" `Quick
        test_locate_3d_exe_names;
      Alcotest.test_case "pools give the same results" `Quick test_pools_agree;
      Alcotest.test_case "WIRE_3D_JOBS" `Quick test_max_jobs;
      Alcotest.test_case "a larger Custard budget from v2026.10" `Quick
        test_has_custard;
      Alcotest.test_case "spawned 3d.exe writes the same C (needs 3d.exe)"
        `Quick test_spawned_3d_same_c;
    ] )
