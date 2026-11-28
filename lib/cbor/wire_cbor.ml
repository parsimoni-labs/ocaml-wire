(* The pipeline docs/cbor.md designs: run evercddl, EverParse's CDDL front
   end, on the .cddl modules of Wire.Cbor. *)

let tool () = Wire_3d.tool "everparse-cddl.exe"

let not_found =
  "EverParse's everparse-cddl.exe not found in PATH or ~/.local/everparse/bin/"

(* What F* prints after each module it verifies, and evercddl once it has
   compiled the C (Main.ml:283-425). *)
let discharged = "All verification conditions discharged"
let succeeded = "EverCDDL succeeded!"

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

(* evercddl finds F*, KaRaMeL, Z3 and the verified libraries in the prefix it
   is installed in; the postlude, which defines int, tstr, any and the other
   prelude names, is one more input it is given (src/cose/Makefile:13-16). *)
let postlude exe =
  let home =
    match Sys.getenv_opt "EVERPARSE_HOME" with
    | Some h when h <> "" -> h
    | _ -> Filename.dirname (Filename.dirname exe)
  in
  List.fold_left Filename.concat home
    [ "lib"; "everparse"; "cddl"; "spec"; "postlude.cddl" ]

let copy_file ~src ~dst =
  let contents = In_channel.with_open_bin src In_channel.input_all in
  Out_channel.with_open_bin dst (fun oc ->
      Out_channel.output_string oc contents)

(* Run [exe] with [args], both output streams to [log], without a shell.
   Returns the exit code. *)
let spawn ~log exe args =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let pid =
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin fd fd)
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n

(* [path] and everything under it. *)
let rec remove path =
  if Sys.is_directory path then begin
    Array.iter (fun f -> remove (Filename.concat path f)) (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

(* The subdirectories evercddl writes in [dir], its --tmpdir and internal/:
   the pool removes a flat directory only, and a job directory it cannot
   remove keeps its name from the next jobs. *)
let remove_subdirs dir =
  Array.iter
    (fun f ->
      let p = Filename.concat dir f in
      if Sys.is_directory p then remove p)
    (Sys.readdir dir)

(* One schema in a {!Wire_3d.pool} job, in [dir], a directory of its own,
   with no --admit: F* checks every proof of the module. The job copies the C
   into [outdir], and the tool's output, its verdict last, as [<name>.log],
   which outlives a failed run. *)
let run_job ~exe ~postlude ~outdir s dir =
  let cddl = Wire.Cbor.filename s in
  let file = Filename.concat dir cddl in
  copy_file ~src:(Filename.concat outdir cddl) ~dst:file;
  let tmp = Filename.concat dir "tmp" in
  Wire_3d.ensure_dir tmp;
  let log = Filename.concat dir "everparse-cddl.log" in
  let start = Unix.gettimeofday () in
  let m = Wire.Cbor.name s in
  let code =
    spawn ~log exe
      [ "--mname"; m; "--odir"; dir; "--tmpdir"; tmp; file; postlude ]
  in
  let output = In_channel.with_open_bin log In_channel.input_all in
  let verdict =
    if code <> 0 then Fmt.str "refused with code %d" code
    else if not (contains ~sub:discharged output) then
      Fmt.str "not verified: no %S" discharged
    else if not (contains ~sub:succeeded output) then
      Fmt.str "not compiled: no %S" succeeded
    else Fmt.str "verified in %.0fs" (Unix.gettimeofday () -. start)
  in
  let summary = Fmt.str "%s: %s" m verdict in
  Out_channel.with_open_bin
    (Filename.concat outdir (m ^ ".log"))
    (fun oc -> Out_channel.output_string oc (output ^ summary ^ "\n"));
  print_string output;
  Fmt.pr "%s@." summary;
  if not (String.starts_with ~prefix:"verified" verdict) then
    Fmt.failwith "everparse-cddl on %s: %s" cddl verdict;
  List.iter
    (fun ext ->
      copy_file
        ~src:(Filename.concat dir (m ^ ext))
        ~dst:(Filename.concat outdir (m ^ ext)))
    [ ".c"; ".h" ]

let job ~exe ~postlude ~outdir s dir =
  Fun.protect
    ~finally:(fun () -> remove_subdirs dir)
    (fun () -> run_job ~exe ~postlude ~outdir s dir)

let run_everparse ?(quiet = true) ~outdir ss =
  let exe = match tool () with Some e -> e | None -> failwith not_found in
  let postlude = postlude exe in
  let outdir =
    if Filename.is_relative outdir then Filename.concat (Sys.getcwd ()) outdir
    else outdir
  in
  match
    Wire_3d.pool
      (List.map (fun s -> (Wire.Cbor.name s, job ~exe ~postlude ~outdir s)) ss)
  with
  | Ok logs -> if not quiet then List.iter print_string logs
  | Error failed -> failwith failed

let run ?quiet ~outdir ss =
  Wire.Cbor.write ~outdir ss;
  run_everparse ?quiet ~outdir ss
