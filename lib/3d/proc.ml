let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let executable path =
  try
    Unix.access path [ Unix.X_OK ];
    not (Sys.is_directory path)
  with Unix.Unix_error _ | Sys_error _ -> false

type process_output = Inherit | Dev_null | File of string

let process_status_code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

let open_output = function
  | Inherit -> None
  | Dev_null -> Some (Unix.openfile Filename.null [ Unix.O_WRONLY ] 0o600)
  | File path ->
      Some
        (Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600)

(* Run [exe] without a shell, in the parent's directory: the child of
   [Unix.create_process] starts where its parent is, and the parent stays
   where it is, since moving it would move every other domain too. *)
let spawn ?(output = Inherit) exe args =
  let output_fd = open_output output in
  let out = Option.value output_fd ~default:Unix.stdout in
  let pid =
    Fun.protect
      ~finally:(fun () -> Option.iter Unix.close output_fd)
      (fun () ->
        Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin out out)
  in
  snd (Unix.waitpid [] pid)

(* Run [exe] without a shell, in [cwd]. Redirection is opened in the parent,
   then the child changes directory and receives each argument literally
   through [execv]. Needs [fork]. *)
let run_process ?(output = Inherit) ~cwd exe args =
  let output_fd = open_output output in
  match Unix.fork () with
  | 0 -> (
      try
        Unix.chdir cwd;
        Option.iter
          (fun fd ->
            Unix.dup2 fd Unix.stdout;
            Unix.dup2 fd Unix.stderr;
            Unix.close fd)
          output_fd;
        Unix.execv exe (Array.of_list (exe :: args))
      with Unix.Unix_error _ -> Unix._exit 127)
  | pid ->
      Option.iter Unix.close output_fd;
      snd (Unix.waitpid [] pid)

(* Run one pool job: [true] when it returns, [false] (with the message on
   stderr) when it raises, whatever it raises: a forked job that raises exits
   with 1, so a job run in this process must fail the same way. *)
let job_ok job =
  match match job () with () -> Ok () | exception exn -> Error exn with
  | Ok () -> true
  | Error (Failure msg | Invalid_argument msg | Sys_error msg) ->
      Fmt.epr "%s\n%!" msg;
      false
  | Error (Unix.Unix_error (e, fn, arg)) ->
      Fmt.epr "%s %s: %s\n%!" fn arg (Unix.error_message e);
      false
  | Error exn ->
      Fmt.epr "%s\n%!" (Printexc.to_string exn);
      false

(* Without [fork] (Windows) the jobs run one after the other in this process:
   the same verdicts, without the overlap. *)
let pool_in_process jobs = Array.map job_ok jobs

let forked_pool ~max_jobs jobs =
  let n = Array.length jobs in
  let ok = Array.make n false in
  let pid_idx = Hashtbl.create 64 in
  let next = ref 0 and running = ref 0 in
  let reap () =
    let pid, status = Unix.wait () in
    match Hashtbl.find_opt pid_idx pid with
    | Some i ->
        Hashtbl.remove pid_idx pid;
        decr running;
        ok.(i) <- (match status with Unix.WEXITED 0 -> true | _ -> false)
    | None -> ()
  in
  (* Drain any buffered output before forking, so a child does not inherit and
     re-flush the parent's pending bytes. *)
  Format.pp_print_flush Fmt.stderr ();
  Format.pp_print_flush Fmt.stdout ();
  while !next < n || !running > 0 do
    if !next < n && !running < max_jobs then begin
      let i = !next in
      incr next;
      match Unix.fork () with
      | 0 ->
          (* The child must never return into the parent's loop. On success
             [_exit 0] runs before [finally]; a failure reaches [finally] and
             the child exits with 1. *)
          Fun.protect
            ~finally:(fun () -> Unix._exit 1)
            (fun () -> if job_ok jobs.(i) then Unix._exit 0)
      | pid ->
          Hashtbl.add pid_idx pid i;
          incr running
    end
    else reap ()
  done;
  ok

(* A bounded fork pool: each job runs in its own process, so blocking EverParse
   runs overlap across cores with full isolation. Returns each
   job's success (exit 0 and no exception) in input order. EverParse runs must
   be isolated because they race on shared intermediate files in a shared
   directory, so jobs that invoke [3d.exe] should each use a private cwd. *)
let fork_pool ~max_jobs jobs =
  if Sys.win32 then pool_in_process jobs else forked_pool ~max_jobs jobs
