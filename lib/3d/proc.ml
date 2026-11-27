let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let executable path =
  try
    Unix.access path [ Unix.X_OK ];
    not (Sys.is_directory path)
  with Unix.Unix_error _ | Sys_error _ -> false

(* A Windows path is rooted by a separator or by a drive and a separator; the
   test is spelled out so it answers for Windows on any host. *)
let is_relative ~win32 path =
  if not win32 then Filename.is_relative path
  else
    let sep i = String.length path > i && (path.[i] = '/' || path.[i] = '\\') in
    not (sep 0 || (String.length path > 1 && path.[1] = ':' && sep 2))

let unquote dir =
  let n = String.length dir in
  if n >= 2 && dir.[0] = '"' && dir.[n - 1] = '"' then String.sub dir 1 (n - 2)
  else dir

let path_dirs ~win32 path =
  if win32 then
    String.split_on_char ';' path
    |> List.filter_map (fun dir ->
        match unquote dir with "" -> None | dir -> Some dir)
  else
    String.split_on_char ':' path
    |> List.map (fun dir -> if dir = "" then "." else dir)

(* The names of EverParse's 3D compiler: [everparse-3d.exe] since v2026.10,
   [3d.exe] before. *)
let names_3d_exe = [ "everparse-3d.exe"; "3d.exe" ]

let not_found_3d_exe =
  "EverParse's everparse-3d.exe (or 3d.exe) not found in PATH or \
   ~/.local/everparse/bin/"

let locate_3d_exe ~win32 ~getenv ~executable =
  let found dir =
    List.find_map
      (fun name ->
        let candidate = Filename.concat dir name in
        if executable candidate then Some candidate else None)
      names_3d_exe
  in
  let on_path dir =
    found
      (if is_relative ~win32 dir then Filename.concat (Sys.getcwd ()) dir
       else dir)
  in
  let in_home var =
    match getenv var with
    | None | Some "" -> None
    | Some home ->
        found
          (List.fold_left Filename.concat home [ ".local"; "everparse"; "bin" ])
  in
  match
    getenv "PATH" |> Option.to_list
    |> List.concat_map (path_dirs ~win32)
    |> List.find_map on_path
  with
  | Some _ as exe -> exe
  | None -> List.find_map in_home [ "HOME"; "USERPROFILE" ]

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

let rm_rf dir =
  (try Sys.readdir dir with Sys_error _ -> [||])
  |> Array.iter (fun f ->
      try Sys.remove (Filename.concat dir f) with Sys_error _ -> ());
  try Sys.rmdir dir with Sys_error _ -> ()

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error _ -> ""

let max_jobs () =
  match Sys.getenv_opt "WIRE_3D_JOBS" with
  | None | Some "" -> max 1 (min 4 (Domain.recommended_domain_count ()))
  | Some s -> (
      match int_of_string_opt s with
      | Some k when k >= 1 -> k
      | _ -> Fmt.failwith "WIRE_3D_JOBS=%S: expected a positive integer" s)

(* [true] when [job] returns, [false] with the message on stderr when it
   raises, whatever it raises. *)
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

let flush_output () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush_all ()

(* Run [f] with the standard output and error of this process, and so of
   every program it starts, going to [log], then put them back. *)
let with_output_to log f =
  flush_output ();
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let out = Unix.dup Unix.stdout and err = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  Fun.protect
    ~finally:(fun () ->
      flush_output ();
      Unix.dup2 out Unix.stdout;
      Unix.dup2 err Unix.stderr;
      Unix.close out;
      Unix.close err)
    f

(* One pool job: [job dir] in a fresh directory, removed afterwards, its
   output and any exception it raises written to [log]. *)
let logged_in_dir ~log job () =
  with_output_to log (fun () ->
      job_ok (fun () ->
          let dir = Filename.temp_dir "wire_3d_job" "" in
          Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> job dir)))

let in_process jobs = Array.map (fun job -> job ()) jobs

let forked ~max_jobs jobs =
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
  flush_output ();
  while !next < n || !running > 0 do
    if !next < n && !running < max_jobs then begin
      let i = !next in
      incr next;
      match Unix.fork () with
      | 0 ->
          (* The child must never return into the parent's loop: it exits 0
             when the job succeeds and 1 otherwise, whatever escapes. *)
          Fun.protect
            ~finally:(fun () -> Unix._exit 1)
            (fun () -> if jobs.(i) () then Unix._exit 0)
      | pid ->
          Hashtbl.add pid_idx pid i;
          incr running
    end
    else reap ()
  done;
  ok

let pool_with run jobs =
  let logs = Filename.temp_dir "wire_3d_logs" "" in
  Fun.protect
    ~finally:(fun () -> rm_rf logs)
    (fun () ->
      let log i = Filename.concat logs (string_of_int i ^ ".log") in
      let ok =
        run
          (Array.of_list
             (List.mapi (fun i (_, job) -> logged_in_dir ~log:(log i) job) jobs))
      in
      let results =
        List.mapi (fun i (name, _) -> (name, ok.(i), read_file (log i))) jobs
      in
      match List.filter (fun (_, ok, _) -> not ok) results with
      | [] -> Ok (List.map (fun (_, _, log) -> log) results)
      | failed ->
          Error
            (String.concat "\n"
               (List.map (fun (name, _, log) -> name ^ ":\n" ^ log) failed)))

(* Without [fork] (Windows) the jobs run one after the other in this process:
   the same verdicts and logs, without the overlap. *)
let pool jobs =
  if Sys.win32 then pool_with in_process jobs
  else pool_with (forked ~max_jobs:(max_jobs ())) jobs

let pool_in_process jobs = pool_with in_process jobs
