(* prove.exe, the process side of proofs/fstar-gen/prove.sh:

     prove.exe 3d EVERPARSE            print EverParse's 3D compiler in
                                       EVERPARSE/bin, found as Wire_3d finds it
     prove.exe run SECONDS LABEL CMD   run CMD in a process group of its own
     prove.exe prune ROOT USED...      remove from ROOT every entry no USED
                                       file lists

   [run] prints LABEL, CMD's wall and user time and the peak resident memory
   of the children (F* and the Z3 it runs) on stderr, and exits with CMD's
   code. After SECONDS the group is killed and the run fails with "timeout"
   and code 124.

   [prune] reads the USED files, one path relative to ROOT per line, keeps
   each listed entry and the directories on the way to it, and removes the
   rest of ROOT. *)

external children_maxrss : unit -> float = "wire_fstar_children_maxrss"

let executable path =
  try
    Unix.access path [ Unix.X_OK ];
    not (Sys.is_directory path)
  with Unix.Unix_error _ | Sys_error _ -> false

let three_d everparse =
  let bin = Filename.concat everparse "bin" in
  let getenv = function "PATH" -> Some bin | _ -> None in
  match Wire_3d.Private.locate_3d_exe ~win32:false ~getenv ~executable with
  | Some exe -> print_endline exe
  | None ->
      Fmt.epr "prove.exe: no 3D compiler in %s@." bin;
      exit 1

let code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n

let rec wait pid =
  try snd (Unix.waitpid [] pid) with Unix.Unix_error (EINTR, _, _) -> wait pid

let run limit label cmd =
  let start = Unix.gettimeofday () in
  let pid =
    match Unix.fork () with
    | 0 -> (
        try
          ignore (Unix.setsid ());
          Unix.execvp (List.hd cmd) (Array.of_list cmd)
        with Unix.Unix_error (e, _, _) ->
          Fmt.epr "%s: %s@." (List.hd cmd) (Unix.error_message e);
          Unix._exit 127)
    | pid -> pid
  in
  let timed_out = ref false in
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle
       (fun _ ->
         timed_out := true;
         try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ()));
  ignore
    (Unix.setitimer Unix.ITIMER_REAL
       { Unix.it_interval = 0.; it_value = limit });
  let status = wait pid in
  ignore
    (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value = 0. });
  let wall = Unix.gettimeofday () -. start in
  let rc, status =
    if !timed_out then (124, "timeout")
    else
      let rc = code status in
      (rc, Fmt.str "rc %d" rc)
  in
  Fmt.epr "%s: wall %.1fs user %.1fs maxrss %.0fMB %s@." label wall
    (Unix.times ()).tms_cutime (children_maxrss ()) status;
  exit rc

let rec remove path =
  match (Unix.lstat path).st_kind with
  | S_DIR ->
      Array.iter (fun e -> remove (Filename.concat path e)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let read_lines file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.filter (fun l -> l <> "")

let prune root used =
  let keep = List.concat_map read_lines used in
  let under dir p = String.starts_with ~prefix:(dir ^ "/") p in
  let rec walk rel =
    let path = if rel = "" then root else Filename.concat root rel in
    Array.iter
      (fun e ->
        let rel = if rel = "" then e else rel ^ "/" ^ e in
        if List.mem rel keep then ()
        else if List.exists (under rel) keep then walk rel
        else remove (Filename.concat root rel))
      (Sys.readdir path)
  in
  if Sys.file_exists root then walk ""

let () =
  match Array.to_list Sys.argv with
  | [ _; "3d"; everparse ] -> three_d everparse
  | _ :: "run" :: limit :: label :: (_ :: _ as cmd) ->
      run (float_of_string limit) label cmd
  | _ :: "prune" :: root :: used -> prune root used
  | _ ->
      Fmt.epr
        "usage: prove.exe 3d EVERPARSE | prove.exe run SECONDS LABEL CMD... | \
         prove.exe prune ROOT USED...@.";
      exit 2
