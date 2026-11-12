(** Processes for the EverParse pipeline.

    Running a program without a shell, and a bounded pool of forked jobs.
    Internal to {!Wire_3d}. *)

val absolute_path : string -> string
(** [absolute_path path] is [path], made absolute against the current directory
    when it is relative. *)

val executable : string -> bool
(** [executable path] is [true] when [path] is a file the process may execute.
*)

(** Where a child's standard output and error go. *)
type process_output = Inherit | Dev_null | File of string

val process_status_code : Unix.process_status -> int
(** [process_status_code s] is the exit code of [s], or [128] plus the signal
    number as a shell reports it. *)

val run_process :
  ?output:process_output ->
  cwd:string ->
  string ->
  string list ->
  Unix.process_status
(** [run_process ?output ~cwd exe args] runs [exe] with [args] in [cwd], without
    a shell, and waits for it. The directory changes in the child only. *)

val fork_pool : max_jobs:int -> (unit -> unit) array -> bool array
(** [fork_pool ~max_jobs jobs] runs each of [jobs] in a process of its own, at
    most [max_jobs] at once, and is each job's success, exit 0 and no exception,
    in input order. A job that raises fails alone: the others still run. *)

val run_spawned :
  ?output:process_output ->
  cwd:string ->
  string ->
  string list ->
  Unix.process_status
(** [run_spawned ?output ~cwd exe args] is {!run_process} where there is no
    [fork]: the parent moves to [cwd] for the spawn alone and moves back, under
    a lock. Other domains see the move while it lasts. *)

val pool_in_process : (unit -> unit) array -> bool array
(** [pool_in_process jobs] is {!fork_pool} where there is no [fork]: the jobs
    run one after the other in this process, with the same verdicts. *)
