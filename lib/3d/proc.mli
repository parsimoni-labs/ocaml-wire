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
    in input order. *)
