(** Processes for the EverParse pipeline.

    Running a program without a shell, and a bounded pool of isolated jobs.
    Internal to {!Wire_3d}. *)

val absolute_path : string -> string
(** [absolute_path path] is [path], made absolute against the current directory
    when it is relative. *)

val executable : string -> bool
(** [executable path] is [true] when [path] is a file the process may execute.
*)

val locate :
  name:string ->
  win32:bool ->
  getenv:(string -> string option) ->
  executable:(string -> bool) ->
  string option
(** [locate ~name ~win32 ~getenv ~executable] is the first file [name] that
    [executable] accepts: in a directory of [PATH], split on [';'] when [win32]
    and on [':'] otherwise, then in [.local/everparse/bin] under [HOME] and
    under [USERPROFILE]. A variable [getenv] does not know is skipped. *)

val locate_3d_exe :
  win32:bool ->
  getenv:(string -> string option) ->
  executable:(string -> bool) ->
  string option
(** [locate_3d_exe] is {!locate} of [everparse-3d.exe]. *)

val not_found_3d_exe : string
(** [not_found_3d_exe] is the message of a failure to find EverParse's 3D
    compiler. *)

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
    a shell, and waits for it. The directory changes in the child only. Needs
    [Unix.fork]. *)

val spawn :
  ?output:process_output -> string -> string list -> Unix.process_status
(** [spawn ?output exe args] runs [exe] with [args] in the current directory,
    without a shell and without [Unix.fork], and waits for it. *)

val rm_rf : string -> unit
(** [rm_rf dir] removes [dir] and the files in it, ignoring errors. *)

val max_jobs : unit -> int
(** [max_jobs ()] is how many jobs {!pool} runs at once: [WIRE_3D_JOBS] when it
    is set, else the smaller of 4 and the number of cores. Raises [Failure] when
    [WIRE_3D_JOBS] is not a positive integer. *)

val pool : (string * (string -> unit)) list -> (string list, string) result
(** [pool jobs] runs each named job of [jobs] in a process of its own and a
    fresh directory of its own, at most {!max_jobs} at once. It is [Ok] of the
    jobs' logs when every job returned, else [Error] naming each job that did
    not, with its log. *)

val pool_in_process :
  (string * (string -> unit)) list -> (string list, string) result
(** [pool_in_process jobs] is {!pool} where there is no [fork]: the jobs run one
    after the other in this process, with the same results. *)
