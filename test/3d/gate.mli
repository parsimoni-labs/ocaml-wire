(** The gate of the cases that run EverParse's 3D compiler. *)

val needs_3d_exe : unit -> unit
(** [needs_3d_exe ()] skips the current case unless [BUILD_EVERPARSE=1], and
    fails it when the variable is set but {!Wire_3d.locate_3d_exe} finds no
    [3d.exe]. *)
