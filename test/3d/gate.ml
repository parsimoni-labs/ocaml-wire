(* A case that runs EverParse's 3D compiler takes minutes, so it runs only with
   [BUILD_EVERPARSE=1], as every other EverParse suite does, and skips
   otherwise: reporting it as [OK] would print a green tick for an assertion
   that never ran. With the variable set, a missing [3d.exe] is a failure. *)
let needs_3d_exe () =
  if Sys.getenv_opt "BUILD_EVERPARSE" <> Some "1" then Alcotest.skip ()
  else if Option.is_none (Wire_3d.locate_3d_exe ()) then
    Alcotest.fail
      "BUILD_EVERPARSE=1 but no 3d.exe in PATH or ~/.local/everparse/bin"
