(** Tests of {!Wire_runtime.Target}: each load at an [int64_u] position against
    the [Bytes] one, and in the paranoid profile that bad positions raise. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the tests of {!Wire_runtime.Target}. *)
