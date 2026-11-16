(** Tests of {!Wire_runtime.Target}: each load against the [Bytes] one, and in
    the paranoid profile that bad offsets raise. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the tests of {!Wire_runtime.Target}. *)
