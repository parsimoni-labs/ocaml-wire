(** Tests of {!Wire_runtime.Vec}: each lane operation against the same operation
    on four [int]s, and the loads and stores against the bytes they cover. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the tests of {!Wire_runtime.Vec}. *)
