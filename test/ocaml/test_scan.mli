(** The fold tests: the fold of a generated module hands its [f] what the codec
    decodes, frame by frame. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the fold test suite. *)
