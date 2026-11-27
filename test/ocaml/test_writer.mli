(** Tests for the one module {!Wire_ocaml} writes for a codec, against its own
    parts and the codec. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the test suite. *)
