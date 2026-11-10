(** Tests of {!Wire_rust}. *)

val suite : string * unit Alcotest.test_case list
(** The generator's tests that need no Rust compiler: what it accepts and
    refuses, and how it names things. *)
