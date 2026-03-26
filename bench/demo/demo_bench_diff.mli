(** Differential testing of demo bench cases against EverParse C stubs. *)

type case = {
  id : Demo_bench_cases.id;
  label : string;
  packed : Wire_diff.packed_test;
}

val cases : case list
(** [cases] is the list of differential test cases for the demo benchmark. *)

val verify_of_id : Demo_bench_cases.id -> unit -> unit
(** [verify_of_id id ()] runs the differential verifier for the benchmark case
    identified by [id]. *)
