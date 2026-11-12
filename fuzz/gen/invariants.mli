(** An audit of the shared fuzzer vocabulary: the registry, the composition
    leaves and the sampler still cover what they are meant to. *)

val assert_contains : string -> string list -> string list -> unit
(** [assert_contains group labels expected] fails unless every label of
    [expected] is in [labels]. *)

val const_case : string -> (unit -> unit) -> Alcobar.test_case
(** [const_case name check] is a case that runs [check] on every sample. *)

val invariant_cases : string -> Alcobar.test_case list
(** [invariant_cases label] asserts registry labels are unique, the registry
    mirrors the Wire API families, the composition leaves include the expected
    weird and adversarial terms, the sampler is reproducible, unique and well
    spread, and the per-field accessor properties reach most of the registry. *)
