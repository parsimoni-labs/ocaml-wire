(** Checks of Wire API surfaces a value round-trip over one typ does not reach:
    construction guards, exact semantic examples, staged accessors, metadata and
    projection helpers. *)

val construction_guard_cases : string -> Alcobar.test_case list
(** [construction_guard_cases label] asserts invalid DSL constructions are
    rejected at construction time. *)

val semantic_invariant_cases : string -> Alcobar.test_case list
(** [semantic_invariant_cases label] checks decode/validate agreement and
    validate-then-get bounds over exact adversarial examples. *)

val api_cases : string -> Alcobar.test_case list
(** [api_cases label] exercises staged [Codec.get] / [set] accessors, slice
    navigation, batch bitfield extraction, custom sequence maps, struct
    validators, metadata helpers and Everparse / Raw projection helpers.
    File-writing helpers run once per process. *)
