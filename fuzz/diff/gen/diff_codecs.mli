(** The codecs the differential fuzzer compares against the EverParse-generated
    C validator: the in-scope part of the Boltzmann {!Fuzz_gen.sample}, not of
    {!Fuzz_gen.registry}. See {!included}. *)

(** Why a candidate is out of the differential's scope. *)
type exclusion =
  | Variable_size  (** no fixed wire size, so no standalone validator *)
  | Zero_size  (** unit codec, hits a setter arity mismatch *)
  | Parameterised  (** binds [Param.env], so has no standalone entrypoint *)
  | Casetype  (** projects an embedded sub-validator, not a standalone one *)

val string_of_exclusion : exclusion -> string
(** A short human-readable reason, for logs. *)

val included : (string * Fuzz_gen.packed) list
(** Sampled codecs the differential fuzzer covers: fixed-size, parameter-free
    projections of a single uniquely-named validator, in sample order. *)

val excluded : (string * Fuzz_gen.packed * exclusion) list
(** Sampled codecs skipped, each with the reason it is out of scope, so coverage
    stays explicit. *)

val summary : string
(** One-line tally: total candidates, included count, and excluded count broken
    down by reason. *)
