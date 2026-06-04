(** The registry subset the differential fuzzer compares against the
    EverParse-generated C validator. See {!included}. *)

(** Why a candidate is out of the differential's scope. *)
type exclusion =
  | Variable_size  (** no fixed wire size, so no standalone validator *)
  | Zero_size  (** unit codec, hits a setter arity mismatch *)
  | Parameterised  (** binds [Param.env], so has no standalone entrypoint *)
  | Casetype  (** projects an embedded sub-validator, not a standalone one *)
  | Name_clash of string list
      (** would redefine the named shared types of an already-included codec *)

val string_of_exclusion : exclusion -> string
(** A short human-readable reason, for logs. *)

val included : (string * Fuzz_gen.packed) list
(** Registry codecs the differential fuzzer covers: fixed-size, parameter-free
    projections of a single uniquely-named validator, in registry order. *)

val excluded : (string * Fuzz_gen.packed * exclusion) list
(** Registry codecs skipped, each with the reason it is out of scope, so
    coverage stays explicit. *)

val summary : string
(** One-line tally: total candidates, included count, and excluded count broken
    down by reason. *)
