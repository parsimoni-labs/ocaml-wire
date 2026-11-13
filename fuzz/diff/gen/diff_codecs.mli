(** The codecs the differential fuzzer compares against the EverParse-generated
    C validator: the in-scope part of the Boltzmann {!Fuzz_gen.sample}, not of
    {!Fuzz_gen.registry}. See {!included}. *)

(** Why a candidate is out of the differential's scope. *)
type exclusion =
  | Name_clash
      (** two field names spell one identifier the FFI pipeline generates (a 3D
          name, a plug macro or member, a stub label), so it refuses them *)
  | Variable_size  (** no fixed wire size, so no standalone validator *)
  | Zero_size  (** unit codec, hits a setter arity mismatch *)
  | Parameterised  (** binds [Param.env], so has no standalone entrypoint *)
  | Casetype  (** projects an embedded sub-validator, not a standalone one *)

val string_of_exclusion : exclusion -> string
(** A short human-readable reason, for logs. *)

val sample_seed : int
(** [sample_seed] is the seed of the sample the codecs come from. *)

val sample_count : int
(** [sample_count] is the size of that sample. *)

val candidates : (string * Fuzz_gen.packed) list
(** [candidates] is the sample {!included} and {!excluded} partition, in sample
    order. *)

val scope_exclusion : Fuzz_gen.packed -> exclusion option
(** [scope_exclusion p] is [None] when {!included} takes [p], and the reason it
    does not otherwise. *)

val in_scope : Fuzz_gen.packed -> bool
(** [in_scope p] is [true] when [p] passes the filter {!included} applies. *)

val included : (string * Fuzz_gen.packed) list
(** Sampled codecs the differential fuzzer covers: fixed-size, parameter-free
    projections of a single uniquely-named validator, in sample order. *)

val excluded : (string * Fuzz_gen.packed * exclusion) list
(** Sampled codecs skipped, each with the reason it is out of scope, so coverage
    stays explicit. *)

type compared_field = { name : string; width : int }
(** A named field the differential compares by value, with its byte width on the
    wire. *)

val compared_fields : Fuzz_gen.packed -> compared_field list
(** [compared_fields p] is the fields of [p] whose decoded value both sides can
    hand back, in declaration order: the named whole-byte integer fields, minus
    the ones the default plug reports as a byte offset rather than a value. A
    bitfield, a float and a byte span are all left out; see the implementation
    for why each is. *)

val summary : string
(** One-line tally: total candidates, included count, and excluded count broken
    down by reason. *)

(** A structured codec the scanners are compared on: its label, the codec,
    [project], the scanner outputs a decoded value gives (an absent optional
    field's are left out, and read as 0), [inputs], the buffers a fuzzed input
    is tried as, and [corpus], its boundary cases. *)
type scan_case =
  | Scan : {
      label : string;
      codec : 'a Wire.Codec.t;
      project : 'a -> (string * int) list;
      inputs : bytes -> bytes list;
      corpus : unit -> bytes list;
    }
      -> scan_case

val scan_cases : scan_case list
(** The structured cases: regions, optional fields, tags, bitfield tests and the
    {!Packet_stack} codec. *)
