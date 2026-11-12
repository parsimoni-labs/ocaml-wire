(** Alcobar cases built from a gen's streams and the properties in
    {!Properties}. *)

open Gen

val test_cases : ?validate:bool -> string -> 'a t -> Alcobar.test_case list
(** [test_cases label g] batches [g]'s four streams into one case: positives
    check round-trip, size and validation, random and adversarial bytes check
    crash-safety, hostile values check encode safety, and every stream is also
    checked field by field, at an offset, for framing and for read purity.
    [validate] defaults to [true]. *)

val sized_cases : string -> Alcobar.test_case list
(** [sized_cases group] round-trips a length/data codec whose data span is sized
    by a cross-field reference, for several length-source field types. *)

val nested_cases : string -> int -> Alcobar.test_case list
(** [nested_cases label depth] runs the checks of {!test_cases} over an
    arbitrary nested codec per sample, composed up to [depth] levels. *)

val validate_agrees_cases : string -> int -> Alcobar.test_case list
(** [validate_agrees_cases label depth] requires {!Wire.Codec.validate} to raise
    exactly when {!Wire.Codec.decode} returns [Error], with the same
    [error_kind], over an arbitrary nested codec per sample. *)

val direct_cases : string -> 'a t -> Alcobar.test_case list
(** [direct_cases label g] round-trips [g] through {!Wire.of_string} and
    {!Wire.to_string}. Skipped when [g] needs a [Param.env]. *)

val streaming_cases : string -> 'a t -> Alcobar.test_case list
(** [streaming_cases label g] round-trips [g] through {!Wire.of_reader} and
    {!Wire.to_writer}. Skipped when [g] needs a [Param.env]. *)

val validate_cases : string -> 'a t -> Alcobar.test_case list
(** [validate_cases label g] requires validate to pass positives and not to
    crash on random and adversarial inputs. *)

val reject_cases : string -> 'a t -> Alcobar.test_case list
(** [reject_cases label g] requires decode to reject every random and
    adversarial input. *)
