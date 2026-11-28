(** The alternate entry points ([Codec.to_string], [Codec.to_bytes], a codec
    embedded as a field, and [validate]) over the whole registry. *)

val entry_point_cases : string -> Alcobar.test_case list
(** [entry_point_cases label] round-trips the alternate entry points over a
    registry gen picked per sample, and requires a record codec embedded as the
    one field of a record to agree with {!Wire.Codec.decode} on random and
    adversarial bytes. Skips codecs that bind a [Param.env]. *)
