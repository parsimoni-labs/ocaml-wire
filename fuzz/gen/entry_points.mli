(** The alternate entry points ([of_string], [of_bytes], [of_reader],
    [to_string], [to_bytes], [to_writer], the [_exn] variants and [validate])
    over the whole registry. *)

val entry_point_cases : string -> Alcobar.test_case list
(** [entry_point_cases label] round-trips the alternate entry points over a
    registry gen picked per sample, and requires the direct decoder not to crash
    and to agree with {!Wire.Codec.decode} on random and adversarial bytes.
    Direct checks skip codecs that bind a [Param.env]. *)
