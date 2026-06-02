(** Fuzz tests for parameterised descriptions and constraint validation, driven
    entirely by {!Fuzz_gen}.

    Every codec in the canonical {!Fuzz_gen.registry} is run through
    {!Fuzz_gen.validate_cases}: a positive must pass {!Wire.Codec.validate}
    (threading any [Param.env] the codec binds), and random / adversarial inputs
    may pass or fail but must not crash. This exercises the param-binding,
    [where]-clause, and field-constraint paths across every combinator. There
    are no hand-written cases: the registry is the single source. *)

open Fuzz_gen

let suite =
  ( "param",
    List.concat_map (fun (name, Pack g) -> validate_cases name g) registry )
