(** AFL fuzzer for the wire library: round-trip and crash-safety ({!Fuzz_wire})
    plus constraint validation ({!Fuzz_param}), both driven by the canonical
    {!Fuzz_gen} registry. *)

let () = Alcobar.run "wire" [ Fuzz_wire.suite; Fuzz_param.suite ]
