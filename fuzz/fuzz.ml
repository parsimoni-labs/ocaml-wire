(** AFL fuzzer for the wire library: round-trip, validation, and crash-safety,
    driven by the canonical {!Fuzz_gen} registry. *)

let () = Alcobar.run "wire" [ Fuzz_wire.suite; Fuzz_param.suite ]
